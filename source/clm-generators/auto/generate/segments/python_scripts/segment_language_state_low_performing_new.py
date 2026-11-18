"""
build_store_segment_mapping.py

1. Reads the GSheet fresh every run (dynamic - always pulls latest data).
2. Builds the list of stores from the GSheet and queries ClickHouse in BATCHES
   of stores (not all at once) -- a single query covering every GSheet store
   was overloading the ClickHouse server's memory (MEMORY_LIMIT_EXCEEDED)
   because of how many distinct customers had to be aggregated at once.
   Each batch also uses ONE combined argMax(tuple(...)) instead of three
   separate argMax() calls, which roughly cuts aggregation memory 3x.
3. Combines all batches and, for any customer who shopped at more than one
   GSheet store, keeps only their single most recent visit (by BILLDATE).
4. Inner-joins the result onto the GSheet on store name: GSheet 'store' <->
   ClickHouse 'latest_store'.
5. Writes out 3 parquet files, each with CUSTOMER_MOBILE + one attribute column:
     - seg_low_performing_store.parquet  -> CUSTOMER_MOBILE, seg_low_performing_store
     - seg_language.parquet              -> CUSTOMER_MOBILE, seg_language
     - seg_state.parquet                     -> CUSTOMER_MOBILE, seg_state

Requirements:
    pip install pandas clickhouse-connect pyarrow
"""

import os
import re
import time
import logging
import traceback

import pandas as pd
import clickhouse_connect

# ------------------------- CONFIG -------------------------

GSHEET_URL = "https://docs.google.com/spreadsheets/d/1uOXvRyHcTC4kv4xnWsTH17HDTzntfriQJcVt3yGA9qc/edit?gid=0#gid=0"

CH_HOST = 'chn1.vmart-tools.com'
CH_PORT = 8123
CH_USER = 'analyst_user'
CH_PASSWORD = 'REDACTED_REQUIRED'
CH_DATABASE = 'vmart_sales'
RETRY_COUNT = 3
RETRY_DELAY_SECONDS = 5

# Number of GSheet stores to query at once. Lower this (e.g. to 10-20) if you
# still see MEMORY_LIMIT_EXCEEDED from ClickHouse; raise it for fewer, faster
# round trips if the server has headroom.
BATCH_SIZE = 1000

# ---- HARD-CODED OUTPUT FOLDER ----
# Change this one line whenever you want the parquet files written elsewhere.
# Forward slashes work fine even on Windows, e.g. "C:/VmartData/customer_segments"
OUTPUT_DIR = "D:/database/seg"

# attribute_col (in GSheet) -> output parquet filename
OUTPUT_FILES = {
    'seg_low_performing_store': os.path.join(OUTPUT_DIR,'seg_low_performing_store', 'seg_low_performing_store.parquet'),
    'seg_language': os.path.join(OUTPUT_DIR,'seg_language', 'seg_language.parquet'),
    'seg_state': os.path.join(OUTPUT_DIR,'seg_state' ,'seg_state.parquet'),
}

CLICKHOUSE_QUERY_TEMPLATE = """
select CUSTOMER_MOBILE,
    argMax(tuple(STORE_ID, SHRTNAME, state), BILLDATE) AS latest,
    max(BILLDATE) AS latest_billdate
FROM vmart_sales.dt_pos_transactional_data a
GLOBAL JOIN temp_db.store_details_master b
    ON a.STORE_ID = b.store_code
WHERE BILLDATE BETWEEN toDate('2022-04-01') AND today() - 1
    AND CUSTOMER_MOBILE IS NOT NULL
    AND LENGTH(CUSTOMER_MOBILE) = 10
    AND REGEXP_INSTR(CUSTOMER_MOBILE, '^[6789][0-9]{{9}}$') > 0
    AND CUSTOMER_MOBILE NOT IN ('9999999999', '9999999990', '9999999993')
    AND SHRTNAME IN {store_filter}
GROUP BY CUSTOMER_MOBILE
"""

# ------------------------- GSHEET -------------------------

def extract_sheet_id_and_gid(url: str):
    sheet_id_match = re.search(r"/d/([a-zA-Z0-9-_]+)", url)
    gid_match = re.search(r"[?&#]gid=(\d+)", url)
    if not sheet_id_match:
        raise ValueError("Could not extract sheet ID from the GSheet URL")
    sheet_id = sheet_id_match.group(1)
    gid = gid_match.group(1) if gid_match else "0"
    return sheet_id, gid


def read_gsheet_as_df(url: str) -> pd.DataFrame:
    """Pulls the sheet as CSV directly from Google every call - always fresh.
    Requires the sheet to be shared as 'Anyone with the link can view'."""
    sheet_id, gid = extract_sheet_id_and_gid(url)
    csv_export_url = f"https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=csv&gid={gid}"
    df = pd.read_csv(csv_export_url)
    df.columns = [c.strip() for c in df.columns]
    return df


def build_sql_in_clause(values) -> str:
    """Safely builds a SQL '(...)' IN-clause from a list of store names,
    escaping any single quotes so store names can't break the query."""
    escaped = [str(v).strip().replace("'", "''") for v in values]
    return "(" + ", ".join(f"'{v}'" for v in escaped) + ")"


def chunk_list(values, size):
    for i in range(0, len(values), size):
        yield values[i:i + size]

# ------------------------- CLICKHOUSE -------------------------

def run_clickhouse_query_with_retry(query: str) -> pd.DataFrame:
    last_exception = None
    for attempt in range(1, RETRY_COUNT + 1):
        try:
            client = clickhouse_connect.get_client(
                host=CH_HOST,
                port=CH_PORT,
                username=CH_USER,
                password=CH_PASSWORD,
                database=CH_DATABASE,
            )
            result_df = client.query_df(query)
            return result_df
        except Exception as e:
            last_exception = e
            logging.warning(
                f"ClickHouse attempt {attempt}/{RETRY_COUNT} failed: "
                f"[{type(e).__name__}] {e!r}"
            )
            logging.warning(traceback.format_exc())
            if attempt < RETRY_COUNT:
                time.sleep(RETRY_DELAY_SECONDS)
    raise RuntimeError(f"ClickHouse query failed after {RETRY_COUNT} attempts") from last_exception


def run_query_in_batches(store_list) -> pd.DataFrame:
    """Queries ClickHouse in batches of BATCH_SIZE stores (to stay under the
    server's memory limit), unpacks the argMax tuple into separate columns,
    then -- for any customer who appears in more than one batch (i.e. shopped
    at more than one GSheet store) -- keeps only their single most recent
    visit across all batches, using latest_billdate."""
    batches = list(chunk_list(store_list, BATCH_SIZE))
    total_batches = len(batches)
    frames = []

    for i, batch in enumerate(batches, start=1):
        logging.info(f"ClickHouse batch {i}/{total_batches}: querying {len(batch)} stores...")
        batch_filter = build_sql_in_clause(batch)
        batch_query = CLICKHOUSE_QUERY_TEMPLATE.format(store_filter=batch_filter)
        batch_df = run_clickhouse_query_with_retry(batch_query)

        unpacked = pd.DataFrame(
            batch_df['latest'].tolist(),
            columns=['latest_store_id', 'latest_store', 'state'],
            index=batch_df.index,
        )
        batch_df = pd.concat([batch_df[['CUSTOMER_MOBILE', 'latest_billdate']], unpacked], axis=1)

        logging.info(f"  -> {batch_df.shape[0]} customer rows from this batch")
        frames.append(batch_df)

    combined = pd.concat(frames, ignore_index=True)
    before = combined.shape[0]

    # A customer shopping at multiple GSheet stores will have one row per
    # batch they matched in -- keep only their most recent visit.
    combined = combined.sort_values('latest_billdate', ascending=False)
    ch_df = combined.drop_duplicates(subset='CUSTOMER_MOBILE', keep='first').reset_index(drop=True)

    reconciled = before - ch_df.shape[0]
    if reconciled:
        logging.info(
            f"Combined {total_batches} batches: {before} rows -> {ch_df.shape[0]} unique customers "
            f"({reconciled} duplicate visits across batches resolved to most-recent)."
        )
    return ch_df

# ------------------------- JOIN -------------------------

def build_attribute_df(ch_df: pd.DataFrame, gsheet_df: pd.DataFrame, attribute_col: str) -> pd.DataFrame:
    """Inner-joins CH customers onto the GSheet (on store == latest_store) and
    returns just CUSTOMER_MOBILE + the requested attribute column."""
    gsheet_subset = (
        gsheet_df[['store', attribute_col]]
        .dropna(subset=['store'])
        .drop_duplicates(subset='store', keep='first')
    )
    dupes = gsheet_df['store'].duplicated().sum()
    if dupes:
        logging.warning(f"{dupes} duplicate 'store' rows found in GSheet; kept first occurrence only.")

    # Only pull CUSTOMER_MOBILE/latest_store from ch_df so there's no column
    # name collision with the GSheet attribute column (e.g. both have 'state').
    ch_minimal = ch_df[['CUSTOMER_MOBILE', 'latest_store']]

    merged = ch_minimal.merge(
        gsheet_subset,
        left_on='latest_store',
        right_on='store',
        how='inner'  # only keep stores present in the GSheet
    )

    result = merged[['CUSTOMER_MOBILE', attribute_col]].reset_index(drop=True)

    dropped = ch_minimal.shape[0] - result.shape[0]
    logging.info(
        f"[{attribute_col}] Kept {result.shape[0]} rows whose store is in the GSheet "
        f"(dropped {dropped} rows with no matching store)."
    )
    return result

# ------------------------- MAIN -------------------------

def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")

    logging.info("Reading GSheet...")
    gsheet_df = read_gsheet_as_df(GSHEET_URL)
    logging.info(f"GSheet loaded: {gsheet_df.shape[0]} rows | columns: {list(gsheet_df.columns)}")

    required_cols = ['store'] + list(OUTPUT_FILES.keys())
    missing = [c for c in required_cols if c not in gsheet_df.columns]
    if missing:
        raise ValueError(f"Missing expected column(s) in GSheet: {missing}")

    # Normalize store names (strip stray whitespace) so the SQL filter and
    # the later pandas join match cleanly.
    gsheet_df['store'] = gsheet_df['store'].apply(lambda x: x.strip() if isinstance(x, str) else x)

    store_list = sorted(gsheet_df['store'].dropna().unique().tolist())
    if not store_list:
        raise ValueError("No store names found in the GSheet 'store' column.")
    logging.info(f"{len(store_list)} distinct stores found in GSheet; querying ClickHouse in batches of {BATCH_SIZE}.")

    ch_df = run_query_in_batches(store_list)
    logging.info(f"ClickHouse (all batches combined) returned {ch_df.shape[0]} unique customers")

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    for attribute_col, output_path in OUTPUT_FILES.items():
        logging.info(f"Building '{attribute_col}' output...")
        attr_df = build_attribute_df(ch_df, gsheet_df, attribute_col)

        # ---- Preview before saving ----
        print(f"\n=== Preview: {output_path} ({attribute_col}) ===")
        print(attr_df.head(10).to_string(index=False))
        print(f"Shape: {attr_df.shape}")
        print(f"{attribute_col} value counts:")
        print(attr_df[attribute_col].value_counts(dropna=False))

        # Rename CUSTOMER_MOBILE to Customer_Mobile
        attr_df = attr_df.rename(columns={"CUSTOMER_MOBILE": "Customer_Mobile"})

        attr_df.to_parquet(output_path, index=False)
        logging.info(f"Saved {attr_df.shape[0]} rows to '{output_path}'")


if __name__ == "__main__":
    main()
