from query_loader import *
import datetime
import pandas as pd
import numpy as np
import datetime
import os

pd.set_option('display.max_columns', None)

# commenting the below query as it is too heavy for clickhouse to run

# q1 = load_query('./queries/1.regenerate_annual_txn_base.sql')
# execute_query(q1)
# q2 = load_query('./queries/2.regenerate_last_two_fy_txn.sql')
# execute_query(q2)

# joining_query = """
# CREATE OR REPLACE TABLE temp_db.an_transaction_data_2126
# ENGINE = MergeTree
# ORDER BY CUSTOMER_MOBILE AS
# select * from temp_db.an_segmentation_base_2122
# union all
# select * from temp_db.an_segmentation_base_2223
# union all
# select * from temp_db.an_segmentation_base_2324
# union all
# select * from temp_db.an_segmentation_base_2425
# union all
# select * from temp_db.an_segmentation_base_2526
# """
# execute_query(joining_query)

# q3 = load_query('./queries/3.regenerate_last_four_FY_txn.sql')
# execute_query(q3)
# q4 = load_query('./queries/4.clustering_customer_visit_summary_2326.sql')
# execute_query(q4)

# ## TWO YEARS ROLLING TXN FOR CURRENT CUSTOMERS
# rolling_txn = """
# CREATE OR REPLACE TABLE temp_db.an_clustering_model_customer_list_2326_two_years_rolling_txns
# ENGINE = MergeTree
# ORDER BY CUSTOMER_MOBILE AS
# SELECT a.*,
# b.BILLDATE,
# b.BILLNO,
# b.CUSTOMER_NAME,
# b.DIVISION,
# b.SECTION,
# b.DEPARTMENT,
# b.COMBINED_CATEGORY,
# b.GROSSAMT,
# b.MRPAMT,
# b.NETAMT,
# b.QTY,
# b.DISCOUNTAMT,
# b.PROMOAMT,
# b.ICODE
# FROM temp_db.an_clustering_model_customer_list_2326 a
# LEFT JOIN temp_db.an_transaction_data_2126_with_combined_category b
#  ON a.CUSTOMER_MOBILE = b.CUSTOMER_MOBILE
#    AND date(b.BILLDATE) BETWEEN addYears(date(a.LAST_VISITED_ON), -2) AND date(a.LAST_VISITED_ON);

# """
# execute_query(rolling_txn)
# q5 = load_query('./queries/5.customer_category_features_with_flags_2326.sql')
# execute_query(q5)
# # --------------------------------------------------------------------
# # -- Step 1: Aggregate Super-Category Revenue for each Customer
# # --------------------------------------------------------------------
# agg_1 = """
# CREATE OR REPLACE TABLE temp_db.an_customer_super_category_aggregates_2326
# ENGINE = MergeTree
# ORDER BY CUSTOMER_MOBILE AS
# SELECT
#    CUSTOMER_MOBILE,
#    -- Sum revenue for each super-category (based on the combined category mapping)
#    SUM(CASE WHEN COMBINED_CATEGORY IN ('Men', 'Women', 'Women_Ethnic', 'Women_Western') THEN NETAMT ELSE 0 END) AS Adult_REV_SUM,
#    SUM(CASE WHEN COMBINED_CATEGORY IN ('Boys_Junior', 'Boys_Senior', 'Girls_Junior', 'Girls_Senior', 'Toddler', 'Infant','Toys', 'Boys', 'Girls') THEN NETAMT ELSE 0 END) AS Kids_Toys_REV_SUM, -- THIS IS WHERE THE COLUMN IS DEFINED
#    SUM(CASE WHEN COMBINED_CATEGORY IN ('FMCG_Food', 'FMCG_Non_Food', 'Staples', 'Home_Mart') THEN NETAMT ELSE 0 END) AS Household_Home_FMCG_Staples_REV_SUM,
#    SUM(CASE WHEN COMBINED_CATEGORY IN ('Others') THEN NETAMT ELSE 0 END) AS Others_REV_SUM,
#    SUM(NETAMT) AS Total_Revenue_2Year -- THIS IS ALSO DEFINED HERE
# FROM temp_db.an_clustering_model_customer_list_2326_two_years_rolling_txns -- Make sure this source table exists and is populated
# GROUP BY CUSTOMER_MOBILE;
# """

# execute_query(agg_1)
# q6 = load_query('./queries/Q6.sql')
# execute_query(q6)
# q7= load_query('./queries/Q7.sql')
# execute_query(q7)
# q8=load_query('./queries/Q8_V2.sql')
# execute_query(q8)


# final_persona = """
# CREATE OR REPLACE TABLE temp_db.an_customer_personas_intuitive_v3
# ENGINE = MergeTree
# ORDER BY CUSTOMER_MOBILE AS
# select a.*,b.STORE_NAME,b.REGION,b.ZONE from
# temp_db.an_customer_personas_intuitive_v2 a
# LEFT JOIN
# (SELECT a.*,b.STORE_NAME,b.REGION,b.ZONE FROM customers.rfm_till_date  a
# left join
# vmart_sales.stores b
# on toString(a.PREFERRED_STORE) = toString(b.CODE)) b
# on a.CUSTOMER_MOBILE = b.CUSTOMER_MOBILE

# """
# execute_query(final_persona)



## generating segment customer_value_tier
query = f"""select CUSTOMER_MOBILE as Customer_Mobile,Customer_Value_Tier as seg_customer_value_tier from temp_db.an_customer_personas_intuitive_v3;"""
df = get_ch_client().query_df(query)
# df["seg_customer_value_tier"]= np.where(df["seg_customer_value_tier"]=="High Value","high_value",
#          np.where(df["seg_customer_value_tier"]=="Low Value","low_value",
#                   np.where(df["seg_customer_value_tier"]=="Mid Value","mid_value","very_low_value")))
df.to_parquet('D:/database/seg/seg_customer_value_tier/seg_customer_value_tier.parquet',engine='fastparquet')

## generating segment inferred_household_structure
query = f"""select CUSTOMER_MOBILE as Customer_Mobile,Inferred_Household_Structure as seg_inferred_household_structure from temp_db.an_customer_personas_intuitive_v3;"""
df = get_ch_client().query_df(query)
df.to_parquet(f'D:/database/seg/seg_inferred_household_structure/seg_inferred_household_structure.parquet',engine='fastparquet')

