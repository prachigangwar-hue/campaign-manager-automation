# Campaign Manager environment

## Sources and confidence

Source document: `C:\Users\prachi.gangwar\Downloads\Campaign master whole process.docx`, titled "Campaign Master – End-to-End Process". Its text was inspected on 2026-11-17. It requires a new segment generator, daily frequency by default, admin Sheet inclusion, and a config mapping under `segment`.

User workflow and historical read-only findings were recovered from the Codex task "Check network path access", task ID `01a1e82a-e321-7f32-b2d4-692252c78356`, on 2026-11-17. The user explicitly requires query review and approval before changes. Server observations below are historical findings from that task, not a fresh connectivity, scheduler, or schema verification by this plugin.

## Paths

Server-local D: paths below belong to Server 93, not the user's laptop.

| Purpose | Server 93 local path | Network read/deployment path |
| --- | --- | --- |
| Segment generator root | `D:\products\clm_generators\auto\generate\segments` | `\\10.0.0.93\products\clm_generators\auto\generate\segments` |
| Python generators | `D:\products\clm_generators\auto\generate\segments\python_scripts` | `\\10.0.0.93\products\clm_generators\auto\generate\segments\python_scripts` |
| Segment output root | `D:\database\seg` | `\\10.0.0.93\database\seg` |
| Campaign config | `D:\digital-transformation\apis\campaign-apis\config.yml` | `\\10.0.0.93\digital-transformation\apis\campaign-apis\config.yml` |

Document example: `family_segment.R` is under the segment generator root. This does not change the user's request to implement future reviewed segment queries in Python.

## Historical Python examples

- `segment_state.py`: standalone state generator, latest transaction store, historically no explicit transaction date window, mapping from `temp_db.an_state_language`.
- `segment_language_state_low_performing_new.py`: state/language/low-performing-store generation, referenced by `activate_segments.bat`; historically latest qualifying purchase among mapping-Sheet stores between 2022-04-01 and yesterday. Uses a separate mapping Sheet and `temp_db.store_details_master`.
- Both state examples historically read `vmart_sales.dt_pos_transactional_data`. Verify current column names rather than inventing them.
- `connection.py`, `query_loader.py`, and `queries\`: connection/query patterns to inspect; do not print connection secrets.
- `activate_segment_updation.bat` launches `app.py`, which historically generated customer-value-tier and household-structure outputs from an existing persona table.
- State output: `D:\database\seg\seg_state\seg_state.parquet`, columns `Customer_Mobile` and `seg_state`.

The standalone state implementation reportedly deletes its existing output directory before generation. Do not copy that behavior into new generators.

## Admin registration

Document Sheet name: `cmng - admin panel - camp` (Campaign Management Admin Panel). Historical code inspection identified the `prepare_master` tab and `segment_name_id` / `is_include_master` fields. Live complete headers and exact ranges were not verified in the previous task; inspect them before writes. Discover the URL/ID from current config or source document; none is fabricated here.

New example config shape from the document:

```yaml
segment:
  seg_001: D:/database/seg/seg_001
```

The document requires matching segment name, folder and path. Historical config has an exception, `seg_family` mapped to `D:/database/seg/family`; preserve it and validate explicit mappings. New segments should follow the documented naming rule. Special characters in segment names can interfere with frontend filters.

## Open verification items

- Remote Task Scheduler inspection previously returned Access is denied. Actual daily run time, enabled status, task identity, and last successful run remain unverified.
- The previously observed state Parquet timestamp was 2026-10-22 17:14:40. It is historical evidence only and must not be used as current freshness.
- Confirm ClickHouse access, runtime dependencies, mapping sources, Sheet access, and output expectations for each new segment.

## Downstream scope boundary

The document also covers `Auto\schedule_campaign_master.R`, campaign master and schema output, Server 53 distribution, active campaign IDs, PM2 operations, and frontend verification. Those are campaign publication steps, outside this segment-creation skill unless explicitly requested separately. Do not treat documented PM2 numeric IDs as stable current service identities.
