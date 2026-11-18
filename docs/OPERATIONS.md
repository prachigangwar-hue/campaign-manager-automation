# Operations guide

## Approval rule

The user requires review of segment SQL and permission before server, database, scheduler or Google Sheet changes. Repository publication authorizes copying code; it does not authorize executing the copied scripts or deploying them back to a server.

## Server mappings

| Repository folder | Original Server 93 folder |
| --- | --- |
| `source/campaign-apis` | `D:\digital-transformation\apis\campaign-apis` |
| `source/clm-generators` | `D:\products\clm_generators` |
| `source/digital-filter-data` | `D:\products\digital_filter_data` |

The code also depends on `C:\R - Projects\library.R`, project ID mappings, existing credential provisioning, R/Python installations and production data. These external requirements are not supplied by the repository.

## Segment workflow

1. Define customer eligibility, date range, mapping source, exclusions, output column and permitted values.
2. Inspect the current ClickHouse schema and draft SQL. Obtain query review before execution.
3. Run authorized validation: row counts, unique customers, nulls, duplicates, distribution and boundary cases.
4. Prepare a Python generator under `auto/generate/segments/python_scripts`. Use the reviewed query and a stable output contract: `Customer_Mobile` plus the segment attribute.
5. Write and validate temporary output before replacing the prior good output. Existing scripts do not all implement this protection.
6. Review the exact file changes and deploy only after authorization.
7. Inspect the existing scheduler and add a daily job at an agreed time. The current task name, account, frequency and last successful run have not been verified.
8. Register the matching config key and output path in campaign `config.yml`; inspect live headers before adding a row to the admin Sheet's `prepare_master` tab. Existing code uses `segment_name_id` and `is_include_master`.
9. Verify the output and job result. Do not rebuild or publish the campaign master unless separately authorized.

## Existing entry points

| File | Observed role |
| --- | --- |
| `clm-generators/auto/generate/segments/python_scripts/activate_segments.bat` | Activates the server virtual environment and runs a list of generators |
| `clm-generators/auto/generate/segments/python_scripts/activate_segment_updation.bat` | Runs `app.py` |
| `clm-generators/auto/generate/segments/python_scripts/segment_language_state_low_performing_new.py` | Builds state/language/low-performing-store outputs using ClickHouse and a separate mapping Sheet |
| `campaign-apis/Auto/schedule_campaign_master.R` | Reads enabled segments from the admin Sheet and requests master generation |
| `campaign-apis/Auto/campaign_master_api.R` | Builds masters and registers campaigns |
| `campaign-apis/Auto/schema_generator_api.R` | Copies master files, uploads to S3 and generates schema metadata |
| `digital-filter-data/host/host_filter_api.R` | Existing filter API host; verify live PM2 mapping before use |

Paths in this table are relative to `source/`. Presence of a launcher does not establish that it is scheduled.

## Campaign publication order

Confirm input readiness -> generate required segments -> validate selected segment mappings -> start/verify required API services -> build VMART and Unlimited masters -> validate schemas and campaign dates -> backup current release and active IDs -> stage files on Server 53 -> verify copied files -> activate consistent release and IDs -> restart the correct filter service -> validate frontend/API -> restore temporary service state.

This order describes the intended controlled workflow. A complete orchestrator, rollback implementation and end-to-end tests have not yet been built. Do not use numeric PM2 IDs from the historical document without checking current service identities.

## Environment setup

- Python dependency versions are preserved in the original `requirement.txt`; the import inventory records additional imports, not a tested lockfile. `app.py` requests the `fastparquet` engine, which must be separately checked against the captured requirements.
- R package names are discovered from `library`, `require` and namespace references. Versions and custom package availability are unverified; `dreaweRTools` and the shared R bootstrap require the existing environment.
- Data inputs include mapped RFM/CPM/master/segment data, birthday and anniversary inputs, store tagging CSV, and production active campaign IDs. These files are intentionally not copied to Git.
- Provision secrets outside Git and replace placeholder usage through a reviewed change before running sanitized scripts. No `.env` integration is claimed for the archived code.
- Remote Task Scheduler access and the Server 53 active IDs share mapping remain unresolved.
