# Campaign Manager Automation

Source archive for campaign master preparation, customer segment generation, and campaign filtering, prepared for public publication at the owner's request. This repository contains sanitized copies from Server 93 plus the supplied process document. It does not deploy, schedule, or execute anything automatically.

## Repository map

| Directory | Purpose |
| --- | --- |
| `source/campaign-apis/` | Master preparation, schema generation, related admin/data APIs and referenced helpers |
| `source/clm-generators/` | Existing R/Python segment generators, SQL, launchers, configuration and referenced helpers |
| `source/digital-filter-data/` | Filter API scripts and hosts |
| `source/campaign-segment-builder/` | Existing local instructions for the reviewed segment workflow |
| `docs/reference/` | Original Campaign Master process document |
| `docs/OPERATIONS.md` | Execution order, approvals and environment requirements |
| `docs/KNOWN-GAPS.md` | Unverified dependencies and observed implementation issues |
| `dependencies/` | Dependency inventories derived from the copied source |
| `inventory/source-manifest.json` | Original paths, original hashes, copied hashes and sanitization flags |
| `inventory/exclusions.json` | Excluded paths and reasons |
| `tools/verify_snapshot.py` | Offline integrity and credential-pattern checks; never runs production code |

Project-internal paths and filenames are preserved. This is an archival baseline, not a portable or production-tested refactor. Original scripts still contain absolute server paths and side effects. Secrets in copied files are replaced with `REDACTED_REQUIRED`; configure secrets securely before any authorized execution.

## Start here

1. Read [operations](docs/OPERATIONS.md) and [known gaps](docs/KNOWN-GAPS.md).
2. Verify the snapshot with `python tools/verify_snapshot.py` using Python 3. No third-party packages are required for this check.
3. Review the original document in `docs/reference/`.
4. For a new segment: objective -> SQL draft -> user review -> approved validation -> reviewed Python script -> approved deployment -> daily schedule -> config and Sheet registration.
5. Treat master rebuilding and Server 53 publication as separate approved operations.

## Data and credential boundary

No customer datasets, Parquet files, active production ID lists, private credential files, virtual environments, compiled applications, caches, logs or output directories are included. Referenced input data stays on the servers. Notebook outputs and attachments are stripped. Unrelated messaging execution and non-campaign project code are excluded; a referenced notification helper may remain because existing segment scripts depend on it.

The SQL directory can contain table-changing SQL. Preserving a script does not authorize its execution. No GitHub Actions or automatic server deployment is configured.
