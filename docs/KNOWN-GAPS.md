# Known gaps and observations

Snapshot observations are not a claim of successful execution.

- Credentials were found in source code and configuration. Copies have placeholders; server originals were not edited. Rotation and environment-based secret injection remain separate work.
- Server 93 scheduler inspection returned Access is denied. Daily frequency, run identity and successful completion remain unverified.
- `C:/R - Projects/library.R` and associated project mappings were not available through the supplied shares. The literal-reference report also includes commented references; it is not a complete dependency graph.
- State segmentation has two implementations. The standalone script maps the latest transaction store via a ClickHouse mapping table. The combined script chooses the latest qualifying transaction among stores listed in a mapping Sheet, with a 2022-04-01 through yesterday window, then assigns Sheet attributes. This represents store-derived state, not verified residential state.
- Some existing generators remove output folders before successful generation. Copies preserve business code; running them may delete or overwrite server outputs.
- Master generation can skip missing or empty segments, append campaign registrations on rerun, and randomly reassign test/control customers.
- Master registration uses the run date for both start and end dates. A separate campaign-period parameter is needed for reliable monthly scheduling.
- Schema generation includes an S3 upload and string-based JSON serialization; confirm downstream compatibility before changing it.
- Input files, generated schemas, customer data, active production IDs, schedules, runtime binaries and credentials are not in this source repository. Definitions/data directories are recorded as exclusions.
- Google Sheet live headers and Server 53 active-ID path mapping still require verification.
- The automated source selection follows literal helper references and Python connection imports. Dynamically computed paths may require additional dependencies; no completeness claim is made for runtime execution.
