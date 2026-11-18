---
name: create-campaign-segment
description: Create or update a Campaign Manager segment using Prachi's Server 93 workflow. Use for segment banana hai, segment creation, reviewed ClickHouse SQL, Python segment generators, daily scheduling, and campaign admin Google Sheet/config registration.
---

# Campaign segment creation

Use this workflow for Campaign Master segments in Prachi's environment. Read [environment.md](references/environment.md) first. Explain progress in Hinglish unless the user requests another language. Keep SQL and code identifiers exact.

## Authorization and scope

The user explicitly instructed: "DO NOT MAKE ANY CHANGES WITHOUT ASKING ME" and requires query review before implementation. Read-only file inspection and drafting in chat may proceed. Before executing any database query (including schema inspection), obtain execution permission unless already granted for that scope. Before saving segment implementation files, deployment, script execution, scheduler changes, configuration edits, or Sheet writes, show the exact proposed changes and obtain approval. A request to draft a segment does not approve production writes. Distinguish approval of SQL logic from permission to execute it.

Reuse approvals already given for the exact work; do not ask again unnecessarily. Record approvals in the conversation with their scope. If business logic, target, schedule, or proposed mutations materially change, obtain approval for that change. Never infer approval from elapsed time or silence. On a declined or missing approval, leave the dependent step pending and report its status accurately.

This plugin supplies a skill, not a database connection or scheduler service. Discover available host tools and existing approved connections. Do not invent endpoints, credentials, Sheet IDs, schemas, or successful runs. Never package or print credentials. Use connected Google Sheets tools when available and follow the corresponding skill. If an integration is unavailable, prepare the concrete draft and state what access is missing.

## 1. Define the segment

Recover relevant context and inspect existing code before asking for information already available. Establish objective, segment identifier, eligible customer population, brand scope, time window and timezone, inclusions/exclusions, mapping rules, output values, and handling of nulls, ties, duplicates, returns, and empty results. Ask only for unresolved business choices.

For a new segment use a simple snake_case identifier consistent with existing code, preferably seg_<meaning>. Match the Sheet identifier, config key, new folder basename, and Parquet attribute column exactly, including case. Preserve existing exceptions through explicit mappings rather than renaming them.

For state segments, distinguish customer residence from shopping-store state and latest transaction overall from latest transaction among mapped stores. Do not silently choose between the two existing state implementations.

## 2. Prepare ClickHouse SQL for review

Inspect relevant generator scripts and query helpers without executing them. Verify actual source tables, columns, joins, and date semantics through existing evidence and, when authorized, bounded read-only database inspection. Historical examples in the reference are starting points, not proof of the current schema.

Draft the query with an explanation of customer grain, eligibility, date boundaries, deduplication, joins, and segment values. Identify unresolved assumptions. Include a bounded validation plan for counts, duplicate Customer_Mobile values, null/invalid identifiers, mapping coverage, category distribution, and boundary cases. Keep customer samples masked by default.

Present the exact SQL and ask for query review. Do not run it or implement a changed version without the required approval. If no connection is available, label the SQL unexecuted and distinguish verified schema from assumptions.

## 3. Validate approved SQL

With execution permission, run scoped checks using the approved connection and bounded query settings. Avoid unrestricted full scans when a smaller check suffices. Compare counts and mapping coverage to the intended population. Verify one row per customer for scalar segments; explicitly agree another grain if needed. Report real results and failures. Revised business logic returns to query review.

## 4. Prepare Python generation

After SQL approval, prepare the exact implementation and file list for review; save only within an approved location and scope. Reuse existing connection/query helpers after inspecting their interfaces. Do not copy embedded secrets. Keep the reviewed SQL unchanged, or obtain review of a material revision. Resolve dates dynamically for daily runs; avoid freezing the creation date into the generator.

Target the existing Python segment directory on Server 93. The output contract is Customer_Mobile plus the agreed segment attribute, preserving identifier formatting and verified downstream types. Use the existing Parquet layout required by consumers.

Create output in a staging location that downstream readers cannot accidentally ingest. Validate schema, uniqueness, counts, and empty-output policy before replacing live output. Keep the previous valid output if generation or validation fails. Do not delete the live output folder before querying. Use a replacement method appropriate to the verified filesystem, and do not claim atomic multi-file publication without implementing it. Include failure exit codes, useful logs without customer records or secrets, resource cleanup, and protection from overlapping runs. Avoid adding unrelated infrastructure.

## 5. Deploy and schedule

Present script and SQL diffs, output location, launcher changes, dependencies, and rollback steps before production writes. Re-read target files before applying approved changes to preserve concurrent edits. Verify that the deployed files match the reviewed implementation.

Daily is the documented default frequency. Confirm exact time, timezone, runtime, working directory, execution account, dependencies, and overlap behavior. Inspect existing scheduler/launcher entries before adding anything to avoid duplicate execution. A BAT reference proves neither an enabled task nor a successful daily run.

Show the exact task or launcher change and obtain schedule authorization. Never create a local laptop schedule when the requested job belongs on Server 93. If Task Scheduler access is denied, leave scheduling pending with the concrete task definition and required access; do not mark it complete. Run an initial job only with execution permission and verify its exit status and output.

## 6. Register the segment

Resolve the current Campaign Management admin Sheet from the verified campaign config or document hyperlink. Inspect the live prepare_master headers and neighboring rows. Do not guess its remaining columns or confuse it with the separate store-mapping Sheet.

Prepare the exact new row/update using segment_name_id and is_include_master where those live headers are confirmed. TRUE means include in prepared campaign master. Check for an existing matching row before adding one. Separately prepare the config.yml entry under segment, mapping the same identifier to the Server 93 output folder. Show both before applying approved changes, preserve unrelated cells/config, and re-read to verify.

Validate the chain: Sheet identifier -> config key -> directory -> Parquet attribute. If only one registration change succeeds, report the partial state and propose recovery instead of claiming completion.

## 7. Verify and report

Use [review-checklist.md](references/review-checklist.md) for the final handoff. Report SQL review/validation, implementation/deployment, output contract and freshness, daily scheduler evidence, and Sheet/config registration separately as verified, failed, or pending. Provide actual counts/timestamps only when observed. A skill installation is not evidence that a segment exists.

Segment registration does not rebuild or publish campaign masters. Campaign master execution, schemas, Server 53 copying, active campaign IDs, PM2 operations, and frontend publication need a separate explicit request and authorization. Never execute these just because the source document lists them after segment creation.
