# Review and handoff checklist

Use only relevant items, with evidence rather than assumed success.

| Stage | Required review/evidence |
| --- | --- |
| Definition | Objective, identifier, brand/customer grain, dates/timezone, eligibility, mapping, null/tie/empty policy |
| SQL | Exact reviewed query, actual schema evidence, logic approval, separate execution authorization |
| Validation | Counts, duplicates, invalid identifiers, mapping coverage, date boundaries; no exposed customer list |
| Implementation | Reviewed SQL preserved, dependency interfaces verified, output schema, staging/replacement behavior, logs and failure exit |
| Deployment | Approved file list/diff and target, backup/rollback, deployed content verified |
| Scheduling | Daily frequency, exact time/timezone, account/runtime/working directory, no duplicate/overlap, enabled state and actual run evidence |
| Registration | Verified Sheet/tab/range, duplicate check, exact approved cell changes, config segment mapping, read-back of both |
| Completion | Output exists and is fresh, correct columns/grain, checks passed, remaining blocked or unverified steps clearly listed |

For approval, present the concrete change followed by the narrowly scoped question. Do not ask users to approve an undefined future action. Never conflate permission to read files with permission to run database queries or jobs.

Example final status wording:

"SQL reviewed; generation validated with observed counts; script deployed to the approved Server 93 path; schedule pending because remote Task Scheduler access was denied; Sheet/config registration verified. Segment setup is incomplete until scheduling is verified."

This is an example of status structure, not results to copy without evidence.
