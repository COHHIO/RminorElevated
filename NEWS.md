## Competition Report

#### Updates 12-01-2025
- Updated "Increase Income (all sources)" report. Previously only showing clients that exited. Now showing
clients at the end of the reporting period or at program exit.

#### Updates 11-26-2025
- Released the 2025 Mahoning Continuum of Care (CoC) Competition Renewal Project Evaluation.

#### Updates 11-25-2025
- Balance of State CoC Competition reporting now included in the daily automatic updates. Previously the competition
report data was being updated manually.

#### Updates 11-21-2025
- Released the 2025 Balance of State Continuum of Care (CoC) Competition Renewal Project Evaluation.

## Prioritization Report

#### Updates 11-19-2025
- We changed how households in Street Outreach (SO) projects get added to the prioritization report. 
It's now factored on an household's most recent living situation. This ensures clients 
are prioritized based on current circumstances rather than current project enrollment alone.
- Most recent living situation is created for individuals in Coordinated Entry (CE) projects,
Services Only (SSO) projects, and Street Outreach projects. Previously we only looked at 'Previous Living Situation'.


## Data Quality Changes

#### Updates 11-11-2025

- Data Quality check effected: "Client with No Disability Receiving SSI/SSDI/Other Disability Based Income (could be ok)"
- Data Quality check is checking if a client has a disabling condition marked as anything 
that's not "Yes" and a disability based income. Previously we were only looking at SSI and SSDI.
- New DQ is also looking at "VA Disability Service" as well as "VA Disability Non-service"
- filtering out anyone 65 or older in the SSI check (65+ year olds are eligible for SSI without a disability)
- checking if disabling condition is "No", "Client Doesn't Know", "Client Prefers Not to Answer", or "Data Not Collected" (Previously only checking if condition was "No")

