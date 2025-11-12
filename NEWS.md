## Data Quality Changes

#### Updates 11-11-2025

- Data Quality check effected: "Client with No Disability Receiving SSI/SSDI/Other Disability Based Income (could be ok)"
- Data Quality check is checking if a client has a disabling condition marked as anything 
that's not "Yes" and a disability based income. Previously we were only looking at SSI and SSDI.
- New DQ is also looking at "VA Disability Service" as well as "VA Disability Non-service"
- filtering out anyone 65 or older in the SSI check (65+ year olds are eligible for SSI without a disability)
- checking if disabling condition is "No", "Client Doesn't Know", "Client Prefers Not to Answer", or "Data Not Collected" (Previously only checking if condition was "No")

