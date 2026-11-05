# Survey data: files, preparation and screening

The replication starts from the files in [SurveyData/](SurveyData/): all 12,182 completed responses, before the paper's screening rules are applied. The original Ipsos extracts are not needed to run the analysis. Their names and checksums are recorded below for provenance.

In the response and timer files, the original vendor respondent identifiers are replaced by serial numbers assigned within each country (1 to N). Free-text answers (`OE1`, `Q5b_Other`, `H8_9_Other`) and Finnish municipality (`Municipality_FI`) are withheld to protect respondents. Their columns remain, but their values are blank. No paper result uses these fields. Reuse is covered by the [data licence](LICENSE.md).

## Analysis inputs

`Data/SurveyData/` contains one pair of files per country: `SWACHE_Hypertension_<Country>_Responses.xlsx` (questionnaire responses) and `SWACHE_Hypertension_<Country>_Timers.csv` (time spent on each questionnaire page, in the same row order as the responses). The countries are Austria, Belgium, Canada, Finland, Greece, Japan, Portugal, Slovenia, United_Kingdom and United_States.

Match files by country, then match `Respondent_Serial` in the response file to `Respondent_serial` in the timer file (`Respondent_Serial` for Japan). Serial numbers repeat across countries. Timer columns record page timings, not answers; for example, a timing for a withheld question can still be present. Variable definitions are in the [codebook](Codebook/README.md).

## From completed responses to the analysis sample

| Step | Removed | Remaining |
|---|---:|---:|
| Completed responses | | 12,182 |
| Whole-survey speeders | 365 | 11,817 |
| Valuation-section speeders | 635 | 11,182 |
| Failed risk-comprehension test | 1,686 | 9,496 |

Missing household income is imputed before screening, so no respondent is excluded for missing income. The screening is deterministic; the data-preparation scripts use no random numbers.

## Source extracts

These checksums identify the original Ipsos extracts, not the shared country files. Sizes are in bytes.

### Wave 1: Austria, Belgium, Canada, Slovenia, United Kingdom (6,111 respondents)

| File | Bytes | SHA-256 |
|---|---:|---|
| `22-029424-01_SWACHE_Hypertension_Main All Countries_client.xlsx` | 3,488,191 | `11642d3f6081a175d35d26bf6e2125f9feafbfef5ab56f8aba5521ce17020b93` |
| `22-029424-01_SWACHE_Hypertension_Main All Countries_Questiontimers.csv` | 1,469,581 | `72971b3c32d0525c4bb69925d7c59f643751b9ab27063edb52eee80ddcc4ebfd` |

### Wave 2: Greece, Portugal, Finland (3,641 respondents)

| File | Bytes | SHA-256 |
|---|---:|---|
| `SWACHE_July2024Latest/22-029424-01_SWACHE_Hypertension_Main_EL_PT_FI_client.xlsx` | 1,973,243 | `f0798cbfac1356d2b95a05f40028449838ed51053a73d718a7156a8a4bcb2345` |
| `SWACHE_July2024Latest/22-029424-01_SWACHE_Hypertension_Main_EL_PT_FI_Questiontimers.csv` | 694,153 | `9fa939534157093e9121751120192eaf28409c2d737712e1f1c1c787a33e6ee4` |

### Wave 3: United States (1,211 respondents)

The United States files contain the 1,211 United States respondents in the September 2024 extract; records for Greece, Portugal and Finland in that extract are excluded. An earlier, superseded extract contains 1,220 United States records and is not used.

| File | Bytes | SHA-256 |
|---|---:|---|
| `SWACHE_Hypertension_Main_EL_PT_FI_US_client_NewSep2024/22-029424-01_SWACHE_Hypertension_Main_EL_PT_FI_US_client.xlsx` | 2,744,207 | `8f2e04e10199b0cd8cfda0c8fc2246fffab00332d46f5b48916aa43b445a7265` |
| `SWACHE_Hypertension_Main_EL_PT_FI_US_client_NewSep2024/22-029424-01_SWACHE_Hypertension_Main_EL_PT_FI_US_Questiontimers.csv` | 937,513 | `c09664c5f4d89e97445d15b5286aba1ae9069fd09e4e47d727b8180a9267fdfc` |

### Wave 4: Japan (1,219 respondents)

| File | Bytes | SHA-256 |
|---|---:|---|
| `24-081509-01_NIES Japan_Hypertension_Main/24-081509-01_NIES Japan_Hypertension_Main_Client.xlsx` | 609,672 | `b79b135a997fc78f671ebee258d55394bf2b0aaaf5a9d32e1013132073eae80b` |
| `24-081509-01_NIES Japan_Hypertension_Main/24-081509-01_NIES Japan_Hypertension_Main_QuestionTimers.csv` | 209,364 | `61b29444a57019bef9d7b551004e461f73742caf54baf066cacb782f611ba078` |
