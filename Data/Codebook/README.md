# Survey codebook

`SWACHE_Hypertension_Codebook.csv` documents the survey instrument as fielded by Ipsos in the four fieldwork waves (ten countries). For every variable in each wave's response export it gives the question text and, for categorical variables, the response codes. It is metadata about the questionnaire and contains no respondent data.

Filter `Wave` or `Countries` to find the relevant questionnaire, then search `Variable` for a column name. Country-specific questions have distinct names; for example, `INCCA1`, `INCBE1` and `INCAT1` are income questions for Canada, Belgium and Austria.

The codebook retains the original question wording and codes. In the shared response files, `OE1`, `Q5b_Other`, `H8_9_Other` and `Municipality_FI` are blank to protect respondents. These are deliberately withheld values, not survey non-response. `Respondent_Serial` is a replacement ID, numbered within each country; combine it with `Country` to identify a response.

## Columns

| Column | Content |
|---|---|
| `Wave` | `Wave1_FirstFive`, `Wave2_NextThree`, `Wave3_US` or `Wave4_Japan` |
| `Countries` | Countries covered by that wave |
| `Variable` | Variable name as it appears in the response files in `Data/SurveyData/` |
| `Position` | Column position of the variable in that wave's vendor export |
| `Description` | Question text |
| `Coding` | Categorical variables only: `code=label` pairs, separated by semicolons, in the order the vendor listed them. Blank for continuous or open-ended fields (age, duration, weights, free text). |
| `Source` | Original Ipsos data-map file (provenance only; not required to run the analysis) |

## Waves

| Wave | Countries | Variables |
|---|---|---:|
| 1 | Austria, Belgium, Canada, Slovenia, United Kingdom | 129 |
| 2 | Greece, Portugal, Finland | 121 |
| 3 | United States | 125 |
| 4 | Japan | 111 |

Some coding schemes are lookup tables rather than categories; for example, `resp_age` lists every age in years. The Wave 1 `Country` scheme lists six countries because the vendor reused a shared label list; the Wave 1 data contain only the five fielded countries.

## Scope

The codebook describes the questionnaire variables only. The analysis variables built by `Scripts/Setup/` (for example `LogIncome`, `Certainty` and the health indicators) are defined in Table A1 of the supplementary information, which is produced by [script 22](../../Scripts/Tables/22_Hypertension_TableA1_VariableCoding.R). The [saved Table A1](../../Output/Tables/Hypertension_TableA1_VariableCoding.txt) includes their definitions and summary statistics.
