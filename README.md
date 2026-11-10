# The value of a statistical case of hypertension: replication package

![R](https://img.shields.io/badge/R-4.5.0-276DC3?logo=r)
![Platform](https://img.shields.io/badge/Tested_on-Windows_11-0078D4)
[![DOI](https://zenodo.org/badge/1447009716.svg)](https://doi.org/10.5281/zenodo.24116901)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--8550--466X-A6CE39?logo=orcid)](https://orcid.org/0000-0001-8550-466X)
[![GitHub release](https://img.shields.io/github/v/release/pmpk20/Hypertension_VSC)](https://github.com/pmpk20/Hypertension_VSC/releases)

------------------------------------------------------------------------

#### About:

- Data and R code for *The value of a statistical case of hypertension: Stated preference evidence from ten OECD countries*, including the supplementary information (Online Resource 1).
- Authors: Peter King (University of Leeds), Alistair Hunt (University of Bath) and Mike Holland (EMRC).
- Citation: see [CITATION.cff](CITATION.cff); please cite the paper and archive. Zenodo DOI to be added.

#### Acknowledgements:

The survey was conducted for the OECD SWACHE project and fielded online by Ipsos. OECD reference data provide purchasing power parities, GDP per capita and out-of-pocket health expenditure; hypertension prevalence comes from the WHO Global report on hypertension 2025.

#### Description:

The data support comparisons of willingness to pay across ten countries and alternative analyses of risk valuation, income effects and sample-screening rules.

- `Data/SurveyData/` contains 12,182 completed responses and page timings from ten countries (2023 to early 2025): Austria, Belgium, Canada, Finland, Greece, Japan, Portugal, Slovenia, the UK and the US. Screening leaves 9,496 respondents. Variable definitions and screening counts are in `Data/Codebook/` and `Data/RAW_MANIFEST.md`.
- Response files exclude names, contact details and IP addresses. Vendor IDs are replaced by within-country serial numbers: `Respondent_Serial` in response files and `Respondent_serial` in timer files (`Respondent_Serial` for Japan). Use the country and serial number together to identify a respondent. To protect respondents, the free-text columns (`OE1`, `Q5b_Other`, `H8_9_Other`) and Finnish municipality (`Municipality_FI`) are left blank; no reported result uses these fields. See the [data notes](Data/RAW_MANIFEST.md) for file matching and screening.
- Licences: code under the [MIT licence](LICENSE); data under [CC BY 4.0](Data/LICENSE.md). The survey data were collected for the OECD; please cite the OECD and the paper when reusing them.
- `Scripts/` prepares the data, fits the models and produces the paper's results. `Output/` includes fitted models, tables and figures. Most table files use `#` as the column separator; `Hypertension_Table_RobustnessChecks.txt` is a plain-text report. Published tables add labels and rounding.
- Monetary values are in 2023 US dollars at purchasing power parity. The value of a statistical case (VSC) converts annual willingness to pay (WTP) using ten years of payments, 3% continuous discounting and the offered risk reduction.

#### Requirements and quick start:

Use **R 4.5.0** (tested on Windows 11) and **Rtools45** on Windows for packages compiled from source. Package versions are recorded in `renv.lock`; macOS and Linux have not been tested.

Open `HypertensionPaper.Rproj` inside `Hypertension_VSC/`, start a fresh R session and run:

```r
source("00_Hypertension_Setup_ReplicatePaper.R", encoding = "UTF-8")
```

Or, in a terminal at the repository root:

```sh
Rscript --vanilla 00_Hypertension_Setup_ReplicatePaper.R
```

The master script installs the recorded packages (internet required), rebuilds the sample, estimates the models and writes the results. A rerun overwrites generated files. Allow about **29 minutes** with packages installed (Windows 11, Intel Core i5-1345U, 16 GB RAM), plus first-time package installation.

To save the console output, warnings and errors as a plain-text replication log, create `Output/Replication/` if necessary and run this command in a terminal at the repository root (use `R.exe` instead of `R` in Windows PowerShell):

```sh
R CMD BATCH --vanilla 00_Hypertension_Setup_ReplicatePaper.R Output/Replication/Replication_Run.log
```

The command runs the same complete analysis and overwrites the log. Check its exit status and the final `Replication checks passed` and `Replication completed` messages. Results written directly to files remain in `Output/Tables/` and `Output/Figures/`. The release includes a completed run log, software versions, input checksums and step timings in [`Output/Replication/`](Output/Replication/README.md).

For one result, find its script below and follow the prerequisites in its header. Steps 01-13 rebuild the sample; saved models in `Output/Models/` can be reused where the result script permits. Preparation also creates `Data/Intermediate/` and `Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv`, which are not tracked.

------------------------------------------------------------------------

#### Directory structure (replication files only):

Script numbers give execution order. Search for `Table1`, `Figure3`, etc. to find scripts and outputs; `A` denotes supplementary tables and figures. Comments beside outputs identify the producing script.

```text
Hypertension_VSC/
├── 00_Hypertension_Setup_ReplicatePaper.R                                               # run the full analysis
├── CITATION.cff                                                                         # citation metadata
├── LICENSE                                                                              # MIT licence for the code
├── Data/
│   ├── Codebook/                                                                        # survey questions and response codes
│   ├── LICENSE.md                                                                       # CC BY 4.0 licence for the data
│   ├── Other/                                                                           # OECD purchasing power, GDP per capita and health spending
│   ├── RAW_MANIFEST.md                                                                  # source files and screening counts
│   ├── Reference_Figures/
│   │   ├── Figure_1_Risk_Display.png                                                    # Figure1: supplied risk illustration
│   │   └── Figure_2_Valuation_Question.png                                              # Figure2: supplied valuation question
│   └── SurveyData/                                                                      # responses (*_Responses.xlsx) and timings (*_Timers.csv), ten countries
├── HypertensionPaper.Rproj                                                              # open in RStudio
├── Output/
│   ├── Figures/
│   │   ├── Hypertension_Figure3_CountryWillingnessToPayDistributions.png                # [output of 21]
│   │   ├── Hypertension_FigureA1_RiskGroupWillingnessToPay.png                          # [output of 29]
│   │   ├── Hypertension_FigureA2_BidAcceptance.pdf                                      # [output of 24] PDF version
│   │   └── Hypertension_FigureA2_BidAcceptance.png                                      # [output of 24]
│   ├── Models/
│   │   ├── Hypertension_Model_KrinskyRobbDraws.rds                                      # [output of 18]
│   │   ├── Hypertension_Model_PooledAndCountryModels.rds                                # [output of 14]
│   │   └── Hypertension_Model_WillingnessToPayDeterminants.rds                          # [output of 16]
│   └── Tables/
│       ├── Hypertension_Table1_CountryWillingnessToPayAndCaseValues.txt                 # [output of 19]
│       ├── Hypertension_Table2_WillingnessToPayDeterminants.txt                         # [output of 20]
│       ├── Hypertension_Table3_CrossCountryRegressions.txt                              # [output of 25]
│       ├── Hypertension_TableA1_VariableCoding.txt                                      # [output of 22]
│       ├── Hypertension_TableA2_BidAmounts.txt                                          # [output of 23]
│       ├── Hypertension_TableA3_AlternativeDistributions.txt                            # [output of 26]
│       ├── Hypertension_TableA4_ScreeningRobustness.txt                                 # [output of 27]
│       ├── Hypertension_TableA5_CountryModels.txt                                       # [output of 15]
│       ├── Hypertension_TableA6_PairwiseCountryComparisons.txt                          # [output of 28]
│       ├── Hypertension_TableA6_PairwiseCountryComparisonsLongFormat.txt                # [output of 28] Table A6 in long format
│       ├── Hypertension_TableA7_CountryInfluence.txt                                    # [output of 31]
│       ├── Hypertension_TableA8_CountryRobustness.txt                                   # [output of 32]
│       ├── Hypertension_Table_CrossCountryRegressionsCooksDistance.txt                  # [output of 25] influence in all six Table 3 models
│       ├── Hypertension_Table_DeterminantEstimates.txt                                  # [output of 17]
│       ├── Hypertension_Table_DiscountingSensitivity.txt                                # [output of 19] pooled VSC under continuous and discrete discounting
│       ├── Hypertension_Table_KrinskyRobbIntervals.txt                                  # [output of 18]
│       └── Hypertension_Table_RobustnessChecks.txt                                      # [output of 30]
├── Replication_Packages.txt                                                             # packages loaded directly
├── Scripts/
│   ├── Figures/
│   │   ├── 21_Hypertension_Figure3_CountryWillingnessToPayDistributions.R               # Figure 3: WTP by country, Specification 2
│   │   ├── 24_Hypertension_FigureA2_BidAcceptance.R                                     # Figure A2: bid-acceptance curves
│   │   └── 29_Hypertension_FigureA1_RiskGroupWillingnessToPay.R                         # Figure A1: WTP by health perception, Specification 3
│   ├── Models/
│   │   ├── 03_Hypertension_Model_DefineEstimators.R                                     # define model estimators
│   │   ├── 14_Hypertension_Model_PooledAndCountryModels.R                               # Specification 1: pooled and country models
│   │   ├── 16_Hypertension_Model_WillingnessToPayDeterminants.R                         # estimate Specifications 1-3
│   │   └── 30_Hypertension_Model_RobustnessChecks.R                                     # Online Resource 1: weights, age, diagnosis and missing-data robustness
│   ├── Replication_Expected_Outputs.txt                                                 # expected files for step 33
│   ├── Setup/
│   │   ├── 01_Hypertension_Setup_RestoreEnvironment.R                                   # restore packages from renv.lock
│   │   ├── 02_Hypertension_Setup_CheckEnvironment.R                                     # check installed package versions
│   │   ├── 04_Hypertension_Setup_SharedCalculations.R                                   # shared formulas, currency conversions and VSC calculations
│   │   ├── 05_Hypertension_Setup_CleanFirstFiveCountries.R                              # clean Austria, Belgium, Canada, Slovenia and UK
│   │   ├── 06_Hypertension_Setup_PurchasingPowerFirstFiveCountries.R                    # convert first five countries to purchasing-power basis
│   │   ├── 07_Hypertension_Setup_CleanNextThreeCountries.R                              # clean Greece, Portugal and Finland
│   │   ├── 08_Hypertension_Setup_MergeFirstFiveNextThree.R                              # merge first eight countries
│   │   ├── 09_Hypertension_Setup_CleanUnitedStates.R                                    # clean United States
│   │   ├── 10_Hypertension_Setup_MergeFirstEightPlusUnitedStates.R                      # add United States
│   │   ├── 11_Hypertension_Setup_CleanJapan.R                                           # clean Japan
│   │   ├── 12_Hypertension_Setup_MergeAllWithJapan.R                                    # add Japan
│   │   ├── 13_Hypertension_Setup_BuildAnalysisSample.R                                  # apply screening; save 9,496-respondent sample
│   │   └── 33_Hypertension_Setup_CheckReplication.R                                     # check sample, models and expected outputs
│   └── Tables/
│       ├── 15_Hypertension_TableA5_CountryModels.R                                      # Table A5: country models, Specification 1
│       ├── 17_Hypertension_Table_DeterminantEstimates.R                                 # coefficient estimates used in Table 2
│       ├── 18_Hypertension_Table_KrinskyRobbIntervals.R                                 # 10,000 Krinsky-Robb draws for Tables 1 and A6
│       ├── 19_Hypertension_Table1_CountryWillingnessToPayAndCaseValues.R                # Table 1: country mean/median WTP and VSC
│       ├── 20_Hypertension_Table2_WillingnessToPayDeterminants.R                        # Table 2: determinants and marginal effects
│       ├── 22_Hypertension_TableA1_VariableCoding.R                                     # Table A1: definitions and summary statistics
│       ├── 23_Hypertension_TableA2_BidAmounts.R                                         # Table A2: bids by country
│       ├── 25_Hypertension_Table3_CrossCountryRegressions.R                             # Table 3: cross-country VSC regressions
│       ├── 26_Hypertension_TableA3_AlternativeDistributions.R                           # Table A3: alternative distributions and single-bound model
│       ├── 27_Hypertension_TableA4_ScreeningRobustness.R                                # Table A4: alternative screening rules
│       ├── 28_Hypertension_TableA6_PairwiseCountryComparisons.R                         # Table A6: pairwise Poe tests of country mean WTP
│       ├── 31_Hypertension_TableA7_CountryInfluence.R                                   # Table A7: influence diagnostics, Table 3 Model 3
│       └── 32_Hypertension_TableA8_CountryRobustness.R                                  # Table A8: country VSC under weighting and never-diagnosed checks
└── renv.lock                                                                            # recorded package versions
```
