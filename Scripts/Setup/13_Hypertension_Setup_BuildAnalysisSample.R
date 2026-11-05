#### Hypertension: Build Analysis Sample ####
# Purpose: Apply the baseline screening rules and export the 9,496-respondent analysis sample.
# Author: Anonymised for peer review.
# Prerequisites: Run script 12; keep the setup libraries and WORK path helper loaded.
# Run from the project root: source("Scripts/Setup/13_Hypertension_Setup_BuildAnalysisSample.R", encoding = "UTF-8")
# Inputs:
#   Data/Intermediate/Data_AllCountries_WithJapan_Step3.csv.
# Outputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


Data <- WORK("Data_AllCountries_WithJapan_Step3.csv") %>%
  fread() %>%
  data.frame()


message(sprintf("[setup] merged file: %d rows x %d cols", nrow(Data), ncol(Data)))


## The composite key.  Respondent_Serial ALONE IS NOT UNIQUE -- it repeats
## across waves (9,055 distinct values over 12,182 rows).  Assert both facts so
## a future change that breaks either is caught here.
stopifnot(all(SWACHE_KEY %in% names(Data)))


if (anyDuplicated(Data[, SWACHE_KEY]) > 0) {
  stop("Composite key (Country, Respondent_Serial) is not unique in the merged file.",
       call. = FALSE)
}


message(sprintf("[setup] key check: %d rows, %d distinct Respondent_Serial, composite key unique.",
                nrow(Data), length(unique(Data$Respondent_Serial))))


# *************************************************************
#### Section 1: Screening and column selection ####
# *************************************************************

Data_Backup <- Data


## Truncate the sample by speeders (valuation section and across the entire
## survey) and by those failing the probability test.  Deterministic -- there is
## no RNG anywhere in Setup, so no seed is needed or wanted.
Analysis_Data <- subset(Data, (Data$Speeders_Survey_TestDummy != 0 &
                                                Data$Speeders_Valuation_TestDummy != 0 &
                                                Data$ProbabilityTest_Numeric != 0 &
                                                !Data$LogIncome %>% is.na()))


## This should already be calculated, but no harm in ensuring
Data$Japan_Weighted <- (Data$Japan * Data$weight)


## Redefining here to drop protests
Analysis_Data$Spike <- ifelse(
  Analysis_Data$DBDC == "NN" &
    Analysis_Data$Q4 == "No, I would not pay anything" &
    Analysis_Data$Q5a  == 0 &
    Analysis_Data$Q5b %in% c(
      "I do not believe the information provided in the survey",
      "Other (Please Specify: )"
    ),
  1,
  0
)


## Just ensure we have consistent recoding here
Analysis_Data$EmployDummy_Employ <-
  ifelse(Analysis_Data$EMP01 %in% c("Employed full-time",
                                                   "Employed part-time",
                                                   "Military",
                                                   "Own business manager / employer",
                                                   "Self employed"),
         1,
         0)


## Consistency
Analysis_Data$Health_Relative_BelowAverage <- ifelse(Analysis_Data$H10 %in% c("Below average", "Well below average"), 1, 0)


Analysis_Data$Health_Relative_AboveAverage <- ifelse(Analysis_Data$H10 %in% c("Well above average", "Above average"), 1, 0)


## Fix issue of the 5 NAs in health_drink from the UK
## This is a coding issue
Analysis_Data <- Analysis_Data %>%
  mutate(Health_Drink = ifelse(is.na(Health_Drink), 1, Health_Drink))


## Missing-income indicator: 1 if the first income question was refused or
## answered "do not know", otherwise 0. Retain the merged indicator outside
## Japan: some country-specific source dummies are no longer available here.
## For Japan, replace the second-question indicator with INCJP1_MissingDummy.
.norm01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0L
  if (max(x) > 1) as.integer(x == max(x)) else as.integer(x == 1)
}


Analysis_Data$INC1_MissingDummy_Legacy <-
  Analysis_Data$INC1_MissingDummy


.is_jp <- Analysis_Data$Country == "Japan"


stopifnot("INCJP1_MissingDummy" %in% names(Analysis_Data))


.jp1 <- .norm01(Analysis_Data$INCJP1_MissingDummy)


Analysis_Data$INC1_MissingDummy[.is_jp] <- .jp1[.is_jp]


message(sprintf(
  "[harmonise] INC1_MissingDummy: Japan switched from INCJP2 (n=%d) to INCJP1 (n=%d) of %d Japanese rows; all other countries unchanged.",
  sum(Analysis_Data$INC1_MissingDummy_Legacy[.is_jp]),
  sum(Analysis_Data$INC1_MissingDummy[.is_jp]),
  sum(.is_jp)))


## Retain the analysis variables and Respondent_Serial as the within-country row key.
Analysis_Data <- Analysis_Data %>%
  dplyr::select(c(
    DBDC,
    Question1,
    Question2,
    Country,
    Respondent_Serial,   ## <-- ADDED (row key; frozen 00_ dropped it)
    Austria_Weighted ,
    Belgium_Weighted,
    Canada_Weighted,
    Finland_Weighted,
    Greece_Weighted,
    Portugal_Weighted,
    Slovenia_Weighted,
    UnitedKingdom_Weighted,
    UnitedStates_Weighted,
    Japan_Weighted,

    ## Sociodemographic
    resp_age, ## Testing
    Dummy_Female,
    LogIncome,
    INC1_MissingDummy,
    EducationCategory_Dummy_High,
    Certainty,
    MarriedDummy,
    HH_PersonsNumeric,
    EmployDummy_Employ,
    Kids_Numeric,   ## <-- ADDED: number of
                    ##     children under 18 in the household, from KIDS02.
                    ##     Already derived identically in 01_/05_/06_/08_; just
                    ##     never selected into the analysis file before now.
    Urban,          ## <-- ADDED, same change: 1 if SE2 == "Urban" else 0
                    ##     (Suburban and Rural collapse to 0 -- that collapse
                    ##     is the existing derivation in 01_/05_/06_/08_, not
                    ##     something introduced here).
    BASE_Numeric,

    ## Health
    Health_HadHBP,
    Health_HBP_DiagnosedStuck,
    Health_HBP_SideEffects,
    Health_Cost_OwnPocket,
    HBP_RelativeRiskHigh,
    HBP_10Yr,
    HBP_PayAnything,
    HBP_Genetic,
    HBP_Healthier,
    HBP_Family,
    Health_Relative,  ## <-- ADDED: raw ordinal (0-5) health-relative-to-others
                      ##     variable, already derived identically in
                      ##     01_/05_/06_/08_; just never selected into the
                      ##     analysis file before now (only the two binary
                      ##     cuts below were kept).
    Health_Relative_BelowAverage,
    Health_Relative_AboveAverage,
    Health_Weight,
    Health_Smoke,
    Health_Drink,
    Health_Exercise,
    Health_Lifestyle,
    Diagnosed_None,
    Diagnosed_Respiratory,
    Response_RealLife,
    RR,
    LogBid1,
    LogBid2,
    A1_1,
    A1_2,
    Spike
  ))


# *************************************************************
#### Section 2: Export -- to the REBUILD filename, never over the live file ####
# *************************************************************
#
# The rebuilt CSV is the input used by load_analysis_data(). The older
# unsuffixed CSV is retained for provenance and is not read by this pipeline.

message(sprintf("[setup] analysis file: %d rows x %d cols",
                nrow(Analysis_Data), ncol(Analysis_Data)))


print(table(Analysis_Data$Country))


Analysis_Data %>%
  data.frame() %>%
  fwrite(sep = ",",
         here("Data", "Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv"))


message("[setup] wrote Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv")


# *************************************************************
#### Section 3: END OF SCRIPT ####
# *************************************************************
