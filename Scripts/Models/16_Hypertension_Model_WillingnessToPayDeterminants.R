#### Hypertension: Willingness To Pay Determinants ####
# Purpose: Fit and cache the baseline, sociodemographic and health-control Weibull specifications.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Models/16_Hypertension_Model_WillingnessToPayDeterminants.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
# Outputs:
#   Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Analysis_Data <- load_analysis_data()


Country_Weight_Terms <- paste(unname(Country_Weight_Columns), collapse = " + ")


## Specification 2 sociodemographic controls. Kids_Numeric and Urban are built
## by script 13 but are not part of the final specification. Nine variables
## are excluded from the health controls -- see the note below.
Sociodemographic_Terms <- paste(
  "Dummy_Female",
  "LogIncome",
  "INC1_MissingDummy",
  "EducationCategory_Dummy_High",
  "Certainty",
  "MarriedDummy",
  "HH_PersonsNumeric",
  "EmployDummy_Employ",
  # "Kids_Numeric",
  # "Urban",
  sep = " + "
)


## Nine variables dropped 14/08/2026: HBP_PayAnything
## (near-tautological with WTP itself -- "would pay almost anything to avoid
## hypertension" is also the debriefing item Table A4 uses to flag yea-sayers;
## its coefficient dwarfed every other covariate),
## Response_RealLife, Health_Cost_OwnPocket, HBP_10Yr, Diagnosed_None,
## Diagnosed_Respiratory, Health_Weight, Health_Smoke, Health_Exercise --
## none significant in the prior Health-controls fit and none theoretically
## load-bearing (the lifestyle-substitution story is already carried by
## Health_Lifestyle and HBP_Healthier, which stay).
## Health_Relative (raw ordinal, 0=missing, 1-5) added 21/08/2026 per
## Additive alongside the existing
## Health_Relative_BelowAverage/AboveAverage dummies, not a replacement.
Health_Terms <- paste(
  "Health_HadHBP",
  "HBP_RelativeRiskHigh",
  "HBP_Genetic",
  "HBP_Healthier",
  "HBP_Family",
  "Health_Relative",
  # "Health_Relative_BelowAverage",
  # "Health_Relative_AboveAverage",
  "Health_Drink",
  "Health_Lifestyle",
  sep = " + "
)


# *************************************************************
#### Section 1: The three specifications ####
# *************************************************************


make_spec <- function(extra = NULL) {
  rhs <-
    paste(c("1", Country_Weight_Terms, extra, "BASE_Numeric", "RR"), collapse = " + ")
  as.formula(paste("Question1 + Question2 ~", rhs, "| LogBid1 + LogBid2"))
}


Specifications <- list(
  Baseline          = make_spec(NULL),
  `With controls`   = make_spec(Sociodemographic_Terms),
  `Health controls` = make_spec(paste(Sociodemographic_Terms, Health_Terms, sep = " + "))
)


# *************************************************************
#### Section 2: Fit and tabulate ####
# *************************************************************


Models <- lapply(names(Specifications), function(nm) {
  cat("Fitting:", nm, "\n")
  f  <- Specifications[[nm]]
  ## intercept + covariates, then the two bid parameters
  nx <- length(all.vars(f[[3]][[2]])) - 1
  dbchoice(f, data = Analysis_Data, dist = "weibull",
           par = c(4, rep(0, nx), 0.15, -0.6))
})


names(Models) <- names(Specifications)


saveRDS(Models, file.path(Report_Models, "Hypertension_Model_WillingnessToPayDeterminants.rds"))
