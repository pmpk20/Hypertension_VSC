#### Hypertension: Screening Robustness ####
# Purpose: Produce Table A4: compare estimates under alternative respondent-screening rules.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Tables/27_Hypertension_TableA4_ScreeningRobustness.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_TableA4_ScreeningRobustness.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Analysis_Data <- load_analysis_data()


## The consequentiality screens drop only the two DISAGREE categories --
## neutral respondents are kept. Getting this wrong changes n from the
## published 9 257 / 9 066 to 8 465 / 7 715, so it matters.
NOT_DISAGREE <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree")


# *************************************************************
#### Section 1: The screens ####
# *************************************************************


## Each entry is a predicate applied to the cleaned analysis sample.
Screens <- list(
  `Baseline`             = function(d) rep(TRUE, nrow(d)),
  `Believed 10yr risk`   = function(d) d$HBP_10Yr != 0,
  `Yea-sayers removed`   = function(d) d$HBP_PayAnything != 1,
  `Consequential (A1_1)` = function(d) d$A1_1 %in% NOT_DISAGREE,
  `Consequential (A1_2)` = function(d) d$A1_2 %in% NOT_DISAGREE
)


SPEC <- as.formula(paste(
  "Question1 + Question2 ~ 1 +",
  paste(unname(Country_Weight_Columns), collapse = " + "),
  "+ BASE_Numeric + RR | LogBid1 + LogBid2"
))


# *************************************************************
#### Section 2: Fit and tabulate ####
# *************************************************************


ScreenN <- integer(0)   # respondents retained under each screen, filled below


Outputs <- lapply(names(Screens), function(nm) {

  d <- Analysis_Data[Screens[[nm]](Analysis_Data), ]
  ScreenN[[nm]] <<- nrow(d)
  cat(sprintf("Fitting: %-22s n = %d\n", nm, nrow(d)))

  m <- dbchoice(SPEC, data = d, dist = "weibull",
                par = c(4, rep(0, 11), 0.15, -0.6))

  TableOutputter_DBCHOICE(m, ppp_factor = unname(ppp_rescale()[d$Country]))
})


names(Outputs) <- names(Screens)


Table8 <- stitch_models(Outputs) %>%
  dplyr::filter(!grepl("_Weighted$", Variable))


## Merge in the pooled exclusion counts: rename
## the "Observations" row to "Included" and insert an "Excluded" row directly
## below it -- N (% of the N=9,496 baseline) dropped by each screen alone.
BaselineN <- ScreenN[["Baseline"]]


excluded  <- BaselineN - ScreenN[names(Screens)]


ExcludedRow <- c(
  Variable = "Excluded",
  setNames(sprintf("%d (%.1f%%)", excluded, 100 * excluded / BaselineN), names(Screens))
)


Table8 <- Table8 %>%
  dplyr::filter(Variable != "Post-stratification weight x country dummies") %>%
  dplyr::mutate(Variable = ifelse(Variable == "Observations", "Included", Variable))


pos <- which(Table8$Variable == "Included")


Table8 <- rbind(
  Table8[seq_len(pos), , drop = FALSE],
  as.list(ExcludedRow),
  Table8[seq(pos + 1, nrow(Table8)), , drop = FALSE]
)


print(Table8)


# *************************************************************
#### Section 3: Export ####
# *************************************************************


OUT <- file.path(Report_Tables, "Hypertension_TableA4_ScreeningRobustness.txt")


Table8 %>% fwrite(sep = "#", OUT)


cat(paste(
  "\nNote: each column re-estimates the pooled Specification 1 (baseline)",
  "Weibull model on the sample remaining after one discretionary screen is",
  "applied ALONE to the N=9,496 baseline analysis sample. 'Included' is that",
  "sample size; 'Excluded' is the number (and % of 9,496) the screen drops.",
  "Screens: thought the risk change was permanent (HBP_10Yr==0); yea-sayer,",
  "would pay almost anything (HBP_PayAnything==1); DBDC answers didn't reflect",
  "real-life choice (A1_1 disagree); didn't feel informed enough (A1_2",
  "disagree). Prior HBP diagnosis is not screened here -- it is a covariate in",
  "the individual-level specifications. WTP/VSC",
  "rows are the sample mean/median of the individual measures on the 2023 AIC",
  "PPP basis.\n"),
  file = OUT, append = TRUE)


cat("\nTable A4 written.\n")
