#### Hypertension: Country Willingness To Pay And Case Values ####
# Purpose: Produce Table 1: country willingness to pay and statistical case values, with 95% confidence intervals.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then scripts 14 and 18.
# Run from the project root: source("Scripts/Tables/19_Hypertension_Table1_CountryWillingnessToPayAndCaseValues.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_KrinskyRobbDraws.rds.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_Table1_CountryWillingnessToPayAndCaseValues.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Simulation_Draws_Path <- file.path(Report_Models, "Hypertension_Model_KrinskyRobbDraws.rds")


if (!file.exists(Simulation_Draws_Path)) {
  stop("Output/Models/Hypertension_Model_KrinskyRobbDraws.rds not found -- run ",
       "Scripts/Tables/18_Hypertension_Table_KrinskyRobbIntervals.R first. This script only reads ",
       "its cached draws; it does not simulate them.", call. = FALSE)
}


KR <- readRDS(Simulation_Draws_Path)


Data <- load_analysis_data()


Sample_Size_By_Country <- table(Data$Country)


## Local currency for each country's mean WTP and VSC columns
CURRENCY <- c(
  "Austria"        = "EUR",
  "Belgium"        = "EUR",
  "Canada"         = "CAD",
  "Finland"        = "EUR",
  "Greece"         = "EUR",
  "Japan"          = "JPY",
  "Portugal"       = "EUR",
  "Slovenia"       = "EUR",
  "United Kingdom" = "GBP",
  "United States"  = "USD"
)


# *************************************************************
#### Section 1: Build the table ####
# *************************************************************


## Krinsky-Robb percentile interval for any of the four simulated statistics.
kr_ci <- function(country, stat) {
  x <- KR$draw_stats[, stat, country]
  unname(quantile(x[is.finite(x)],
                  probs = c(KR$alpha / 2, 1 - KR$alpha / 2), names = FALSE))
}


## Comma thousands separator, no space.
money <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")


mean_with_ci <- function(point, ci) sprintf("%s (%s – %s)", money(point), money(ci[1]), money(ci[2]))


## USD PPP figures are already on the 2023 AIC basis; local currency simply
## multiplies back by the same PPP.
ppp <- read_ppp_aic()


build_row <- function(cc, n) {

  wtp_mean_ci <- kr_ci(cc, "WTP_mean")
  vsc_mean_ci <- kr_ci(cc, "VSC_mean")
  local_mult  <- if (cc == "All") NA_real_ else unname(ppp[cc])
  currency    <- if (cc == "All") "" else unname(CURRENCY[cc])

  data.frame(
    Country                    = cc,
    N                          = n,
    `Mean USD (CI)`            = mean_with_ci(KR$point["WTP_mean", cc], wtp_mean_ci),
    `Mean local`               = if (cc == "All") NA_character_ else paste(currency, money(KR$point["WTP_mean", cc] * local_mult)),
    Median                     = money(KR$point["WTP_median", cc]),
    `Mean USD (CI) `           = mean_with_ci(KR$point["VSC_mean", cc], vsc_mean_ci),
    `Mean local `              = if (cc == "All") NA_character_ else paste(currency, money(KR$point["VSC_mean", cc] * local_mult)),
    `Median `                  = money(KR$point["VSC_median", cc]),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}


CountryRows <- lapply(Report_Countries, function(cc) build_row(cc, unname(Sample_Size_By_Country[cc]))) %>%
  dplyr::bind_rows()


AllRow <- build_row("All", nrow(Data))


PaperTable1 <- dplyr::bind_rows(CountryRows, AllRow)


## Two trailing-space column names (`Mean USD (CI) `, `Mean local `, `Median `)
## disambiguate the VSC triple from the identically-labelled WTP triple --
## fwrite needs unique names. The header row below documents the intended
## two-level grouping (WTP / VSC) that a flat delimited file can't express;
## add that grouping row by hand when pasting into Word.
cat("\n           |------------------- WTP -------------------|------------------- VSC -------------------|\n")


print(as.data.frame(PaperTable1), row.names = FALSE)


# *************************************************************
#### Section 2: Export ####
# *************************************************************


PaperTable1 %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_Table1_CountryWillingnessToPayAndCaseValues.txt"))


cat("\nPaper Table 1 written.\n")
