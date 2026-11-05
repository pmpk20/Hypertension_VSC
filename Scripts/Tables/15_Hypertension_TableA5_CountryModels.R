#### Hypertension: Country Models ####
# Purpose: Produce Table A5 from the saved pooled and country-specific model estimates.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 14.
# Run from the project root: source("Scripts/Tables/15_Hypertension_TableA5_CountryModels.R", encoding = "UTF-8")
# Inputs:
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds.
# Outputs:
#   Output/Tables/Hypertension_TableA5_CountryModels.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Model_Cache <- readRDS(file.path(Report_Models, "Hypertension_Model_PooledAndCountryModels.rds"))


Country_Models <- Model_Cache$country_models


Country_Welfare <- Model_Cache$country_welfare


Pooled_Welfare <- Model_Cache$pooled_welfare


# *************************************************************
#### Section 1: Assemble Table A5 ####
# *************************************************************


## Coefficient with significance stars and standard error underneath.
## Stars: * p<0.10, ** p<0.05, *** p<0.01.
fmt_coef <- function(est, se, p) {
  stars <- if (is.na(p)) "" else
    if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
  sprintf("%.3f%s (%.3f)", est, stars, se)
}


## One column of Table A5
country_column <- function(country, model, welfare_cs, welfare_pooled) {

  s  <- summary(model)
  cf <- s$coef

  ## Row order follows the published table. dbchoice returns an unnamed
  ## coefficient matrix, so rows are located positionally.
  want <- c("BASE_Numeric", "RR", "BID")
  lab  <- c("Base", "Risk Reduction", "Log (cost)")

  vals <- vapply(want, function(v) {
    j <- coef_row(model, v)
    if (is.na(j)) NA_character_ else fmt_coef(cf[j, 1], cf[j, 2], cf[j, 4])
  }, character(1))

  intercept <- fmt_coef(cf[1, 1], cf[1, 2], cf[1, 4])

  wcs <- welfare_cs     %>% dplyr::filter(Country == country)
  wpl <- welfare_pooled %>% dplyr::filter(Country == country)

  money <- function(x) formatC(x, format = "f", digits = 0, big.mark = " ")

  data.frame(
    Variable = c(lab, "Intercept",
                 "Observations", "AIC", "BIC", "Log-likelihood", "LR Stat",
                 "CS: Mean WTP", "CS: Median WTP", "CS: Mean VSC", "CS: Median VSC",
                 "Pooled: Mean WTP", "Pooled: Median WTP",
                 "Pooled: Mean VSC", "Pooled: Median VSC"),
    Value = c(
      vals, intercept,
      as.character(s$nobs),
      sprintf("%.3f", s$AIC[1]),
      sprintf("%.3f", s$AIC[2]),
      sprintf("%.3f", s$loglik),
      sprintf("%.3f", s$LR.test[1]),
      money(wcs$WTP_mean), money(wcs$WTP_median),
      money(wcs$VSC_mean), money(wcs$VSC_median),
      money(wpl$WTP_mean), money(wpl$WTP_median),
      money(wpl$VSC_mean), money(wpl$VSC_median)
    ),
    stringsAsFactors = FALSE
  )
}


Country_Estimates_Table <- Reduce(
  function(a, b) dplyr::full_join(a, b, by = "Variable"),
  Map(function(cc) {
    country_column(cc, Country_Models[[cc]], Country_Welfare, Pooled_Welfare) %>%
      dplyr::rename(!!cc := Value)
  }, Report_Countries)
)


print(Country_Estimates_Table)


# *************************************************************
#### Section 2: Export ####
# *************************************************************


Country_Estimates_Table %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_TableA5_CountryModels.txt"))
