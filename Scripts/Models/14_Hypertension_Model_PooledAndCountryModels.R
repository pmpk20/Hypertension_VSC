#### Hypertension: Pooled And Country Models ####
# Purpose: Fit pooled and country-specific Weibull models and cache their estimates and welfare summaries.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Models/14_Hypertension_Model_PooledAndCountryModels.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Analysis_Data <- load_analysis_data()


# *************************************************************
#### Section 1: Country-specific models ####
# *************************************************************


## One model per country: intercept, that country's weighted dummy, baseline
## risk and risk reduction, with the two logged bids as the cost variables.
fit_country_model <- function(country, data) {

  d <- subset(data, data$Country == country)

  spec <- as.formula(sprintf(
    "Question1 + Question2 ~ 1 + %s + BASE_Numeric + RR | LogBid1 + LogBid2",
    Country_Weight_Columns[[country]]
  ))

  dbchoice(spec, data = d, dist = "weibull", par = c(4, 0, 0, 0.15, -0.6))
}


cat("\nFitting country-specific models...\n")


Country_Models <- lapply(Report_Countries, function(cc) {
  cat("  ", cc, "\n")
  fit_country_model(cc, Analysis_Data)
})


names(Country_Models) <- Report_Countries


# *************************************************************
#### Section 2: Pooled model ####
# *************************************************************


cat("\nFitting pooled model...\n")


Pooled_Model <- fit_pooled(Analysis_Data)


## Individual welfare from the pooled model, already on the 2023 AIC PPP basis
Data_With_Welfare <- attach_welfare(Pooled_Model, Analysis_Data)


Pooled_Welfare     <- summarise_welfare(Data_With_Welfare)


# *************************************************************
#### Section 3: Welfare from the country-specific models ####
# *************************************************************


## Same arithmetic as the pooled case, but each country's own model
country_specific_welfare <- function(country, model, data) {

  d <- subset(data, data$Country == country)
  w <- model %>% wtp_i() %>% data.frame()
  names(w) <- c("WTP_mean_raw", "WTP_trunc_raw", "WTP_adjtrunc_raw", "WTP_median_raw")

  f <- unname(ppp_rescale()[country])

  data.frame(
    Country    = country,
    WTP_mean   =   mean(w$WTP_adjtrunc_raw * f),
    WTP_median = median(w$WTP_median_raw   * f),
    VSC_mean   =   mean(vsc_from_wtp(w$WTP_adjtrunc_raw * f, d$RR)),
    VSC_median = median(vsc_from_wtp(w$WTP_median_raw   * f, d$RR))
  )
}


Country_Welfare <- Map(
  function(cc, m) country_specific_welfare(cc, m, Analysis_Data),
  Report_Countries, Country_Models
) %>%
  dplyr::bind_rows()


## Cache everything the downstream scripts need so nothing is re-estimated
list(
  pooled_model     = Pooled_Model,
  country_models   = Country_Models,
  data_with_welfare = Data_With_Welfare,
  pooled_welfare   = Pooled_Welfare,
  country_welfare  = Country_Welfare
) %>%
  saveRDS(file.path(Report_Models, "Hypertension_Model_PooledAndCountryModels.rds"))
