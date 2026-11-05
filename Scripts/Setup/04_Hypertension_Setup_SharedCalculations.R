#### Hypertension: Shared Calculations ####
# Purpose: Provide shared model formulas, purchasing-power conversions and statistical case-value calculations.
# Author: Anonymised for peer review.
# Prerequisites: Restore packages with script 01; script 03 is sourced automatically when needed.
# Run from the project root: source("Scripts/Setup/04_Hypertension_Setup_SharedCalculations.R", encoding = "UTF-8")
# Inputs:
#   Shared formulas and constants defined here.
#   Functions read Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv and Data/Other/OECD_PPP_ActualIndividualConsumption.csv when called.
# Outputs:
#   Shared functions and constants in the R session; creates Output subfolders.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


library(here)


library(magrittr)


library(data.table)


library(dplyr)


library(tidyr)


library(DCchoice)


library(Ecdat)


library(Ecfun)


## This file loads the repository's dbchoice(), wtp_i() and spike extensions.
## The local dbchoice() masks DCchoice::dbchoice(); preserve that distinction
## when investigating estimates or updating the upstream package.
if (!isTRUE(get0("Hypertension_Estimators_Loaded", ifnotfound = FALSE))) {
  source(here("Scripts", "Models", "03_Hypertension_Model_DefineEstimators.R"), encoding = "UTF-8")
}


# *************************************************************
#### Section 1: Output locations ####
# *************************************************************


## Text tables, figures and cached model objects each get their own folder so
## the large .rds files never sit alongside the human-readable output.
Report_Tables  <- here("Output", "Tables")


Report_Figures <- here("Output", "Figures")


Report_Models  <- here("Output", "Models")


for (.d in c(Report_Tables, Report_Figures, Report_Models)) {
  if (!dir.exists(.d)) dir.create(.d, recursive = TRUE)
}


# *************************************************************
#### Section 2: Convert willingness to pay to statistical case values ####
# *************************************************************


## VSC_mu = WTP_mu * ((1 - exp(-r*t)) / r) / DeltaR
Discount_Rate  <- 0.03   # r, EC (2023) social discount rate, report footnote 4


Payment_Period <- 10     # t, years


Annuity_Factor <-
  (1 - exp(-Discount_Rate * Payment_Period)) / Discount_Rate  # 8.639387


## RR is recorded in the data in percentage points (values 10 and 20),
## so the risk reduction as a proportion is RR/100.
Risk_Reduction_Divisor <- 100


## Single place where WTP becomes VSC. Everything downstream calls this.
##
## The default conversion discounts ten annual payments and divides by
## the risk reduction as a proportion. The alternative conversion is an
## undiscounted sensitivity calculation; it is not used for the paper tables.
vsc_from_wtp <- function(wtp, RR, variant = c("Report", "AnnexC")) {
  variant <- match.arg(variant)
  switch(variant,
    Report = wtp * Annuity_Factor / (RR / Risk_Reduction_Divisor),
    AnnexC = wtp / RR * 1000
  )
}


# *************************************************************
#### Section 3: Purchasing power parities ####
# *************************************************************


## The cleaned data are already denominated in USD at 2021 GDP PPPs: every
## Setup script (02_, 04_, 07_, 08_) divides bids and income by these factors.
## Any change of PPP basis is therefore a pure per-country rescaling applied
## AFTER estimation -- see the note at the end of this section.
PPP_BASE_2021_GDP <- c(
  "Austria"        =  0.720350,
  "Belgium"        =  0.719916,
  "Canada"         =  1.163492,
  "Finland"        =  0.790539,
  "Greece"         =  0.520812,
  "Japan"          = 99.211289,
  "Portugal"       =  0.540472,
  "Slovenia"       =  0.537477,
  "United Kingdom" =  0.668934,
  "United States"  =  1.000000
)


## OECD Data Explorer, "Annual Purchasing Power Parities and exchange rates",
## transaction = Purchasing Power Parities for actual individual consumption,
## unit = national currency per US dollar. Renamed from the literal OECD Data
## Explorer export filename; stored in Data/Other/
## alongside the other OECD reference datasets.
PPP_AIC_FILE <- here("Data", "Other", "OECD_PPP_ActualIndividualConsumption.csv")


## The repository's one PPP basis:
## OECD AIC, 2023 vintage. No other vintage or basis is read anywhere.
read_ppp_aic <- function() {
  out <- PPP_AIC_FILE %>%
    fread() %>%
    dplyr::filter(TIME_PERIOD == 2023) %>%
    dplyr::select(Country = `Reference area`, PPP = OBS_VALUE)

  setNames(out$PPP, out$Country)[names(PPP_BASE_2021_GDP)]
}


## Multiplicative factor taking a USD value on the 2021 GDP PPP basis onto the
## 2023 OECD AIC basis. A value in local currency L is L/PPP under either
## basis, so USD_2023 = USD_2021 * PPP_2021 / PPP_2023.
##
## NOTE ON WHERE THIS IS APPLIED: rescaling happens AFTER estimation, not
## before -- see attach_welfare(). So the model, and every coefficient table,
## is unaffected by the PPP basis; only the welfare measures move.
ppp_rescale <- function() {
  f <- PPP_BASE_2021_GDP / read_ppp_aic()
  f[names(PPP_BASE_2021_GDP)]
}


# *************************************************************
#### Section 4: Data and the baseline pooled specification ####
# *************************************************************


load_analysis_data <- function() {
  # here("Data", "Data_AllCountries_SubsampledForAnalysis.csv") %>%
    here("Data", "Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv") %>%
    fread() %>%
    data.frame()
}


## Study countries, in table order
Report_Countries <- c("Austria", "Belgium", "Canada", "Finland", "Greece",
                      "Japan", "Portugal", "Slovenia", "United Kingdom",
                      "United States")


## Weighted country-dummy column for each country
Country_Weight_Columns <- c(
  "Austria"        = "Austria_Weighted",
  "Belgium"        = "Belgium_Weighted",
  "Canada"         = "Canada_Weighted",
  "Finland"        = "Finland_Weighted",
  "Greece"         = "Greece_Weighted",
  "Japan"          = "Japan_Weighted",
  "Portugal"       = "Portugal_Weighted",
  "Slovenia"       = "Slovenia_Weighted",
  "United Kingdom" = "UnitedKingdom_Weighted",
  "United States"  = "UnitedStates_Weighted"
)


## Baseline pooled specification: Weibull DBDC, no spike, intercept and
## ten country indicators interacted with survey weights.
Pooled_Specification <- Question1 + Question2 ~
  1 +
  Canada_Weighted +
  Austria_Weighted +
  Belgium_Weighted +
  Slovenia_Weighted +
  UnitedKingdom_Weighted +
  Finland_Weighted +
  Greece_Weighted +
  Portugal_Weighted +
  UnitedStates_Weighted +
  Japan_Weighted +
  BASE_Numeric +
  RR |
  LogBid1 + LogBid2


POOLED_START <- c(4, rep(0, 11), 0.15, -0.6)


fit_pooled <- function(data, spec = Pooled_Specification, par = POOLED_START) {
  dbchoice(spec, data = data, dist = "weibull", par = par)
}


# *************************************************************
#### Section 5: Individual-level welfare, on the 2023 AIC PPP basis ####
# *************************************************************


## Attaches individual WTP and VSC to the data, already rescaled to the 2023
## AIC PPP basis. wtp_i() returns four columns; this analysis uses the
## truncated-with-adjustment mean and the individual median.
attach_welfare <- function(model, data, variant = "Report") {

  w <- model %>% wtp_i() %>% data.frame()
  names(w) <- c("WTP_mean_raw", "WTP_trunc_raw", "WTP_adjtrunc_raw", "WTP_median_raw")

  f <- ppp_rescale()

  data %>%
    dplyr::bind_cols(w) %>%
    dplyr::mutate(
      PPP_Factor = unname(f[Country]),
      WTP_mean   = WTP_adjtrunc_raw * PPP_Factor,
      WTP_median = WTP_median_raw   * PPP_Factor,
      VSC_mean   = vsc_from_wtp(WTP_mean,   RR, variant),
      VSC_median = vsc_from_wtp(WTP_median, RR, variant)
    )
}


## Country-level aggregation averages the individual MEAN measures
## and takes the median of the individual MEDIAN measures; using mean() for both
## would give a different summary for the median columns.
summarise_welfare <- function(data_with_welfare) {
  data_with_welfare %>%
    dplyr::group_by(Country) %>%
    dplyr::summarise(
      n          = dplyr::n(),
      WTP_mean   = mean(WTP_mean),
      WTP_median = median(WTP_median),
      VSC_mean   = mean(VSC_mean),
      VSC_median = median(VSC_median),
      .groups    = "drop"
    )
}


## Positional coefficient lookup. dbchoice returns an unnamed coefficient
## matrix; the rows are the covariates in order, then the bid coefficient.
coef_row <- function(model, variable) {
  nm <- c(colnames(model$covariates), "BID")
  match(variable, nm)
}


# *************************************************************
#### Section 6: Coefficient-table output ####
# *************************************************************


## Format coefficient tables and calculate VSC with vsc_from_wtp().
## Significance codes: * p<0.10, ** p<0.05, *** p<0.01.
TableOutputter_DBCHOICE <- function(Model,
                                    Country = NULL,
                                    ppp_factor = 1,
                                    variant = "Report") {

  sumry <- summary(Model)
  wtp   <- Model %>% wtp_i() %>% data.frame()

  ## dbchoice's summary() exposes the coefficient matrix as $coef; DCchoice's
  ## summary.sbchoice() (used for the single-bound column) calls it $coefmat.
  ## Same columns either way (Estimate / Std. Error / z value / Pr(>|z|)).
  coef_mat <- if (!is.null(sumry$coef)) sumry$coef else sumry$coefmat

  stars <- function(p) {
    if (is.na(p)) ""      else
    if (p < 0.01)  "***"  else
    if (p < 0.05)  "**"   else
    if (p < 0.1)   "*"    else ""
  }

  var_names <- c(colnames(Model$covariates), "Bid")

  estimates <- vapply(seq_along(var_names), function(i) {
    sprintf("%.3f%s (%.3f)",
            coef_mat[i, "Estimate"],
            stars(coef_mat[i, "Pr(>|z|)"]),
            coef_mat[i, "Std. Error"])
  }, character(1))

  vars_df <- data.frame(Variable = var_names,
                        Estimate = estimates,
                        stringsAsFactors = FALSE)

  RR_i       <- data.frame(sumry$covariates)$RR
  wtp_mean   <- wtp$individual.mean.WTP..truncated.at.the.maximum.bid.with.adjustment. * ppp_factor
  wtp_median <- wtp$individual.median.WTP * ppp_factor

  vsc        <- vsc_from_wtp(wtp_mean,   RR_i, variant)
  median_vsc <- vsc_from_wtp(wtp_median, RR_i, variant)

  diag_df <- data.frame(
    ## "(published)" marks rows that appear in the printed report table;
    ## "(added)" marks rows we compute but the report does not print for
    ## Tables 6-9, so they have nothing to be validated against.
    Variable = c("Diagnostics", "Observations",
                 if (!is.null(Country)) "Country" else NULL,
                 "Post-stratification weight x country dummies",
                 "AIC", "BIC", "LogLik", "LR Stat",
                 "MeanWTP", "MedianWTP", "MeanVSC", "MedianVSC"),
    Estimate = c(
      "Diagnostics",
      as.character(sumry$nobs),
      if (!is.null(Country)) Country else NULL,
      if (any(grepl("_Weighted$", colnames(Model$covariates)))) "Yes" else "No",
      sprintf("%.3f", sumry$AIC[1]),
      sprintf("%.3f", sumry$AIC[2]),
      sprintf("%.3f", sumry$loglik),
      sprintf("%.3f", sumry$LR.test[1]),
      ## Estimand:
      ## every mean/median WTP and VSC in the paper is the SAMPLE MEAN /
      ## MEDIAN OF THE INDIVIDUAL measures -- mean_i E[WTP_i | x_i], not
      ## the fitted WTP evaluated at the mean covariate vector. This is
      ## the population-mean estimand a regulatory VSC requires (total
      ## benefit = N x E[WTP]) and matches SS4.1's aggregation argument.
      ## Aggregation is asymmetric, exactly as
      ## Scripts/Tables/18_Hypertension_Table_KrinskyRobbIntervals.R's
      ## aggregate_welfare() does it: mean() for the mean rows, median()
      ## for the median rows. wtp_mean / wtp_median are the individual
      ## adjusted-truncated mean and the individual median WTP (from
      ## wtp_i(), all five distributions), already PPP-rescaled above.
      sprintf("$%.2f", mean(wtp_mean)),
      sprintf("$%.2f", median(wtp_median)),
      sprintf("$%.2f", mean(vsc)),
      sprintf("$%.2f", median(median_vsc))
    ),
    stringsAsFactors = FALSE
  )

  rbind(vars_df, diag_df)
}


## Same, for spike models fitted with dbspike(). Adds the spike-share row.
## Significance codes:
##   * p<0.10, ** p<0.05, *** p<0.01
TableOutputter_Spike <- function(Model, ppp_factor = 1, variant = "Report") {

  sumry <- summary.spike(Model)
  spk   <- spikeCoef(Model)
  wtp   <- Model %>% wtp_i() %>% data.frame()

  stars <- function(p) {
    if (is.na(p)) ""      else
    if (p < 0.01)  "***"  else
    if (p < 0.05)  "**"   else
    if (p < 0.1)   "*"    else ""
  }

  fmt <- function(est, se, p) sprintf("%.3f%s (%.3f)", est, stars(p), se)

  vars_df <- data.frame(
    Variable = c(colnames(Model$covariates), "Bid"),
    Estimate = mapply(fmt,
                      sumry$coef[, "Estimate"],
                      sumry$coef[, "Std. Error"],
                      sumry$coef[, "Pr(>|z|)"]),
    stringsAsFactors = FALSE
  )

  spike_df <- data.frame(Variable = "Spike",
                         Estimate = fmt(spk[, 1], spk[, 2], spk[, 4]),
                         stringsAsFactors = FALSE)

  RR_i       <- sumry$data$RR
  wtp_mean   <- wtp$individual.mean.WTP..truncated.at.the.maximum.bid.with.adjustment. * ppp_factor
  wtp_median <- wtp$individual.median.WTP * ppp_factor

  diag_df <- data.frame(
    Variable = c("Diagnostics", "Observations",
                 "Post-stratification weight x country dummies",
                 "AIC", "BIC", "LogLik", "LR Stat",
                 "MeanWTP", "MedianWTP", "MeanVSC", "MedianVSC"),
    Estimate = c(
      "Diagnostics",
      as.character(sumry$nobs),
      ## The "No weights" column drops the weighted dummies entirely
      if (any(grepl("_Weighted$", colnames(Model$covariates)))) "Yes" else "No",
      sprintf("%.3f", sumry$AIC[1]),
      sprintf("%.3f", sumry$AIC[2]),
      sprintf("%.3f", sumry$loglik),
      sprintf("%.3f", sumry$LR.test[1]),
      ## Sample mean / median of the INDIVIDUAL adjusted-truncated WTP and
      ## median WTP -- the population-mean estimand, consistent with
      ## TableOutputter_DBCHOICE and aggregate_welfare(). For spike fits the adjusted-
      ## truncated individual figure is conditional on not being in the spike.
      sprintf("$%.2f", mean(wtp_mean)),
      sprintf("$%.2f", median(wtp_median)),
      sprintf("$%.2f", mean(vsc_from_wtp(wtp_mean,   RR_i, variant))),
      sprintf("$%.2f", median(vsc_from_wtp(wtp_median, RR_i, variant)))
    ),
    stringsAsFactors = FALSE
  )

  rbind(vars_df, spike_df, diag_df)
}


## Stitch a named list of model outputs into one wide table
stitch_models <- function(outputs, col_names = names(outputs)) {
  out <- data.frame(Variable = outputs[[1]]$Variable, stringsAsFactors = FALSE)
  for (i in seq_along(outputs)) {
    out <- dplyr::full_join(
      out,
      outputs[[i]] %>% dplyr::rename(!!col_names[i] := Estimate),
      by = "Variable"
    )
  }
  out
}


cat("Analysis helpers loaded. PPP basis = AIC2023",
    "| annuity factor =", round(Annuity_Factor, 6), "\n")
