#### Hypertension: Willingness To Pay Determinants ####
# Purpose: Produce Table 2: coefficient estimates and simulated marginal effects for the three specifications.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then scripts 16 and 17.
# Run from the project root: source("Scripts/Tables/20_Hypertension_Table2_WillingnessToPayDeterminants.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds.
#   Output/Tables/Hypertension_Table_DeterminantEstimates.txt.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_Table2_WillingnessToPayDeterminants.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


ModelsPath <- file.path(Report_Models, "Hypertension_Model_WillingnessToPayDeterminants.rds")


if (!file.exists(ModelsPath)) {
  stop("Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds not found -- run ",
       "Scripts/Models/16_Hypertension_Model_WillingnessToPayDeterminants.R first.", call. = FALSE)
}


Table7Txt <- file.path(Report_Tables, "Hypertension_Table_DeterminantEstimates.txt")


if (!file.exists(Table7Txt)) {
  stop("Output/Tables/Hypertension_Table_DeterminantEstimates.txt not found -- run ",
       "Scripts/Tables/17_Hypertension_Table_DeterminantEstimates.R first.", call. = FALSE)
}


Models <- readRDS(ModelsPath)


Model  <- Models[["Health controls"]]


Data <- load_analysis_data()


stopifnot("Cached Health-controls model row count disagrees with the analysis data" =
            nrow(Model$covariates) == nrow(Data))


# *************************************************************
#### Section 1: Closed-form Weibull adjusted-truncated mean ####
# *************************************************************


## Same construction as Scripts/Tables/18_Hypertension_Table_KrinskyRobbIntervals.R Section 1 (adjtrunc
## component only -- marginal effects here are reported on the mean, not the
## median).
weibull_adjtrunc <- function(beta, X, max_bid) {

  k    <- length(beta)
  cpar <- -beta[k]

  if (!is.finite(cpar) || cpar <= 0) return(NULL)

  XB  <- as.vector(X %*% beta[-k])
  inv <- 1 / cpar

  s <- exp(XB * inv)
  t <- exp(cpar * max_bid - XB)

  adjtrunc <- s * gamma(inv + 1) * pgamma(t, shape = inv) / (1 - exp(-t))

  if (!all(is.finite(adjtrunc))) return(NULL)

  adjtrunc
}


Coefficient_Estimates <- Model$coefficients


Maximum_Bid  <- max(Model$bid)


Xmat    <- Model$covariates


AdjTrunc <- weibull_adjtrunc(Coefficient_Estimates, Xmat, Maximum_Bid)


stopifnot(!is.null(AdjTrunc))


## Per-respondent check (diagnostic, not gating): 14/08/2026 refit found 2 of
## 9,496 respondents at ~3e-6 relative error (99.9th percentile is 1.3e-7;
## the worst case is $141.9672 vs $141.9676 -- a $0.0004 difference). This is
## integrate()'s own adaptive tolerance being looser for those two respondents'
## particular covariate combination, exactly the residual the README's
## "closed form" section already documents as ~1e-7 and attributes to
## integrate(), not the closed form. Report it; don't gate on it -- AME below
## only ever uses the POPULATION MEAN, which the check right after this one
## verifies directly.
PerRespErr <- abs(AdjTrunc - (Model %>% wtp_i())[, 3]) / abs((Model %>% wtp_i())[, 3])


cat(sprintf("\nClosed-form vs wtp_i(), per-respondent: max relative error %.3g (%d of %d respondents > 1e-6)\n",
            max(PerRespErr), sum(PerRespErr > 1e-6), length(PerRespErr)))


## The check that actually matters: does the closed form agree with wtp_i()
## on the POPULATION MEAN adjusted-truncated WTP -- the only quantity AME
## uses. This is far tighter than the per-respondent max because the two
## outlier respondents above wash out in a 9,496-respondent average.
WtpI <- Model %>% wtp_i()


mean_err <- abs(mean(AdjTrunc) - mean(WtpI[, 3])) / abs(mean(WtpI[, 3]))


cat(sprintf("Closed-form vs wtp_i(), population mean: relative error %.3g\n", mean_err))


stopifnot(mean_err < 1e-6)


## USD PPP, matching Scripts/Tables/17_Hypertension_Table_DeterminantEstimates.R's own ppp_vec construction.
PPP_i <- unname(ppp_rescale()[Data$Country])


mean_wtp_usd <- function(beta, X) {
  w <- weibull_adjtrunc(beta, X, Maximum_Bid)
  mean(w * PPP_i)
}


# *************************************************************
#### Section 2: Average marginal effects, Health-controls specification ####
# *************************************************************


CovNames <- colnames(Xmat)


## Weighted country dummies and the intercept are not reported here -- they
## are identity/location terms rather than substantive covariates.
SKIP_VARS <- c("(Intercept)", grep("_Weighted$", CovNames, value = TRUE))


## Revised 14/08/2026 alongside Scripts/Tables/17_Hypertension_Table_DeterminantEstimates.R's variable-list
## change: the nine dropped variables are gone from
## this list too (HBP_PayAnything, Response_RealLife, Health_Cost_OwnPocket,
## HBP_10Yr, Diagnosed_None, Diagnosed_Respiratory, Health_Weight,
## Health_Smoke, Health_Exercise), and Urban joins as a tenth dummy.
DUMMY_VARS <- c(
  "Dummy_Female", "INC1_MissingDummy", "EducationCategory_Dummy_High",
  "MarriedDummy", "EmployDummy_Employ", "Urban",
  "Health_HadHBP", "HBP_RelativeRiskHigh", "HBP_Genetic", "HBP_Healthier",
  "HBP_Family", "Health_Relative_BelowAverage", "Health_Relative_AboveAverage"
)


LOG_VARS <- "LogIncome"


## Everything else reported (Certainty, HH_PersonsNumeric, Kids_Numeric,
## BASE_Numeric, RR, Health_Drink, Health_Lifestyle) is an ordinal/count
## scale: the marginal effect is evaluated for a one-unit increase.

WtpBaseline <- mean_wtp_usd(Coefficient_Estimates, Xmat)


cat(sprintf("Population mean adjusted-truncated WTP (USD PPP), Health controls: $%.2f\n",
            WtpBaseline))


average_marginal_effect <- function(var) {

  col <- which(CovNames == var)
  X1  <- Xmat

  if (var %in% DUMMY_VARS) {

    X0 <- Xmat; X0[, col] <- 0
    X1[, col] <- 1
    wtp0 <- mean_wtp_usd(Coefficient_Estimates, X0)
    wtp1 <- mean_wtp_usd(Coefficient_Estimates, X1)
    ame  <- wtp1 - wtp0
    pct  <- 100 * ame / wtp0

  } else if (var %in% LOG_VARS) {

    X1[, col] <- Xmat[, col] + log(1.01)
    wtp1 <- mean_wtp_usd(Coefficient_Estimates, X1)
    ame  <- wtp1 - WtpBaseline
    pct  <- 100 * ame / WtpBaseline

  } else {

    X1[, col] <- Xmat[, col] + 1
    wtp1 <- mean_wtp_usd(Coefficient_Estimates, X1)
    ame  <- wtp1 - WtpBaseline
    pct  <- 100 * ame / WtpBaseline
  }

  data.frame(Variable = var, AME_USD = ame, AME_Pct = pct)
}


ReportedVars <- setdiff(CovNames, SKIP_VARS)


AME <- lapply(ReportedVars, average_marginal_effect) %>% dplyr::bind_rows()


print(AME)


# *************************************************************
#### Section 3: Income elasticity harmonisation ####
# *************************************************************


## Two distinct elasticity concepts, both from this same Health-controls fit,
## full 10-country sample (not a LOOCV fold):
##  - SIMULATION (canonical, used for benefit transfer): the LogIncome row's
##    own AME_Pct above IS this elasticity -- a 1% income shift is exactly
##    what average_marginal_effect() applies for LOG_VARS, so "% change in
##    mean WTP" per "1% change in income" is already an elasticity.
##  - ANALYTICAL: beta_LogIncome / |beta_bid|, exact for the Weibull MEDIAN
##    (a pure log-linear function of the index), NOT the truncated mean this
##    table's MeanWTP/MeanVSC rows and the AME column use.
Elasticity_Sim <- AME$AME_Pct[AME$Variable == "LogIncome"]


CfHealth <- summary(Model)$coef[, "Estimate"]


Elasticity_Analytical <- unname(
  CfHealth[coef_row(Model, "LogIncome")] / abs(CfHealth[coef_row(Model, "BID")])
)


cat(sprintf(
  "\nIncome elasticity (Health controls, full sample): simulation (truncated mean) = %.4f, analytical (median) = %.4f\n",
  Elasticity_Sim, Elasticity_Analytical))


## Revised 20/10/2026 (reviewer pass): Krinsky-Robb 95% CIs for both
## elasticities. The paper previously attached the LogIncome coefficient's
## p-value to the simulated elasticity, which has no SE of its own. Krinsky-Robb
## draws use the inverse Hessian and a Cholesky factor (1,000 draws); each draw re-runs the 1% income
## shift through the closed-form adjusted-truncated mean.
Simulation_Draw_Count <- 1000


Simulation_Seed  <- 20261020


Significance_Level <- 0.05


Coefficient_Covariance <- solve(Model$Hessian)


Coefficient_Covariance <- (Coefficient_Covariance + t(Coefficient_Covariance)) / 2


set.seed(Simulation_Seed)


Draws <- matrix(rnorm(Simulation_Draw_Count * length(Coefficient_Estimates)), nrow = Simulation_Draw_Count) %*% chol(Coefficient_Covariance)


Draws <- sweep(Draws, 2, Coefficient_Estimates, "+")


IncCol  <- which(CovNames == "LogIncome")


XmatInc <- Xmat


XmatInc[, IncCol] <- Xmat[, IncCol] + log(1.01)


elasticity_sim_draw <- function(beta) {
  w0 <- weibull_adjtrunc(beta, Xmat, Maximum_Bid)
  w1 <- weibull_adjtrunc(beta, XmatInc, Maximum_Bid)
  if (is.null(w0) || is.null(w1)) return(NA_real_)
  100 * (mean(w1 * PPP_i) / mean(w0 * PPP_i) - 1)
}


ElasSimDraws <- apply(Draws, 1, elasticity_sim_draw)


ElasAnaDraws <- Draws[, IncCol] / abs(Draws[, ncol(Draws)])


cat(sprintf("KR elasticity draws: %d, invalid: %d\n", Simulation_Draw_Count, sum(is.na(ElasSimDraws))))


stopifnot(mean(is.na(ElasSimDraws)) < 0.01)


kr_ci <- function(x) stats::quantile(x, c(Significance_Level / 2, 1 - Significance_Level / 2), na.rm = TRUE, names = FALSE)


Elasticity_Sim_CI        <- kr_ci(ElasSimDraws)


Elasticity_Analytical_CI <- kr_ci(ElasAnaDraws)


cat(sprintf("  simulation 95%% CI [%.4f, %.4f]; analytical 95%% CI [%.4f, %.4f]\n",
            Elasticity_Sim_CI[1], Elasticity_Sim_CI[2],
            Elasticity_Analytical_CI[1], Elasticity_Analytical_CI[2]))


# *************************************************************
#### Section 4: Merge onto the coefficient table, format, export ####
# *************************************************************


Determinants_Table <- fread(Table7Txt, sep = "#")


## left_join leaves non-covariate rows (Diagnostics, AIC, ...) as NA in the
## two new columns, which fwrite renders blank -- the same convention the
## Specification 1 and 2 columns use for rows a specification omits.
PaperTable2 <- Determinants_Table %>%
  dplyr::left_join(
    AME %>%
      dplyr::transmute(
        Variable,
        `Marginal effect on WTP (%)`   = sprintf("%+.2f%%", AME_Pct),
        `Marginal effect on WTP (USD)` = sprintf("%+.2f", AME_USD)
      ),
    by = "Variable"
  )


## Move the diagnostics panel (fit
## statistics, N, AIC/BIC, WTP/VSC summary rows) down to the end of the
## table, so every substantive coefficient row is visible together first.
## dplyr::filter() preserves each subset's existing relative row order, so
## this is a pure reordering -- no row is dropped, added, or renamed.
DIAG_ROWS <- c(
  "Diagnostics", "Observations", "Post-stratification weight x country dummies",
  "AIC", "BIC", "LogLik", "LR Stat",
  "MeanWTP", "MedianWTP", "MeanVSC", "MedianVSC"
)


PaperTable2 <- dplyr::bind_rows(
  PaperTable2 %>% dplyr::filter(!Variable %in% DIAG_ROWS),
  PaperTable2 %>% dplyr::filter(Variable %in% DIAG_ROWS)
)


## Human-readable row labels, and one
## row dropped outright ("Post-stratification weight x country dummies" --
## every spec has it, it isn't informative row-by-row). RELABEL_MAP is typed
## out in full below rather than built programmatically so the mapping is
## auditable in a diff.
##
## FLAG: HBP_Genetic's requested label text was IDENTICAL to
## HBP_RelativeRiskHigh's ("Risk of developing high blood pressure compared
## to people of your age and gender is larger") -- almost certainly a
## copy-paste slip, since two different variables can't share one row label
## without a reader assuming they're the same question. Cross-checked
## against Data/Codebook/SWACHE_Hypertension_Codebook.csv: HBP_RelativeRiskHigh
## is question H2 ("How do you rate your risk of developing high blood
## pressure compared to people of your age and gender?"), which matches the
## text given. HBP_Genetic is question H6 ("Is high blood pressure a genetic
## condition in your family?"), which the variable NAME already matches but
## the given LABEL TEXT didn't. Used H6's actual wording for HBP_Genetic
## below -- flag this in case a different label was intended.
##
## Also normalised a handful of missing-space typos in the pasted question
## text (e.g. "youhad", "bloodpressure", "yourage", "havehad", "7hours"),
## consistent with line-wrap artifacts from a copy-paste rather than
## intentional wording -- meaning preserved, spacing fixed.
RELABEL_MAP <- c(
  "(Intercept)"                   = "Intercept",
  "Bid"                           = "Log (bid level)",
  "BASE_Numeric"                  = "Base risk",
  "RR"                            = "Risk reduction",

  "EmployDummy_Employ"            = "Employed (0/1)",
  "Dummy_Female"                  = "Female (0/1)",
  "EducationCategory_Dummy_High"  = "High education (0/1)",
  "HH_PersonsNumeric"             = "Household size (0 - 7)",
  "LogIncome"                     = "Log (income)",
  "INC1_MissingDummy"             = "Missing income (0/1)",
  "MarriedDummy"                  = "Married (0/1)",
  "Certainty"                     = "Response certainty (0 - 4)",
  # "Kids_Numeric"                  = "Number of children (0 - 9)",
  # "Urban"                         = "Urban (0/1)",
  "Health_Drink"                  = "Frequency of alcoholic drinks",
  "HBP_Genetic"                   = "Genetic condition (0/1)",
  "Health_HadHBP"                 = "Has a doctor ever told you that you had high blood pressure? (0/1)",
  "Health_Lifestyle"              = "Do you consider yourself to have a healthy lifestyle?",
  "HBP_RelativeRiskHigh"          = "Risk of developing high blood pressure compared to people of your age and gender is larger (0/1)",
  "HBP_Family"                    = "People close to you (family, friends) who have, or have had, high blood pressure? (0/1)",
  "HBP_Healthier"                 = "Just adopt a healthier lifestyle to reduce your risk instead of paying? (0/1)",
  "Health_Relative"               = "Relative health compared to others of their age and gender in their country (1 - 5)",
  # "Health_Relative_BelowAverage"  = "Below average health relative to others of their age and gender in their country (0/1)",
  # "Health_Relative_AboveAverage"  = "Above average health relative to others of their age and gender in their country (0/1)",
  "Diagnostics"                   = "Diagnostics",
  "Observations"                  = "Observations",
  "AIC"                           = "AIC",
  "BIC"                           = "BIC",
  "LogLik"                        = "Log-likelihood",
  "LR Stat"                       = "Likelihood-ratio statistic",
  "MeanWTP"                       = "Mean WTP (USD2023 PPP)",
  "MedianWTP"                     = "Median WTP (USD2023 PPP)",
  "MeanVSC"                       = "Mean VSC (USD2023 PPP)",
  "MedianVSC"                     = "Median VSC (USD2023 PPP)"
)


stopifnot("RELABEL_MAP is missing a Variable present in PaperTable2 (excluding the row being dropped)" =
            all(setdiff(PaperTable2$Variable, "Post-stratification weight x country dummies") %in% names(RELABEL_MAP)))


PaperTable2 <- PaperTable2 %>%
  dplyr::filter(Variable != "Post-stratification weight x country dummies") %>%
  dplyr::mutate(Variable = unname(RELABEL_MAP[Variable]))


## Explicit, clearly-labelled footer rows
## for both elasticity concepts, appended after the relabelling step so they
## bypass RELABEL_MAP entirely (their text is already reader-facing). Value
## goes in the "Health controls" column since that's the spec both numbers
## come from; the two marginal-effect columns don't apply to a summary row.
ElasticityFooter <- data.frame(
  Variable          = c("Income elasticity (simulation, truncated mean)",
                         "Income elasticity (analytical, median)"),
  Baseline          = c("", ""),
  `With controls`   = c("", ""),
  `Health controls` = sprintf("%.3f [%.3f, %.3f]",
                              c(Elasticity_Sim, Elasticity_Analytical),
                              c(Elasticity_Sim_CI[1], Elasticity_Analytical_CI[1]),
                              c(Elasticity_Sim_CI[2], Elasticity_Analytical_CI[2])),
  `Marginal effect on WTP (%)`   = c("", ""),
  `Marginal effect on WTP (USD)` = c("", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)


PaperTable2 <- dplyr::bind_rows(PaperTable2, ElasticityFooter)


print(as.data.frame(PaperTable2), row.names = FALSE)


PaperTable2 %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_Table2_WillingnessToPayDeterminants.txt"))


## Footnote appended below the data rows. The output is a flat "#"-delimited
## text table with no in-band footnote mechanism, so the note is written as
## trailing lines in the same file rather than fabricated as a table row.
## Greek beta kept as ASCII ("beta_income"/"beta_bid") to match this repo's
## existing comment convention rather than introduce the first non-ASCII
## character into a pipe-delimited output file.
Footnote <- sprintf(paste(
  "Note: The marginal effect of log(income) reports the simulation-based",
  "elasticity of the truncated mean WTP: the percentage change in mean WTP",
  "resulting from a 1%% increase in all respondents' incomes, computed by",
  "reintegration of the survival function. The analytical ratio",
  "beta_income/|beta_bid| = %.3f gives the exact elasticity of the Weibull",
  "median WTP, which is a log-linear function of the scale parameter.",
  "Elasticity 95%% confidence",
  "intervals in brackets are from %d Krinsky-Robb draws."
), Elasticity_Analytical, Simulation_Draw_Count)


cat(Footnote, "\n", file = file.path(Report_Tables, "Hypertension_Table2_WillingnessToPayDeterminants.txt"),
    append = TRUE)


cat("\nPaper Table 2 (determinants of WTP, with marginal effects) written.\n")
