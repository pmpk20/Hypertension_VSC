#### Hypertension: Krinsky Robb Intervals ####
# Purpose: calculate Krinsky-Robb confidence intervals for willingness to pay and statistical case values.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 14.
# Run from the project root: source("Scripts/Tables/18_Hypertension_Table_KrinskyRobbIntervals.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Models/Hypertension_Model_KrinskyRobbDraws.rds.
#   Output/Tables/Hypertension_Table_KrinskyRobbIntervals.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Simulation_Draw_Count <- 10000


Simulation_Seed  <- 20260726


Significance_Level <- 0.05


Data <- load_analysis_data()


## Reuse the cached fit. PPP is applied AFTER estimation (see the note in
## 04_Hypertension_Setup_SharedCalculations.R Section 3), so the cached model is basis-independent
## and can be re-expressed on AIC2023 without re-fitting.
Cached_Model_Path <- file.path(Report_Models, "Hypertension_Model_PooledAndCountryModels.rds")


if (file.exists(Cached_Model_Path)) {
  cat("\nUsing cached full-sample pooled model.\n")
  Pooled_Model <- readRDS(Cached_Model_Path)$pooled_model
} else {
  cat("\nNo cache found; fitting full-sample pooled model.\n")
  Pooled_Model <- fit_pooled(Data)
}


## This script re-fits/re-simulates from PooledModel directly rather than
## reusing Hypertension_Model_PooledAndCountryModels.rds's cached welfare columns -- see README item R2.
stopifnot(nrow(Pooled_Model$covariates) == nrow(Data))


# *************************************************************
#### Section 1: Closed-form Weibull welfare ####
# *************************************************************


## Returns the two welfare measures used in Tables 1 and A5, for one coefficient vector.
##   beta     : full coefficient vector, bid coefficient LAST
##   X        : model$covariates
##   max_bid  : max(model$bid), on the log scale
##
## NULL signals a draw that is not a valid Weibull (non-negative bid
## coefficient, or a non-finite value); the caller drops it and counts it.
weibull_welfare <- function(beta, X, max_bid) {

  k    <- length(beta)
  cpar <- -beta[k]                       # = |b_bid| for any admissible draw

  if (!is.finite(cpar) || cpar <= 0) return(NULL)

  XB  <- as.vector(X %*% beta[-k])
  inv <- 1 / cpar

  s <- exp(XB * inv)                     # Weibull scale
  t <- exp(cpar * max_bid - XB)          # = a * U^c

  adjtrunc <- s * gamma(inv + 1) * pgamma(t, shape = inv) / (1 - exp(-t))
  medn     <- s * log(2) ^ inv

  if (!all(is.finite(adjtrunc)) || !all(is.finite(medn))) return(NULL)

  list(adjtrunc = adjtrunc, median = medn)
}


# *************************************************************
#### Section 2: Verification -- closed form against wtp_i() ####
# *************************************************************


cat("\nVerifying the closed form against wtp_i()...\n")


Coefficient_Estimates <- Pooled_Model$coefficients


Maximum_Bid  <- max(Pooled_Model$bid)


Xmat    <- Pooled_Model$covariates


PointWelfare <- weibull_welfare(Coefficient_Estimates, Xmat, Maximum_Bid)


stopifnot(!is.null(PointWelfare))


WtpI <- Pooled_Model %>% wtp_i()


err_adj <- max(abs(PointWelfare$adjtrunc - WtpI[, 3]) / abs(WtpI[, 3]))


err_med <- max(abs(PointWelfare$median   - WtpI[, 4]) / abs(WtpI[, 4]))


cat(sprintf("  adjusted truncated mean: max relative error %.3g\n", err_adj))


cat(sprintf("  median                 : max relative error %.3g\n", err_med))


## 1e-6 is loose against integrate()'s own tolerance, which is what the ~1e-7
## residual actually reflects -- the closed form is the exact one of the pair.
stopifnot(err_adj < 1e-6, err_med < 1e-6)


## Second, stronger check: the point estimates this script produces must equal
## what the existing pipeline produces via attach_welfare()/summarise_welfare().
PipelineWelfare <- attach_welfare(Pooled_Model, Data) %>%
  summarise_welfare()


cat("Verifying against attach_welfare() + summarise_welfare()...\n")


# *************************************************************
#### Section 3: Aggregation, matching summarise_welfare() exactly ####
# *************************************************************


## Per-respondent constants, hoisted out of the draw loop.
PPP_i <- unname(ppp_rescale()[Data$Country])


VSC_i <- Annuity_Factor / (Data$RR / Risk_Reduction_Divisor)   # vsc_from_wtp(), "Report"


stopifnot(all(is.finite(PPP_i)), all(is.finite(VSC_i)))


## Row indices per output row. The trailing "All" row pools every respondent.
GROUPS <- c(
  lapply(Report_Countries, function(cc) which(Data$Country == cc)),
  list(seq_len(nrow(Data)))
)


names(GROUPS) <- c(Report_Countries, "All")


stopifnot(lengths(GROUPS[Report_Countries]) > 0)


STAT_NAMES <- c("WTP_mean", "WTP_median", "VSC_mean", "VSC_median")


## The four published statistics for one coefficient vector's welfare.
## mean() for the mean measures, median() for the median measures -- this is
## the asymmetry summarise_welfare() documents, and it must be preserved or the
## intervals would not bracket the point estimates.
aggregate_welfare <- function(w) {

  wtp_m <- w$adjtrunc * PPP_i
  wtp_d <- w$median   * PPP_i

  vsc_m <- wtp_m * VSC_i
  vsc_d <- wtp_d * VSC_i

  vapply(GROUPS, function(g) {
    c(mean(wtp_m[g]), median(wtp_d[g]), mean(vsc_m[g]), median(vsc_d[g]))
  }, numeric(4))                                    # 4 stats x 11 groups
}


Point_Estimates <- aggregate_welfare(PointWelfare)


rownames(Point_Estimates) <- STAT_NAMES


## Assert the point estimates match the existing pipeline. Anything above
## rounding noise means this script and the rest of the repo disagree.
for (cc in Report_Countries) {
  p <- PipelineWelfare %>% dplyr::filter(Country == cc)
  d <- max(abs(Point_Estimates[, cc] -
                 c(p$WTP_mean, p$WTP_median, p$VSC_mean, p$VSC_median)) /
             c(p$WTP_mean, p$WTP_median, p$VSC_mean, p$VSC_median))
  if (d > 1e-6) stop("Point estimates disagree with attach_welfare() for ", cc)
}


cat("  all ten countries agree to <1e-6 relative.\n")


# *************************************************************
#### Section 4: Krinsky-Robb draws ####
# *************************************************************


## V = solve(Hessian). DCchoice's dbchoice has no vcov() method and stores the
## Hessian of the NEGATIVE log-likelihood, so this is the covariance directly:
## verified below against the standard errors summary() prints.
Coefficient_Covariance <- solve(Pooled_Model$Hessian)


SE_check <- sqrt(diag(Coefficient_Covariance))


SE_summ  <- summary(Pooled_Model)$coef[, "Std. Error"]


cat(sprintf("\nvcov check: max |sqrt(diag(V)) - summary SE| = %.3g\n",
            max(abs(SE_check - SE_summ))))


stopifnot(max(abs(SE_check - SE_summ)) < 1e-8)


## Symmetrise before factorising -- solve() can leave asymmetry at ~1e-18.
Coefficient_Covariance <- (Coefficient_Covariance + t(Coefficient_Covariance)) / 2


Chol <- chol(Coefficient_Covariance)   # upper triangular, t(Chol) %*% Chol == VCov


set.seed(Simulation_Seed)


## z %*% Chol has covariance t(Chol) %*% Chol = V, for z a row of iid normals.
Draws <- matrix(rnorm(Simulation_Draw_Count * length(Coefficient_Estimates)),
                nrow = Simulation_Draw_Count) %*% Chol


Draws <- sweep(Draws, 2, Coefficient_Estimates, "+")


cat(sprintf("\nRunning %d Krinsky-Robb draws...\n", Simulation_Draw_Count))


Draw_Statistics <- array(NA_real_,
                   dim = c(Simulation_Draw_Count, 4, length(GROUPS)),
                   dimnames = list(NULL, STAT_NAMES, names(GROUPS)))


n_invalid <- 0L


t0 <- Sys.time()


for (r in seq_len(Simulation_Draw_Count)) {

  w <- weibull_welfare(Draws[r, ], Xmat, Maximum_Bid)

  if (is.null(w)) { n_invalid <- n_invalid + 1L; next }

  Draw_Statistics[r, , ] <- aggregate_welfare(w)

  if (r %% 1000 == 0) {
    cat(sprintf("  %5d / %d  (%.0f s elapsed)\n", r, Simulation_Draw_Count,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
}


cat(sprintf("Done in %.0f s. Invalid draws dropped: %d\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs")), n_invalid))


## A KR with a material share of inadmissible draws would not be trustworthy.
## With b_bid = -0.506 (se 0.0065) a non-negative draw is ~78 sd away, so this
## should always be zero; the guard is here so it cannot pass silently.
stopifnot(n_invalid / Simulation_Draw_Count < 0.001)


# *************************************************************
#### Section 5: Calculate percentile confidence intervals ####
# *************************************************************


kr_ci <- function(group, stat) {
  x <- Draw_Statistics[, stat, group]
  unname(quantile(x[is.finite(x)],
                  probs = c(Significance_Level / 2, 1 - Significance_Level / 2), names = FALSE))
}


Results <- lapply(names(GROUPS), function(gg) {
  row <- data.frame(Country = gg, n = length(GROUPS[[gg]]),
                    stringsAsFactors = FALSE)
  for (st in STAT_NAMES) {
    kr <- kr_ci(gg, st)
    row[[st]]                  <- Point_Estimates[st, gg]
    row[[paste0(st, "_lo")]]   <- kr[1]
    row[[paste0(st, "_hi")]]   <- kr[2]
  }
  row
}) %>% dplyr::bind_rows()


# *************************************************************
#### Section 6: Export confidence intervals and simulation draws ####
# *************************************************************


money <- function(x) formatC(x, format = "f", digits = 0, big.mark = " ")


Confidence_Interval_Table <- Results %>%
  dplyr::mutate(dplyr::across(-c(Country, n), money))


cat("\n#### Krinsky-Robb confidence intervals, PPP basis AIC2023 ####\n")


print(as.data.frame(Confidence_Interval_Table), row.names = FALSE)


Confidence_Interval_Table %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_Table_KrinskyRobbIntervals.txt"))


## Save the joint draws for subsequent tables and figures.
list(
  draws      = Simulation_Draw_Count,
  seed       = Simulation_Seed,
  alpha      = Significance_Level,
  n_invalid  = n_invalid,
  coefficients = Coefficient_Estimates,
  vcov       = Coefficient_Covariance,
  point      = Point_Estimates,
  draw_stats = Draw_Statistics
) %>%
  saveRDS(file.path(Report_Models, "Hypertension_Model_KrinskyRobbDraws.rds"))


cat("\nKrinsky-Robb confidence intervals written. Draws cached to Hypertension_Model_KrinskyRobbDraws.rds\n")
