#### Hypertension: Robustness Checks ####
# Purpose: Assess sensitivity to weighting, age specification, prior diagnosis and missing health responses.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Models/30_Hypertension_Model_RobustnessChecks.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_Table_RobustnessChecks.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Data <- load_analysis_data()


Output_Text_Path <- file.path(Report_Tables, "Hypertension_Table_RobustnessChecks.txt")


sink(Output_Text_Path, split = TRUE)


## Recover the raw Ipsos weight from the weight x country columns
Data$Weight <- rowSums(Data[, unname(Country_Weight_Columns)])


for (cty in Report_Countries) Data[[paste0("D_", gsub(" ", "", cty))]] <- as.numeric(Data$Country == cty)


PPP_i  <- unname(ppp_rescale()[Data$Country])


Maximum_Bid <- max(c(Data$LogBid1, Data$LogBid2))


# *************************************************************
#### Section 1: Weibull DBDC log-likelihood (DCchoice parameterisation) ####
# *************************************************************


## Pr(yes at log-bid lb) = exp(-exp(-(x'b + g*lb))), Equation (6) of the paper;
## checked numerically against dbchoice() below.
dbdc_ll_i <- function(par, X, d) {
  k  <- length(par)
  xb <- as.vector(X %*% par[-k])
  S  <- function(lb) exp(-exp(-(xb + par[k] * lb)))
  s1 <- S(d$LogBid1); s2 <- S(d$LogBid2)
  p <- ifelse(d$Question1 == 1 & d$Question2 == 1, s2,
       ifelse(d$Question1 == 1 & d$Question2 == 0, s1 - s2,
       ifelse(d$Question1 == 0 & d$Question2 == 1, s2 - s1, 1 - s2)))
  log(pmax(p, 1e-300))
}


fit_weighted <- function(X, d, w, start) {
  nll <- function(p) -sum(w * dbdc_ll_i(p, X, d))
  o <- optim(start, nll, method = "BFGS", control = list(maxit = 5000, reltol = 1e-12))
  o <- optim(o$par, nll, method = "BFGS", control = list(maxit = 5000, reltol = 1e-14))
  ## Sandwich (pseudo-ML) standard errors
  H <- optimHess(o$par, nll)
  eps <- 1e-5
  G <- sapply(seq_along(o$par), function(j) {
    up <- o$par; up[j] <- up[j] + eps; dn <- o$par; dn[j] <- dn[j] - eps
    w * (dbdc_ll_i(up, X, d) - dbdc_ll_i(dn, X, d)) / (2 * eps)
  })
  Hi <- solve(H)
  V  <- Hi %*% crossprod(G) %*% Hi
  list(par = o$par, se = sqrt(diag(V)), ll = -o$value)
}


## Closed-form adjusted-truncated mean, identical to the Table 2 script.
adjtrunc <- function(beta, X) {
  k <- length(beta); cpar <- -beta[k]
  XB <- as.vector(X %*% beta[-k]); inv <- 1 / cpar
  s <- exp(XB * inv); t <- exp(cpar * Maximum_Bid - XB)
  s * gamma(inv + 1) * pgamma(t, shape = inv) / (1 - exp(-t))
}


welfare <- function(beta, X, w = rep(1, nrow(X)), d = Data) {
  wtp <- adjtrunc(beta, X) * PPP_i[seq_len(nrow(X))]
  vsc <- vsc_from_wtp(wtp, d$RR)
  by_c <- sapply(Report_Countries, function(cty) {
    i <- d$Country == cty; weighted.mean(vsc[i], w[i])
  })
  list(pooled_wtp = weighted.mean(wtp, w), pooled_vsc = weighted.mean(vsc, w), country_vsc = by_c)
}


# *************************************************************
#### Section 2: (a) weights ####
# *************************************************************


## Published Specification 1, refitted with the hand-coded likelihood
X_pub <- cbind(1, as.matrix(Data[, c("Canada_Weighted", "Austria_Weighted", "Belgium_Weighted",
  "Slovenia_Weighted", "UnitedKingdom_Weighted", "Finland_Weighted", "Greece_Weighted",
  "Portugal_Weighted", "UnitedStates_Weighted", "Japan_Weighted", "BASE_Numeric", "RR")]))


M_pub <- fit_pooled(Data)


ll_check <- sum(dbdc_ll_i(M_pub$coefficients, X_pub, Data))


cat(sprintf("Validation: dbchoice logLik %.3f vs hand-coded at dbchoice estimates %.3f\n",
            M_pub$loglik, ll_check))


stopifnot(abs(M_pub$loglik - ll_check) < 1e-2)


W_pub <- welfare(M_pub$coefficients, X_pub)


## Alternative: unweighted country FE (Austria omitted) + likelihood weights + weighted aggregation
FE <- paste0("D_", gsub(" ", "", setdiff(Report_Countries, "Austria")))


X_fe <- cbind(1, as.matrix(Data[, c(FE, "BASE_Numeric", "RR")]))


start <- c(M_pub$coefficients[1] + mean(M_pub$coefficients[2:11]), rep(0, length(FE)),
           M_pub$coefficients[12:14])


F_w  <- fit_weighted(X_fe, Data, Data$Weight, start)


F_uw <- fit_weighted(X_fe, Data, rep(1, nrow(Data)), start)


W_w  <- welfare(F_w$par,  X_fe, Data$Weight)


W_uw <- welfare(F_uw$par, X_fe)


tab <- data.frame(
  Country = c(Report_Countries, "Pooled"),
  Published = round(c(W_pub$country_vsc, W_pub$pooled_vsc)),
  FE_weightedML = round(c(W_w$country_vsc, W_w$pooled_vsc)),
  FE_unweighted = round(c(W_uw$country_vsc, W_uw$pooled_vsc))
)


tab$Pct_diff_weightedML <- round(100 * (tab$FE_weightedML / tab$Published - 1), 1)


cat("\n(a) Mean VSC (USD 2023 PPP): published Spec 1 vs country FE with weighted pseudo-ML\n")


print(tab, row.names = FALSE)


nm <- c("Intercept", FE, "BASE", "RR", "LogBid")


cat("\nWeighted pseudo-ML coefficients (sandwich SE):\n")


print(data.frame(term = nm, est = round(F_w$par, 4), se = round(F_w$se, 4)), row.names = FALSE)


cat(sprintf("Spearman rank correlation of country VSCs, published vs weighted ML: %.3f\n",
            cor(tab$Published[1:10], tab$FE_weightedML[1:10], method = "spearman")))


# *************************************************************
#### Section 3: (b) age, (c) diagnosed, (d) zero codes, via dbchoice ####
# *************************************************************


WEIGHTS_RHS <- paste(unname(Country_Weight_Columns), collapse = " + ")


Sociodemographic_Terms <- "Dummy_Female + LogIncome + INC1_MissingDummy + EducationCategory_Dummy_High + Certainty + MarriedDummy + HH_PersonsNumeric + EmployDummy_Employ"


Health_Terms   <- "Health_HadHBP + HBP_RelativeRiskHigh + HBP_Genetic + HBP_Healthier + HBP_Family + Health_Relative + Health_Drink + Health_Lifestyle"


Data$AgeWithin <- Data$resp_age - ifelse(Data$BASE_Numeric == 55, 45, 18)  # years since stratum start


spec <- function(extra = NULL) as.formula(paste("Question1 + Question2 ~",
  paste(c("1", WEIGHTS_RHS, extra, "BASE_Numeric", "RR"), collapse = " + "), "| LogBid1 + LogBid2"))


fit <- function(f, d) {
  nx <- length(all.vars(f[[3]][[2]])) - 1
  dbchoice(f, data = d, dist = "weibull", par = c(4, rep(0, nx), 0.15, -0.6))
}


report <- function(label, m, d, terms) {
  cf <- summary(m)$coef
  rn <- c(colnames(m$covariates), "BID")
  wtp <- (m %>% wtp_i())[, 3] * unname(ppp_rescale()[d$Country])
  vsc <- vsc_from_wtp(wtp, d$RR)
  cat(sprintf("\n%s: N = %d, mean WTP = %.2f, mean VSC = %.0f, logLik = %.3f\n",
              label, nrow(d), mean(wtp), mean(vsc), m$loglik))
  for (t in terms) {
    i <- match(t, rn)
    cat(sprintf("   %-28s %8.4f (SE %.4f, p = %.3f)\n", t, cf[i, 1], cf[i, 2], cf[i, 4]))
  }
}


key <- c("BASE_Numeric", "RR", "BID", "LogIncome", "Health_HadHBP", "HBP_RelativeRiskHigh",
         "HBP_Genetic", "HBP_Healthier", "Health_Relative", "Health_Drink", "Health_Lifestyle",
         "resp_age", "AgeWithin")


cat("\n(b) Age within baseline-risk strata\n")


m1a <- fit(spec("AgeWithin"), Data); report("Spec 1 + years since stratum start", m1a, Data, key)


m3  <- fit(spec(paste(Sociodemographic_Terms, Health_Terms, sep = " + ")), Data); report("Spec 3 (published)", m3, Data, key)


m3a <- fit(spec(paste(Sociodemographic_Terms, Health_Terms, "AgeWithin", sep = " + ")), Data); report("Spec 3 + years since stratum start", m3a, Data, key)


cat("\n(c) Respondents never diagnosed with hypertension\n")


DU <- Data[Data$Health_HadHBP == 0, ]


m1u <- fit(spec(), DU); report("Spec 1, undiagnosed only", m1u, DU, key)


wu <- (m1u %>% wtp_i())[, 3] * unname(ppp_rescale()[DU$Country])


vu <- vsc_from_wtp(wu, DU$RR)


print(round(tapply(vu, DU$Country, mean)))


cat("\n(d) Ordinal health items coded 0 = not answered\n")


Z <- Data$Health_Relative == 0 | Data$Health_Drink == 0 | Data$Health_Lifestyle == 0


cat(sprintf("Respondents with any 0 = not answered: %d (Health_Relative %d, Health_Drink %d, Health_Lifestyle %d)\n",
            sum(Z), sum(Data$Health_Relative == 0), sum(Data$Health_Drink == 0), sum(Data$Health_Lifestyle == 0)))


DZ <- Data[!Z, ]


m3z <- fit(spec(paste(Sociodemographic_Terms, Health_Terms, sep = " + ")), DZ); report("Spec 3 excluding item non-response", m3z, DZ, key)


sink()
