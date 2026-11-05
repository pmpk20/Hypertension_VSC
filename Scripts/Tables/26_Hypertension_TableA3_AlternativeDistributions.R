#### Hypertension: Alternative Distributions ####
# Purpose: Produce Table A3: compare willingness-to-pay estimates under alternative parametric distributions.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Tables/26_Hypertension_TableA3_AlternativeDistributions.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_TableA3_AlternativeDistributions.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Analysis_Data <- load_analysis_data()


## Shared right-hand side: weighted country dummies + baseline risk + risk
## reduction. DBDC models take both bids ( | LogBid1 + LogBid2 ); the SBDC
## column takes the first bid only ( | LogBid1 ).
RHS_COVARS <- paste(
  "1 +", paste(unname(Country_Weight_Columns), collapse = " + "),
  "+ BASE_Numeric + RR"
)


RHS_DBDC <- paste(RHS_COVARS, "| LogBid1 + LogBid2")


RHS_SBDC <- paste(RHS_COVARS, "| LogBid1")


# *************************************************************
#### Section 1: The four specifications ####
# *************************************************************


## All four are NO-SPIKE, matching the main results (Table 1 / Table 2). The
## first three vary the error distribution of the DBDC model; the fourth
## re-estimates the Weibull model as single-bounded dichotomous choice (SBDC),
## using only the first bid.
##   Weibull | Log-logistic | Log-normal | SBDC
Specifications <- list(
  Weibull = list(
    engine = "dbchoice",
    form   = as.formula(paste("Question1 + Question2 ~", RHS_DBDC)),
    dist   = "weibull",
    par    = c(4, rep(0, 11), 0.15, -0.6)
  ),
  `Log-logistic` = list(
    engine = "dbchoice",
    form   = as.formula(paste("Question1 + Question2 ~", RHS_DBDC)),
    dist   = "log-logistic",
    par    = c(4, rep(0, 11), 0.15, -0.6)
  ),
  `Log-normal` = list(
    engine = "dbchoice",
    form   = as.formula(paste("Question1 + Question2 ~", RHS_DBDC)),
    dist   = "log-normal",
    par    = c(4, rep(0, 11), 0.15, -0.6)
  ),
  SBDC = list(
    engine = "sbchoice",
    form   = as.formula(paste("Question1 ~", RHS_SBDC)),
    dist   = "weibull",
    par    = NULL
  )
)


# *************************************************************
#### Section 2: Fit and tabulate ####
# *************************************************************


## PPP factor is per respondent, because a pooled model spans all ten countries.
## vsc_from_wtp() and the outputters are vectorised, so a vector is fine here.
ppp_vec <- unname(ppp_rescale()[Analysis_Data$Country])


Outputs <- lapply(names(Specifications), function(nm) {

  s <- Specifications[[nm]]
  cat("Fitting:", nm, "\n")

  if (s$engine == "sbchoice") {
    ## Single-bounded dichotomous choice, first bid only. DCchoice::sbchoice
    ## fits via glm() internally, so no start values are needed. Its summary()
    ## exposes the coefficient matrix as $coefmat rather than $coef, which
    ## TableOutputter_DBCHOICE() now handles.
    m <- DCchoice::sbchoice(s$form, data = Analysis_Data,
                            dist = s$dist)
    TableOutputter_DBCHOICE(m, ppp_factor = ppp_vec)
  } else {
    m <- dbchoice(s$form, data = Analysis_Data,
                  dist = s$dist, par = s$par)
    TableOutputter_DBCHOICE(m, ppp_factor = ppp_vec)
  }
})


names(Outputs) <- names(Specifications)


Table6 <- stitch_models(Outputs)


## Drop the weighted-dummy rows, as the published table does, and record
## instead whether they were included
Table6 <- Table6 %>%
  dplyr::filter(!grepl("_Weighted$", Variable))


print(Table6)


# *************************************************************
#### Section 3: Export ####
# *************************************************************


Table6 %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_TableA3_AlternativeDistributions.txt"))


cat("\nTable A3 written.\n")
