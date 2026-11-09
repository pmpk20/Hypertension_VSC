#### Hypertension: Cross Country Regressions ####
# Purpose: Produce Table 3: cross-country regressions of statistical case values on income and health spending.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 14.
# Run from the project root: source("Scripts/Tables/25_Hypertension_Table3_CrossCountryRegressions.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds.
#   Data/Other/OECD_GDP_Per_Capita.csv.
#   Data/Other/OECD_OutOfPocket_Health_Share.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_Table3_CrossCountryRegressions.txt.
#   Output/Tables/Hypertension_Table_CrossCountryRegressionsCooksDistance.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Data <- load_analysis_data()


Weight_Columns <- unname(Country_Weight_Columns)


Data$PostStratWeight <- rowSums(Data[, Weight_Columns])


# *************************************************************
#### Section 1: Country-level VSC (from Specification 1, baseline pooled model) ####
# *************************************************************


Cached_Model_Path <- file.path(Report_Models, "Hypertension_Model_PooledAndCountryModels.rds")


Full_Model <- if (file.exists(Cached_Model_Path)) {
  cat("\nUsing cached full-sample pooled (Specification 1, baseline) model.\n")
  readRDS(Cached_Model_Path)$pooled_model
} else {
  cat("\nNo cache found; fitting full-sample pooled model.\n")
  fit_pooled(Data)
}


Country_Statistical_Case_Value <- attach_welfare(Full_Model, Data) %>%
  summarise_welfare() %>%
  dplyr::select(Country, VSC_mean)


CountryIncome <- Data %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    Income_geo = exp(sum(PostStratWeight * LogIncome) / sum(PostStratWeight)),
    Age_Mean   = mean(resp_age, na.rm = TRUE),
    .groups = "drop"
  )


GDP <- here("Data", "Other", "OECD_GDP_Per_Capita.csv") %>%
  fread() %>%
  dplyr::filter(`Reference area` %in% Report_Countries, TIME_PERIOD == 2023) %>%
  dplyr::select(Country = `Reference area`, GDP_pc = OBS_VALUE)


OOP <- here("Data", "Other", "OECD_OutOfPocket_Health_Share.csv") %>%
  fread() %>%
  dplyr::filter(`Reference area` %in% Report_Countries, TIME_PERIOD == 2022) %>%
  dplyr::transmute(Country = `Reference area`, OOP_share = OBS_VALUE / 100)


## WHO hypertension cascade prevalence (adults 30-79, both sexes), 2024/25 --
## identical values to Scripts/Tables/22_Hypertension_TableA1_VariableCoding.R (duplicated rather
## than sourced), from the WHO Global report on hypertension 2025 (Annex 1
## country profiles).
Prevalence <- tibble::tribble(
  ~Country,             ~Prevalence,
  "Austria",             0.38,
  "Belgium",             0.35,
  "Canada",              0.27,
  "Finland",             0.43,
  "Greece",              0.39,
  "Japan",               0.41,
  "Portugal",            0.41,
  "Slovenia",            0.53,
  "United Kingdom",      0.31,
  "United States",       0.37
)


Country_Data <- Country_Statistical_Case_Value %>%
  dplyr::inner_join(CountryIncome, by = "Country") %>%
  dplyr::inner_join(GDP,           by = "Country") %>%
  dplyr::inner_join(OOP,           by = "Country") %>%
  dplyr::inner_join(Prevalence,    by = "Country")


stopifnot(
  "Expected exactly ten countries with no missing values" =
    nrow(Country_Data) == length(Report_Countries) && !anyNA(Country_Data))


cat("\n#### Country-level data (VSC, GDP, sample income, OOP share, prevalence, age) ####\n")


print(as.data.frame(Country_Data %>%
  dplyr::mutate(VSC_mean = round(VSC_mean), GDP_pc = round(GDP_pc),
                Income_geo = round(Income_geo), OOP_share = round(OOP_share, 4),
                Age_Mean = round(Age_Mean, 2))), row.names = FALSE)


# *************************************************************
#### Section 2: The six specifications ####
# *************************************************************


SPECS <- list(
  `(1)` = log(VSC_mean) ~ log(GDP_pc),
  `(2)` = log(VSC_mean) ~ log(Income_geo),
  `(3)` = log(VSC_mean) ~ log(GDP_pc) + OOP_share,
  `(4)` = log(VSC_mean) ~ OOP_share + Prevalence,
  `(5)` = log(VSC_mean) ~ log(GDP_pc) + Prevalence + Age_Mean,
  `(6)` = log(VSC_mean) ~ log(GDP_pc) + OOP_share + Age_Mean
)


Models <- lapply(SPECS, stats::lm, data = Country_Data)


for (nm in names(Models)) {
  cat(sprintf("\n#### Spec %s: %s ####\n", nm, deparse(SPECS[[nm]])))
  print(summary(Models[[nm]]))
}


# *************************************************************
#### Section 3: Cook's distance -- the most influential observation per spec ####
# *************************************************************


## Japan is expected to be the most influential observation (see Table A7).
## Verified, not assumed: flag any spec where Japan is NOT the max, rather than silently
## reporting Japan's row regardless.
CooksByModel <- lapply(names(Models), function(nm) {
  cd <- cooks.distance(Models[[nm]])
  names(cd) <- Country_Data$Country
  top <- names(cd)[which.max(cd)]
  if (top != "Japan") {
    cat(sprintf(
      "\nFLAG: spec %s's most-influential observation is %s (Cook's D = %.3f), NOT Japan (Cook's D = %.3f).\n",
      nm, top, cd[top], cd["Japan"]))
  }
  data.frame(Spec = nm, MostInfluential = top, CooksD_MostInfluential = cd[top],
             CooksD_Japan = cd["Japan"])
}) %>% dplyr::bind_rows()


cat("\n#### Cook's distance, most influential observation per spec ####\n")


print(CooksByModel, row.names = FALSE)


# *************************************************************
#### Section 4: Assemble the table (no preferred spec named) ####
# *************************************************************


sig_stars <- function(p) {
  dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
}


fmt_coef <- function(model, term) {
  cf <- summary(model)$coefficients
  if (!(term %in% rownames(cf))) return("")
  sprintf("%.3f%s (%.3f)", cf[term, "Estimate"], sig_stars(cf[term, "Pr(>|t|)"]), cf[term, "Std. Error"])
}


TERM_MAP <- c(
  "(Intercept)"      = "Intercept",
  "log(GDP_pc)"      = "Log GDP per capita (OECD)",
  "log(Income_geo)"  = "Log sample income (geometric mean)",
  "OOP_share"        = "OOP health expenditure share (OECD)",
  "Prevalence"       = "Hypertension prevalence (WHO 2025)",
  "Age_Mean"         = "Mean age of sample"
)


ROW_ORDER <- c("Intercept", "Log GDP per capita (OECD)", "Log sample income (geometric mean)",
               "OOP health expenditure share (OECD)", "Hypertension prevalence (WHO 2025)",
               "Mean age of sample")


CoefRows <- lapply(names(TERM_MAP), function(term) {
  label <- unname(TERM_MAP[term])
  vals <- vapply(names(Models), function(nm) fmt_coef(Models[[nm]], term), character(1))
  data.frame(Variable = label, t(vals), check.names = FALSE, stringsAsFactors = FALSE)
}) %>% dplyr::bind_rows()


names(CoefRows) <- c("Variable", names(Models))


CoefRows <- CoefRows[match(ROW_ORDER, CoefRows$Variable), ]


DiagRows <- data.frame(
  Variable = c("Observations", "Residual df", "R-squared", "Adj. R-squared", "Cook's D (Japan)", "Cook's D (most influential)"),
  stringsAsFactors = FALSE
)


for (nm in names(Models)) {
  m <- Models[[nm]]
  cd <- cooks.distance(m); names(cd) <- Country_Data$Country
  DiagRows[[nm]] <- c(
    "10",
    as.character(df.residual(m)),
    sprintf("%.3f", summary(m)$r.squared),
    sprintf("%.3f", summary(m)$adj.r.squared),
    sprintf("%.3f", cd["Japan"]),
    sprintf("%.3f (%s)", max(cd), names(cd)[which.max(cd)])
  )
}


PaperTable3 <- dplyr::bind_rows(CoefRows, DiagRows)


cat("\n#### Paper Table 3: expanded cross-country specifications (no preferred spec) ####\n")


print(as.data.frame(PaperTable3), row.names = FALSE)


cat("Stars: *** p<0.01, ** p<0.05, * p<0.1. Dependent variable in all six",
    "specs: log(VSC_mean), n=10, from Specification 1 (baseline pooled model).",
    "This table intentionally names no preferred specification -- see text",
    "for discussion of which variables are robust vs fragile across columns.\n")


# *************************************************************
#### Section 5: Export ####
# *************************************************************


PaperTable3 %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_Table3_CrossCountryRegressions.txt"))


CooksByModel %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_Table_CrossCountryRegressionsCooksDistance.txt"))


cat("\nPaper Table 3 (expanded cross-country specifications) written.\n")
