#### Hypertension: Country Influence ####
# Purpose: Produce Table A7: assess the influence of individual countries on cross-country regressions.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 14.
# Run from the project root: source("Scripts/Tables/31_Hypertension_TableA7_CountryInfluence.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds.
#   Data/Other/OECD_GDP_Per_Capita.csv.
#   Data/Other/OECD_OutOfPocket_Health_Share.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_TableA7_CountryInfluence.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Data <- load_analysis_data()


Weight_Columns <- unname(Country_Weight_Columns)


Data$PostStratWeight <- rowSums(Data[, Weight_Columns])


# *************************************************************
#### Section 1: Country-level data (same construction as Paper Table 3) ####
# *************************************************************


Cached_Model_Path <- file.path(Report_Models, "Hypertension_Model_PooledAndCountryModels.rds")


Full_Model  <- readRDS(Cached_Model_Path)$pooled_model


Country_Statistical_Case_Value <- attach_welfare(Full_Model, Data) %>%
  summarise_welfare() %>%
  dplyr::select(Country, VSC_mean)


GDP <- here("Data", "Other", "OECD_GDP_Per_Capita.csv") %>%
  fread() %>%
  dplyr::filter(`Reference area` %in% Report_Countries, TIME_PERIOD == 2023) %>%
  dplyr::select(Country = `Reference area`, GDP_pc = OBS_VALUE)


OOP <- here("Data", "Other", "OECD_OutOfPocket_Health_Share.csv") %>%
  fread() %>%
  dplyr::filter(`Reference area` %in% Report_Countries, TIME_PERIOD == 2022) %>%
  dplyr::transmute(Country = `Reference area`, OOP_share = OBS_VALUE / 100)


Country_Data <- Country_Statistical_Case_Value %>%
  dplyr::inner_join(GDP, by = "Country") %>%
  dplyr::inner_join(OOP, by = "Country")


stopifnot(nrow(Country_Data) == length(Report_Countries), !anyNA(Country_Data))


# *************************************************************
#### Section 2: Full-sample fit (Model 3) ####
# *************************************************************


FORM  <- log(VSC_mean) ~ log(GDP_pc) + OOP_share


full  <- stats::lm(FORM, data = Country_Data)


cf    <- summary(full)$coefficients


b_oop_full  <- cf["OOP_share", "Estimate"]


p_oop_full  <- cf["OOP_share", "Pr(>|t|)"]


b_gdp_full  <- cf["log(GDP_pc)", "Estimate"]


cat(sprintf("\nFull sample (n=10): OOP_share = %.3f (p = %.3f), log(GDP_pc) = %.3f, R-sq = %.3f\n",
            b_oop_full, p_oop_full, b_gdp_full, summary(full)$r.squared))


# *************************************************************
#### Section 3: Leave-one-country-out refits + Cook's D / leverage / DFBETAS ####
# *************************************************************


infl  <- stats::influence.measures(full)


cooks <- stats::cooks.distance(full)


hat   <- stats::hatvalues(full)


dfb   <- as.data.frame(stats::dfbetas(full))


names(dfb) <- c("DFB_Intercept", "DFB_logGDP", "DFB_OOP")


k <- length(coef(full))               # 3 parameters


n <- nrow(Country_Data)                 # 10


cook_thresh <- 4 / n                    # common rule of thumb


hat_thresh  <- 2 * k / n


dfb_thresh  <- 2 / sqrt(n)


rows <- lapply(seq_len(n), function(i) {
  cc  <- Country_Data$Country[i]
  loo <- stats::lm(FORM, data = Country_Data[-i, ])
  s   <- summary(loo)$coefficients
  data.frame(
    Dropped        = cc,
    OOP_coef       = s["OOP_share", "Estimate"],
    OOP_SE         = s["OOP_share", "Std. Error"],
    OOP_p          = s["OOP_share", "Pr(>|t|)"],
    logGDP_coef    = s["log(GDP_pc)", "Estimate"],
    R2             = summary(loo)$r.squared,
    CooksD         = unname(cooks[i]),
    Leverage_hat   = unname(hat[i]),
    DFBETAS_OOP    = dfb$DFB_OOP[i],
    stringsAsFactors = FALSE
  )
})


LOO <- do.call(rbind, rows)


LOO <- LOO[order(-LOO$CooksD), ]


cat("\n#### Leave-one-country-out, Model 3: log(VSC_mean) ~ log(GDP_pc) + OOP_share ####\n")


print(LOO, row.names = FALSE, digits = 3)


cat(sprintf("\nThresholds (n=10, k=3): Cook's D > %.3f, leverage h_ii > %.3f, |DFBETAS| > %.3f\n",
            cook_thresh, hat_thresh, dfb_thresh))


# *************************************************************
#### Section 4: Assemble the appendix table ####
# *************************************************************


fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)


AppTable <- data.frame(
  `Country dropped`               = LOO$Dropped,
  `OOP coef.`                     = paste0(fmt(LOO$OOP_coef), " (", fmt(LOO$OOP_SE), ")"),
  `OOP p-value`                   = fmt(LOO$OOP_p),
  `log GDP coef.`                 = fmt(LOO$logGDP_coef),
  `R-squared`                     = fmt(LOO$R2),
  `Cook's D`                      = fmt(LOO$CooksD),
  `Leverage (h)`                  = fmt(LOO$Leverage_hat),
  `DFBETAS (OOP)`                 = fmt(LOO$DFBETAS_OOP),
  check.names = FALSE, stringsAsFactors = FALSE
)


## Full-sample reference row on top
ref <- data.frame(
  `Country dropped`  = "(none -- full sample)",
  `OOP coef.`        = paste0(fmt(b_oop_full), " (", fmt(cf["OOP_share", "Std. Error"]), ")"),
  `OOP p-value`      = fmt(p_oop_full),
  `log GDP coef.`    = fmt(b_gdp_full),
  `R-squared`        = fmt(summary(full)$r.squared),
  `Cook's D`         = "",
  `Leverage (h)`     = "",
  `DFBETAS (OOP)`    = "",
  check.names = FALSE, stringsAsFactors = FALSE
)


AppTable <- rbind(ref, AppTable)


cat("\n#### Appendix table: influence diagnostics, cross-country Model 3 ####\n")


print(AppTable, row.names = FALSE)


# *************************************************************
#### Section 5: Export ####
# *************************************************************


AppTable %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_TableA7_CountryInfluence.txt"))


cat("\nAppendix cross-country influence-diagnostics table written.\n")
