#### Hypertension: Variable Coding ####
# Purpose: Produce Table A1: variable definitions, coding and summary statistics.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 14.
# Run from the project root: source("Scripts/Tables/22_Hypertension_TableA1_VariableCoding.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds.
#   Data/Other/OECD_GDP_Per_Capita.csv.
#   Data/Other/OECD_OutOfPocket_Health_Share.csv.
#   Data/Intermediate/Data_AllCountries_WithJapan_Step3.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_TableA1_VariableCoding.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Data <- load_analysis_data()


# *************************************************************
#### Section 1: Individual-level variable definitions ####
# *************************************************************


## Definition text reused verbatim from Scripts/Tables/20_Hypertension_Table2_WillingnessToPayDeterminants.R's
## RELABEL_MAP (question wording only, without that map's "(0/1)"/range
## suffixes -- those are now the computed Min/Max columns instead).
IndividualVars <- tibble::tribble(
  ~Variable,                        ~Definition,                                                                                                                    ~Coding,
  "BASE_Numeric",                   "Base risk of developing high blood pressure over 10 years, as presented to the respondent",                                     "Continuous: percentage points (30 or 55)",
  "RR",                             "Risk reduction offered (BASE% to BASE%-RR)",                                                                                     "Continuous: percentage points (10 or 20)",
  "LogBid1",                        "First (opening) bid level shown in the double-bounded dichotomous-choice sequence",                                             "Continuous: log of PPP-adjusted USD bid",
  "LogBid2",                        "Second (follow-up) bid level, contingent on the response to LogBid1",                                                           "Continuous: log of PPP-adjusted USD bid",
  "Dummy_Female",                   "Are you...? (gender)",                                                                                                          "Binary: 1 = female, 0 = otherwise",
  "LogIncome",                      "Monthly household income, with refused or unknown responses imputed before analysis",                                    "Continuous: log of PPP-adjusted monthly household income",
  "INC1_MissingDummy",              "Refused or did not know their first income question",                                                                          "Binary: 1 = missing/refused, 0 = answered",
  "EducationCategory_Dummy_High",   "Highest level of education attained",                                                                                           "Binary: 1 = high (tertiary), 0 = otherwise",
  "Certainty",                      "How certain are you of the maximum value you just indicated?",                                                                  "Ordinal: 0 (strongly uncertain) to 4 (strongly certain)",
  "MarriedDummy",                   "What is your marital status?",                                                                                                  "Binary: 1 = married, 0 = otherwise",
  "HH_PersonsNumeric",              "Number of people living in the household",                                                                                     "Count: 1 to 7",
  "EmployDummy_Employ",             "Current employment status",                                                                                                    "Binary: 1 = employed (incl. self-employed/military), 0 = otherwise",
  "Health_HadHBP",                  "Has a doctor ever told you that you had high blood pressure?",                                                                  "Binary: 1 = yes, 0 = no",
  "HBP_RelativeRiskHigh",           "Risk of developing high blood pressure compared to people of your age and gender is larger",                                    "Binary: 1 = larger, 0 = otherwise",
  "HBP_Genetic",                    "Is high blood pressure a genetic condition in your family?",                                                                    "Binary: 1 = yes, 0 = no",
  "HBP_Healthier",                  "When choosing between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle instead of paying?", "Binary: 1 = yes, 0 = no",
  "HBP_Family",                     "Do you know of people close to you (family, friends) who have, or have had, high blood pressure?",                              "Binary: 1 = yes, 0 = no",
  "Health_Relative",                "Self-reported health relative to others of their age and gender in their country",                                              "Ordinal: 1 (well below average) to 5 (well above average); 0 = not answered",
  "Health_Drink",                   "How many alcoholic drinks do you usually have in a normal week?",                                                                "Ordinal: 1 = non-drinker, 2 = 1-4/week, 3 = 5-9/week, 4 = >9/week; 0 = not answered",
  "Health_Lifestyle",               "Do you consider yourself to have a healthy lifestyle (exercise regularly, eat healthily, sleep more than 7 hours a night)?",     "Ordinal: 0 (strongly disagree) to 4 (strongly agree)",
  "resp_age",                       "Respondent age",                                                                                                                "Continuous: years"
)


stopifnot("Every individual-level variable must exist in the analysis data" =
            all(IndividualVars$Variable %in% names(Data)))


IndividualVars$Source <- "SWACHE survey"


IndividualStats <- lapply(IndividualVars$Variable, function(v) {
  x <- Data[[v]]
  data.frame(Variable = v,
             Min  = round(min(x, na.rm = TRUE), 3),
             Mean = round(mean(x, na.rm = TRUE), 3),
             SD   = round(stats::sd(x, na.rm = TRUE), 3),
             Max  = round(max(x, na.rm = TRUE), 3))
}) %>% dplyr::bind_rows()


IndividualTable <- IndividualVars %>%
  dplyr::left_join(IndividualStats, by = "Variable") %>%
  dplyr::select(Variable, Definition, Source, Coding, Min, Mean, SD, Max)


# *************************************************************
#### Section 2: Survey design and sample (country dummies, weight, timing) ####
# *************************************************************


Weight_Columns <- unname(Country_Weight_Columns)


## Pre-screen merged file: per-country starting counts and the response-timing
## variables (both dropped from the analysis file by Setup/10_). Composite key
## Country x Respondent_Serial (Respondent_Serial alone is not unique).
PreScreen <- here("Data", "Intermediate", "Data_AllCountries_WithJapan_Step3.csv") %>%
  fread()


stopifnot(
  "Pre-screen file must have the 12,182 merged responses" = nrow(PreScreen) == 12182,
  "timing columns present" =
    all(c("Speeders_Survey_CompletionTime", "Speeders_Valuation_CompletionTime") %in% names(PreScreen)))


KEY <- c("Country", "Respondent_Serial")


Timing <- merge(
  Data[, KEY],
  PreScreen[, c(KEY, "Speeders_Survey_CompletionTime", "Speeders_Valuation_CompletionTime"), with = FALSE],
  by = KEY, all.x = TRUE)


stopifnot("every analysis respondent matched a pre-screen row" =
            nrow(Timing) == nrow(Data) && !anyNA(Timing))


StartN <- as.integer(table(factor(PreScreen$Country, levels = Report_Countries)))


FinalN <- as.integer(table(factor(Data$Country,      levels = Report_Countries)))


names(StartN) <- names(FinalN) <- Report_Countries


stat_row <- function(variable, definition, source, coding, x) {
  data.frame(
    Variable = variable, Definition = definition, Source = source, Coding = coding,
    Min  = round(min(x,  na.rm = TRUE), 3),
    Mean = round(mean(x, na.rm = TRUE), 3),
    SD   = round(stats::sd(x, na.rm = TRUE), 3),
    Max  = round(max(x,  na.rm = TRUE), 3),
    stringsAsFactors = FALSE)
}


## One row per country: the 0/1 country dummy, with the sample-flow sentence in
## the Definition column.
CountryDummyRows <- lapply(Report_Countries, function(cc) {
  dummy <- as.integer(Data$Country == cc)
  stat_row(
    cc,
    sprintf("From %s completed interviews, %s respondents remained in the analysis sample after screening for speeding and failed risk comprehension.",
            format(StartN[cc], big.mark = ","), format(FinalN[cc], big.mark = ",")),
    "SWACHE survey",
    sprintf("Binary: 1 = %s, 0 = otherwise", cc),
    dummy)
}) %>% dplyr::bind_rows()


WeightRow <- stat_row(
  "Post-stratification weight",
  paste("Ipsos post-stratification weight aligning each country sample to national population",
        "totals for gender x age, education, and geographic region by iterative proportional",
        "fitting (raking), computed country by country and efficiency-trimmed; entered in the",
        "model interacted with the country indicator."),
  "Ipsos",
  "Continuous (raking weight)",
  rowSums(Data[, Weight_Columns]))


TimingRows <- dplyr::bind_rows(
  stat_row("Survey completion time",
           "Total elapsed time to complete the questionnaire.",
           "SWACHE survey (Ipsos paradata)", "Minutes",
           Timing$Speeders_Survey_CompletionTime / 60),
  stat_row("Valuation section time",
           "Elapsed time on the valuation (double-bounded dichotomous choice) section.",
           "SWACHE survey (Ipsos paradata)", "Minutes",
           Timing$Speeders_Valuation_CompletionTime / 60))


DesignTable <- dplyr::bind_rows(CountryDummyRows, WeightRow, TimingRows)


# *************************************************************
#### Section 2b: Debriefing questions (the Table A4 screens) ####
# *************************************************************


## Each row is the indicator that Table A4 (script 27) uses to exclude
## respondents, labelled with the matching Table A4 column heading.
## The two agree-disagree screens exclude only the two disagree categories.
DISAGREE <- c("Strongly disagree", "Somewhat disagree")


DebriefTable <- dplyr::bind_rows(
  stat_row("Thought risk reduction was permanent",
           "Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?",
           "SWACHE survey", "Binary: 1 = did not answer yes, 0 = yes",
           as.integer(Data$HBP_10Yr == 0)),
  stat_row("Yea-sayer, would pay anything",
           "Please consider the statement: 'I would pay almost anything to improve my health even by a small amount'. Do you agree?",
           "SWACHE survey", "Binary: 1 = agree, 0 = otherwise",
           Data$HBP_PayAnything),
  stat_row("Did not reflect a real-life choice",
           "My survey responses reflect what I would have done in real life.",
           "SWACHE survey", "Binary: 1 = strongly or somewhat disagree, 0 = otherwise",
           as.integer(Data$A1_1 %in% DISAGREE)),
  stat_row("Did not feel informed enough",
           "The survey provided me with enough information to make informed choices.",
           "SWACHE survey", "Binary: 1 = strongly or somewhat disagree, 0 = otherwise",
           as.integer(Data$A1_2 %in% DISAGREE))
)


# *************************************************************
#### Section 3: Country-level variable definitions (n=10) ####
# *************************************************************


## Same construction as Scripts/Tables/25_Hypertension_Table3_CrossCountryRegressions.R,
## duplicated rather than sourced so this script stays self-contained.

Data$PostStratWeight <- rowSums(Data[, Weight_Columns])


Cached_Model_Path <- file.path(Report_Models, "Hypertension_Model_PooledAndCountryModels.rds")


Full_Model <- if (file.exists(Cached_Model_Path)) {
  readRDS(Cached_Model_Path)$pooled_model
} else {
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


## WHO hypertension cascade prevalence (adults 30-79, both sexes), 2024/25
## vintage, from the WHO Global report on hypertension 2025 (Annex 1 country
## profiles); the same values are used in Table 3.
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


stopifnot(nrow(Country_Data) == length(Report_Countries), !anyNA(Country_Data))


CountryVarDefs <- tibble::tribble(
  ~Variable,        ~Column,      ~Definition,                                                              ~Source,                                                          ~Coding,
  "Log GDP per capita",    "GDP_pc",     "Gross domestic product per capita",                                      "OECD National Accounts, 2023",                                  "USD, PPP-adjusted (log taken in models)",
  "Log sample income",     "Income_geo", "Weighted geometric mean of respondents' household income",              "SWACHE survey (aggregated to country level)",                   "USD, PPP-adjusted (log taken in models)",
  "OOP health expenditure share", "OOP_share", "Out-of-pocket share of current health expenditure",               "OECD Health Expenditure and Financing, SHA framework, 2022",    "Proportion, 0-1 scale",
  "Hypertension prevalence", "Prevalence", "Age-standardised hypertension prevalence, adults 30-79, both sexes",  "WHO, Global report on hypertension 2025, Annex 1 country profiles", "Proportion, 0-1 scale",
  "Mean age of sample",    "Age_Mean",   "Mean respondent age",                                                    "SWACHE survey (aggregated to country level)",                   "Years"
)


CountryStats <- lapply(seq_len(nrow(CountryVarDefs)), function(i) {
  col <- CountryVarDefs$Column[i]
  x <- Country_Data[[col]]
  data.frame(Variable = CountryVarDefs$Variable[i],
             Min  = round(min(x), 3), Mean = round(mean(x), 3),
             SD   = round(stats::sd(x), 3), Max = round(max(x), 3))
}) %>% dplyr::bind_rows()


CountryTable <- CountryVarDefs %>%
  dplyr::select(-Column) %>%
  dplyr::left_join(CountryStats, by = "Variable")


# *************************************************************
#### Section 4: Assemble and export ####
# *************************************************************


section_row <- function(label) data.frame(
  Variable = label, Definition = "", Source = "", Coding = "",
  Min = NA, Mean = NA, SD = NA, Max = NA, stringsAsFactors = FALSE)


VariableCoding <- dplyr::bind_rows(
  section_row("-- Survey design and sample (N = 9,496) --"),
  DesignTable,
  section_row("-- Individual-level (N = 9,496) --"),
  IndividualTable,
  section_row("-- Debriefing questions (N = 9,496) --"),
  DebriefTable,
  section_row("-- Country-level (N = 10) --"),
  CountryTable
)


cat("\n#### Appendix Table A1: variable coding ####\n")


print(as.data.frame(VariableCoding), row.names = FALSE)


VariableCoding %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_TableA1_VariableCoding.txt"))


cat(paste(
  "\nNote: individual-level and survey-design summary statistics computed on",
  "the baseline estimation sample (N=9,496); country-level statistics across",
  "the 10 survey countries (N=10). The per-country 'completed interviews'",
  "counts and the two response-timing rows are read from the pre-screen",
  "merged file (12,182 responses); timing is elapsed minutes and is",
  "right-skewed (a few respondents left the survey open). Individual-level",
  "survey question wording is reused from",
  "20_Hypertension_Table2_WillingnessToPayDeterminants.R's RELABEL_MAP, not",
  "re-derived, to avoid two competing sources of truth for the same wording.\n"),
  file = file.path(Report_Tables, "Hypertension_TableA1_VariableCoding.txt"), append = TRUE)


cat("\nAppendix Table A1 (variable coding) written.\n")
