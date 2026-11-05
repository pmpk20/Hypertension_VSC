#### Hypertension: Clean Japan ####
# Purpose: Clean Japanese responses, impute income and convert bids and income to the common purchasing-power basis.
# Author: Anonymised for peer review.
# Prerequisites: Run setup scripts 01–04.
# Run from the project root: source("Scripts/Setup/11_Hypertension_Setup_CleanJapan.R", encoding = "UTF-8")
# Inputs:
#   Data/SurveyData/SWACHE_Hypertension_Japan_Responses.xlsx and *_Timers.csv.
# Outputs:
#   Data/Intermediate/Data_Japan_New_Cleaned_Step2PPP.csv (purchasing-power-adjusted data).
#   Data/Intermediate/JapanData_Client_Xlsx_New_Cleaned_Step1.csv, JapanData_Main_Data_Cleaned.csv and JapanData_Main_Timers_Cleaned.csv.
#   Output/Figures/Hypertension_Diagnostic_JapanScope.png, Hypertension_Diagnostic_JapanResponseCheckA.png and Hypertension_Diagnostic_JapanResponseCheckB.png.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


library(haven)


library(here)


library(magrittr)


library(data.table)


library(tidyverse)


library(readxl)


library(DCchoice)


library(scales)


library(janitor)


# *************************************************************
#### Section 1: Path configuration ####
# *************************************************************
## Duplicated identically in every Setup script so each one can be read and run
## on its own; see Data/RAW_MANIFEST.md for the raw inputs it resolves against.

## Data/SurveyData/ holds one Responses and one Timers file per country,
## prepared from the vendor extracts listed in Data/RAW_MANIFEST.md.
## Where pipeline intermediates are written (git-ignored).
if (!dir.exists(here::here("Data", "Intermediate"))) {
  dir.create(here::here("Data", "Intermediate"), recursive = TRUE, showWarnings = FALSE)
}


#' Path to this country's Responses/Timers file under Data/SurveyData/.
SURVEY_FILE <- function(country, type = c("Responses", "Timers")) {
  type <- match.arg(type)
  ext  <- if (type == "Responses") "xlsx" else "csv"
  p <- file.path(here::here("Data", "SurveyData"),
                 sprintf("SWACHE_Hypertension_%s_%s.%s",
                         gsub(" ", "_", country, fixed = TRUE), type, ext))
  if (!file.exists(p)) {
    stop(
      "Survey data file not found:\n  ", p,
      "\nCheck that Data/SurveyData/ contains the response and timer files for all ten countries.",
      call. = FALSE
    )
  }
  p
}


#' Path to a pipeline intermediate (read or write).
WORK <- function(...) file.path(here::here("Data", "Intermediate"), ...)


# *************************************************************
#### Section 2: Recode assertions -- make silent fall-through impossible ####
# *************************************************************
## Every income/education/health recode below is exact string matching against
## multi-language survey labels (including Japanese). dplyr::case_when() with
## no .default returns NA on fall-through, and that is exactly how the Canadian
## INCCA2 defect (UK GBP labels matched against CAD data) survived to
## publication. After every recode, every observed non-missing label must
## produce a number or be a declared don't-know/refuse label; anything else is
## a hard stop.

#' Assert that a label -> value recode has no silent fall-through.
assert_no_fallthrough <- function(raw, recoded, varname, na_labels = character()) {
  stopifnot(length(raw) == length(recoded))
  raw_chr  <- as.character(raw)
  is_blank <- is.na(raw_chr) | !nzchar(trimws(raw_chr))
  bad <- !is_blank & is.na(recoded) & !(raw_chr %in% na_labels)
  if (any(bad)) {
    offenders <- sort(unique(raw_chr[bad]))
    stop(sprintf(
      paste0(
        "RECODE FALL-THROUGH in `%s`: %d row(s) carry a non-missing label that ",
        "no case_when() arm matched, so they were silently set to NA.\n",
        "Unmatched label(s) (%d):\n%s\n",
        "Either add a mapping arm or, if these really are don't-know/refuse ",
        "labels, add them to `na_labels`."
      ),
      varname, sum(bad), length(offenders),
      paste0("  [", offenders, "]  (n=",
             vapply(offenders, function(o) sum(raw_chr %in% o), integer(1)), ")",
             collapse = "\n")
    ), call. = FALSE)
  }
  invisible(TRUE)
}


#' Convenience wrapper: assert a whole family of recodes at once.
assert_recode_family <- function(data, spec) {
  for (rawcol in names(spec)) {
    s <- spec[[rawcol]]
    if (!rawcol %in% names(data)) {
      stop(sprintf("assert_recode_family: raw column `%s` not present", rawcol),
           call. = FALSE)
    }
    if (!s$out %in% names(data)) {
      stop(sprintf("assert_recode_family: recoded column `%s` not present", s$out),
           call. = FALSE)
    }
    assert_no_fallthrough(
      raw       = data[[rawcol]],
      recoded   = data[[s$out]],
      varname   = s$out,
      na_labels = if (is.null(s$na)) character() else s$na
    )
  }
  invisible(TRUE)
}


# *************************************************************
#### Section 3: Keyed income imputation (replaces the ifelse() recycling bug) ####
# *************************************************************
## ifelse(test, yes, no) evaluates rep(yes, length.out = length(test))[test].
## The predictions vector (length = number of MISSING respondents) is recycled
## across ALL rows and indexed by GLOBAL ROW POSITION, not by rank among the
## TRUEs, so every imputed income lands on the wrong respondent:
##   ifelse(c(F,T,F,T,F,T), c(100,200,300), rep(0,6))
##   #> [1]   0 200   0 100   0 300      correct: 0 100 0 200 0 300
## Fixed by a keyed join on (Country, Respondent_Serial) -- Respondent_Serial
## alone is NOT unique across waves, so the composite key is mandatory.

SWACHE_KEY <- c("Country", "Respondent_Serial")


#' Impute income by keyed join rather than positional `ifelse`.
impute_income_keyed <- function(data, model, need_mask, reported,
                                label = "", key = SWACHE_KEY) {

  missing_key <- setdiff(key, names(data))
  if (length(missing_key)) {
    stop(sprintf(
      "impute_income_keyed(%s): key column(s) missing from the data: %s.\n",
      label, paste(missing_key, collapse = ", ")),
      call. = FALSE)
  }
  stopifnot(length(need_mask) == nrow(data),
            length(reported)  == nrow(data))

  need <- !is.na(need_mask) & (need_mask == 1)

  newdata <- data[need, , drop = FALSE]
  if (!nrow(newdata)) {
    return(as.numeric(reported))
  }

  preds <- stats::predict(model, newdata = newdata)
  hit   <- match(names(preds), rownames(newdata))
  if (anyNA(hit)) {
    stop(sprintf("impute_income_keyed(%s): could not tie predictions back to rows.",
                 label), call. = FALSE)
  }

  pred_tbl <- newdata[hit, key, drop = FALSE]
  pred_tbl[[".Imputed_Income"]] <- exp(unname(preds))

  if (anyDuplicated(pred_tbl[key]) > 0) {
    dup <- pred_tbl[duplicated(pred_tbl[key]), key, drop = FALSE]
    stop(sprintf(
      "impute_income_keyed(%s): composite key (%s) is not unique among the %d ",
      label, paste(key, collapse = ", "), nrow(pred_tbl)),
      "rows to impute -- a keyed join would be ambiguous. First duplicate: ",
      paste(unlist(dup[1, ]), collapse = " / "), call. = FALSE)
  }

  base <- data[, key, drop = FALSE]
  base[[".Reported"]] <- as.numeric(reported)
  base[[".row_order"]] <- seq_len(nrow(base))

  joined <- dplyr::left_join(base, pred_tbl, by = key)

  if (nrow(joined) != nrow(data)) {
    stop(sprintf(
      "impute_income_keyed(%s): join changed the row count (%d -> %d).",
      label, nrow(data), nrow(joined)), call. = FALSE)
  }
  joined <- joined[order(joined$.row_order), , drop = FALSE]

  out <- dplyr::coalesce(joined$.Imputed_Income, joined$.Reported)

  message(sprintf(
    "  [impute] %-14s imputed %d of %d flagged rows by keyed join (%d flagged rows had missing predictors and keep their reported value).",
    label, sum(!is.na(joined$.Imputed_Income)), sum(need),
    sum(need) - sum(!is.na(joined$.Imputed_Income))))

  as.numeric(out)
}


# *************************************************************
#### Section 4: Important Data ####
# *************************************************************


JapanData_Client_Xlsx <-
  SURVEY_FILE("Japan", "Responses") %>%
  readxl::read_xlsx(col_types = "text") %>%
  data.frame()


JapanData_Timers <-
  SURVEY_FILE("Japan", "Timers") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 5: Identify speeders ####
# *************************************************************
## First survey speeders then valuation section


## First append the country from the main data to the timer data
JapanData_Timers$Country <- JapanData_Client_Xlsx$Country


# *************************************************************
#### Section 6: SURVEY SPEEDERS ####
# *************************************************************
## Calculate each persons time
## by getting each rows sum of time in seconds
JapanData_Timers$Speeders_Survey_CompletionTime <-
  JapanData_Timers[2:78] %>%
  rowSums(na.rm = TRUE)


## Now calculate and append to data the
## country specific threshold using 48% of median (for speeding)
## of 148% for too slow
JapanData_Timers <-
  JapanData_Timers %>%
  group_by(Country) %>%
  mutate("Speeders_Survey_CountryThreshold" =
           median(Speeders_Survey_CompletionTime) %>% multiply_by(0.48),
         "Slowers_Survey_CountryThreshold" =
           median(Speeders_Survey_CompletionTime) %>% multiply_by(1.48)
  )


## Recode as fail (0) or pass (10)
JapanData_Timers$Speeders_Survey_TestDummy <- ifelse(
  JapanData_Timers$Speeders_Survey_CompletionTime <=
    JapanData_Timers$Speeders_Survey_CountryThreshold, 0, 1)


## Test people going too slow
JapanData_Timers$Slowers_Survey_TestDummy <- ifelse(
  JapanData_Timers$Speeders_Survey_CompletionTime >=
    JapanData_Timers$Slowers_Survey_CountryThreshold, 0, 1)


# *************************************************************
#### Section 7: VALUATION SPEEDERS ####
# *************************************************************
## Valuation section
JapanData_Timers$Speeders_Valuation_CompletionTime <-
  JapanData_Timers[c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6a", "Q6b", "Q6c", "Q7")] %>%
  rowSums(na.rm = TRUE)


## Categorise valuation section threshold
JapanData_Timers <- JapanData_Timers %>%
  group_by(Country) %>%
  mutate("Speeders_Valuation_CountryThreshold" =
           median(Speeders_Valuation_CompletionTime) %>% multiply_by(0.48),
         "Slowers_Valuation_CountryThreshold" =
           median(Speeders_Valuation_CompletionTime) %>% multiply_by(1.48))


## Recode as fail (0) or pass (10)
JapanData_Timers$Speeders_Valuation_TestDummy <- ifelse(
  JapanData_Timers$Speeders_Valuation_CompletionTime <=
    JapanData_Timers$Speeders_Valuation_CountryThreshold, 0, 1)


JapanData_Timers$Slowers_Valuation_TestDummy <- ifelse(
  JapanData_Timers$Speeders_Valuation_CompletionTime >=
    JapanData_Timers$Slowers_Valuation_CountryThreshold, 0, 1)


# *************************************************************
#### Section 8: Append variables ####
# *************************************************************
JapanData_Client_Xlsx$Speeders_Survey_TestDummy <- JapanData_Timers$Speeders_Survey_TestDummy


JapanData_Client_Xlsx$Slowers_Survey_TestDummy <- JapanData_Timers$Slowers_Survey_TestDummy


JapanData_Client_Xlsx$Speeders_Survey_CountryThreshold <- JapanData_Timers$Speeders_Survey_CountryThreshold


JapanData_Client_Xlsx$Slowers_Survey_CountryThreshold <- JapanData_Timers$Slowers_Survey_CountryThreshold


JapanData_Client_Xlsx$Speeders_Survey_CompletionTime <- JapanData_Timers$Speeders_Survey_CompletionTime


JapanData_Client_Xlsx$Speeders_Valuation_TestDummy <- JapanData_Timers$Speeders_Valuation_TestDummy


JapanData_Client_Xlsx$Slowers_Valuation_TestDummy <- JapanData_Timers$Slowers_Valuation_TestDummy


JapanData_Client_Xlsx$Speeders_Valuation_CountryThreshold <- JapanData_Timers$Speeders_Valuation_CountryThreshold


JapanData_Client_Xlsx$Slowers_Valuation_CountryThreshold <- JapanData_Timers$Slowers_Valuation_CountryThreshold


JapanData_Client_Xlsx$Speeders_Valuation_CompletionTime <- JapanData_Timers$Speeders_Valuation_CompletionTime


# *************************************************************
#### Section 9: SUMMARY ####
# *************************************************************
## Output survey speeder data
JapanData_Timers %>%
  group_by(Country) %>%
  summarise(
    Threshold = first(Speeders_Valuation_CountryThreshold),

    N_Survey_Fail = paste0(
      sum(Speeders_Survey_TestDummy == 0),
      " (", mean(Speeders_Survey_TestDummy == 0) %>%
        round(2) %>%
        multiply_by(100), "%)"),

    N_Survey_Pass = paste0(
      sum(Speeders_Survey_TestDummy == 1),
      " (", mean(Speeders_Survey_TestDummy == 1) %>%
        round(2) %>%
        multiply_by(100), "%)"),

    N_Valuation_Fail = paste0(
      sum(Speeders_Valuation_TestDummy == 0),
      " (", mean(Speeders_Valuation_TestDummy == 0) %>%
        round(2) %>%
        multiply_by(100), "%)"),

    N_Valuation_Pass = paste0(
      sum(Speeders_Valuation_TestDummy == 1),
      " (", mean(Speeders_Valuation_TestDummy == 1) %>%
        round(2) %>%
        multiply_by(100), "%)")
  ) %>%
  write.csv(quote = FALSE)


# *************************************************************
#### Section 10: Data cleaning and sorting for later ####
# *************************************************************


## Recategorise so that pass  = 1
JapanData_Client_Xlsx$ProbabilityTest_Numeric <- ifelse(
  JapanData_Client_Xlsx$T1 ==
    "The risk of getting a cold is higher in group A",
  0 ,
  1)


JapanData_Client_Xlsx$ProbabilityTest_Categorical <- ifelse(
  JapanData_Client_Xlsx$T1 ==
    "The risk of getting a cold is higher in group A",
  "Fail",
  "Pass")


## From character to number
JapanData_Client_Xlsx$Kids_Numeric <- JapanData_Client_Xlsx$KIDS02 %>% as.numeric()


## Categorising partner or no
JapanData_Client_Xlsx$MarriedDummy <- ifelse(
  !JapanData_Client_Xlsx$S4 %in%
    c("Not in a relationship",
      "Other"),
  1,
  0)


JapanData_Client_Xlsx$EmployDummy_Employed <-
  ifelse(JapanData_Client_Xlsx$EMP01 %in% c("Employed full-time",
                              "Self employed"),
         1,
         0)


JapanData_Client_Xlsx$EmployDummy_Retired <-
  ifelse(JapanData_Client_Xlsx$EMP01 %in% c("Retired"),
         1,
         0)


JapanData_Client_Xlsx$EmployDummy_PT <-
  ifelse(JapanData_Client_Xlsx$EMP01 %in% c("Employed part-time"),
         1,
         0)


JapanData_Client_Xlsx$Dummy_Female <-
  ifelse(JapanData_Client_Xlsx$GENDER_NonBinary %in% c("Female"),
         1,
         0)


JapanData_Client_Xlsx$HH_PersonsNumeric <-
  ifelse(JapanData_Client_Xlsx$SE3 %in% c("7 or more"),
         7,
         JapanData_Client_Xlsx$SE3) %>% as.numeric()


# Age group recoding
JapanData_Client_Xlsx$Age_1829Dummy <-
  ifelse(JapanData_Client_Xlsx$AgeGroup %in% c("18-29"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$Age_3044Dummy <-
  ifelse(JapanData_Client_Xlsx$AgeGroup %in% c("30-44"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$Age_4560Dummy <-
  ifelse(JapanData_Client_Xlsx$AgeGroup %in% c("45-60"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$Age_60Dummy <-
  ifelse(JapanData_Client_Xlsx$AgeGroup %in% c("60+"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeGroup_Numeric <- dplyr::recode(JapanData_Client_Xlsx$AgeGroup,
                                          '18-29' = 23.50,
                                          '30-44' = 37,
                                          '45-60' = 52.5,
                                          '60+' = 60)


# BY QUOTA ********************************************************
JapanData_Client_Xlsx$AgeQuota_1826 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("18-26"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeQuota_2734 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("27-34"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeQuota_3539 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("35-39"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeQuota_4044 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("40-44"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeQuota_4559 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("45-59"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeQuota_6065 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("60-65"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$AgeQuota_65 <-
  ifelse(JapanData_Client_Xlsx$AgeQuota %in% c("65+"),
         1,
         0) %>% as.numeric()


JapanData_Client_Xlsx$Age_1829Dummy_Adjusted <- JapanData_Client_Xlsx$Age_1829Dummy + 1


JapanData_Client_Xlsx$Age_3044Dummy_Adjusted <- JapanData_Client_Xlsx$Age_3044Dummy + 1


JapanData_Client_Xlsx$Age_4560Dummy_Adjusted <- JapanData_Client_Xlsx$Age_4560Dummy + 1


JapanData_Client_Xlsx$Age_60Dummy_Adjusted <- JapanData_Client_Xlsx$Age_60Dummy + 1


# EDUCATION DUMMIES ********************************************************


JapanData_Client_Xlsx$Dummy_HighEducation_JP <-
  ifelse(JapanData_Client_Xlsx$ISCED == "High",
         1,
         0)


JapanData_Client_Xlsx$EDU_All <-
  ifelse(
    JapanData_Client_Xlsx$Dummy_HighEducation_JP == 1,
    1, 0
  )


JapanData_Client_Xlsx$RED_Numeric <- JapanData_Client_Xlsx$RED %>% as.numeric()


## Dummy categorise education levels
JapanData_Client_Xlsx$EducationCategory <- JapanData_Client_Xlsx$ISCED


JapanData_Client_Xlsx$EducationCategory_Dummy_High <- ifelse(
  JapanData_Client_Xlsx$ISCED == "High", 1, 0)


JapanData_Client_Xlsx$EducationCategory_Dummy_Medium <- ifelse(
  JapanData_Client_Xlsx$ISCED == "Medium", 1, 0)


JapanData_Client_Xlsx$EducationCategory_Dummy_Low <- ifelse(
  JapanData_Client_Xlsx$ISCED == "Low", 1, 0)


# *************************************************************
#### Section 11: Recoding health questions ####
# *************************************************************


# How much do you agree or disagree with the following statements? - My survey responses reflect what I would have done in real life.
JapanData_Client_Xlsx$Response_RealLife <- dplyr::recode(
  JapanData_Client_Xlsx$A1_1,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - The survey provided me with enough information to make informed choices.
JapanData_Client_Xlsx$Response_Information <- dplyr::recode(
  JapanData_Client_Xlsx$A1_2,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - I find the description of high blood pressure provided in this survey to be accurate
JapanData_Client_Xlsx$Response_Accurate <- dplyr::recode(
  JapanData_Client_Xlsx$A1_3,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How confident are you that the information that has been provided in this survey is correct?
JapanData_Client_Xlsx$Response_Confident <- dplyr::recode(
  JapanData_Client_Xlsx$A2,
  "Very confident" = 4,
  "Somewhat confident"                 = 3,
  "Neither doubtful nor confident"  = 2,
  "Somewhat doubtful" = 1,
  "Very doubtful"   = 0
)


# Q02 and DX
## Yes if heard of high blood pressure, zero if not
JapanData_Client_Xlsx$Health_HeardOfHBP <- ifelse(
  JapanData_Client_Xlsx$Q02 == "Yes", 1, 0)


## Yes if they had high blood pressure
JapanData_Client_Xlsx$Health_HadHBP <- ifelse(
  JapanData_Client_Xlsx$Q03 == "Yes", 1, 0)


# Before answering this survey, did you know about the health risks associated with untreated high blood pressure?
JapanData_Client_Xlsx$Health_HBP_D1 <- ifelse(
  JapanData_Client_Xlsx$D1 == "No, I did not know", 0, 1)


# Before answering this survey, did you know that high blood pressure could be treated by having a healthier lifestyle or medical treatments?
JapanData_Client_Xlsx$Health_HBP_D2 <- ifelse(
  JapanData_Client_Xlsx$D2 == "No, I did not know", 1, 0)


## Diagnosed and stuck to it
JapanData_Client_Xlsx$Health_HBP_DiagnosedStuck <- ifelse(
  JapanData_Client_Xlsx$D2 == "Yes", 1, 0)


JapanData_Client_Xlsx$Health_HBP_SideEffects <- ifelse(
  JapanData_Client_Xlsx$D2 == "Yes", 1, 0)


# Dummy coding knowledge of risks
## Can do faster with %>% mutate() but more legible here
## Recoding Q04 here
JapanData_Client_Xlsx$Risks_Genetic <- ifelse(
  JapanData_Client_Xlsx$Q04_1 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Obesity <- ifelse(
  JapanData_Client_Xlsx$Q04_2 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Alcohol <- ifelse(
  JapanData_Client_Xlsx$Q04_3 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Exposure <- ifelse(
  JapanData_Client_Xlsx$Q04_4 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Insulin <- ifelse(
  JapanData_Client_Xlsx$Q04_5 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Aging <- ifelse(
  JapanData_Client_Xlsx$Q04_6 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Exercise <- ifelse(
  JapanData_Client_Xlsx$Q04_7 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Smoking <- ifelse(
  JapanData_Client_Xlsx$Q04_8 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Stress <- ifelse(
  JapanData_Client_Xlsx$Q04_9 == "Yes", 1, 0)


JapanData_Client_Xlsx$Risks_Deprive <- ifelse(
  JapanData_Client_Xlsx$Q04_10 == "Yes", 1, 0)


# Recoding H series Qs


# How do you rate your risk of developing high blood pressure compared to people of your age and gender?
JapanData_Client_Xlsx$HBP_RelativeRiskHigh <- ifelse(
  JapanData_Client_Xlsx$H2 %in%
    c("My risk is clearly larger",
      "My risk is somewhat larger"),
  1, 0)


# Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?
JapanData_Client_Xlsx$HBP_10Yr <- ifelse(
  JapanData_Client_Xlsx$H3 == "Yes",
  1, 0)


# When you choose between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle to reduce your risk instead of paying?
JapanData_Client_Xlsx$HBP_Healthier <- ifelse(
  JapanData_Client_Xlsx$H4 == "Yes",
  1, 0)


# Please consider the statement: “I would pay almost anything to improve my health even by a small amount”. Do you…
JapanData_Client_Xlsx$HBP_PayAnything <- ifelse(
  JapanData_Client_Xlsx$H5 == "…agree",
  1, 0)


# Is high blood pressure a genetic condition in your family?
JapanData_Client_Xlsx$HBP_Genetic <- ifelse(
  JapanData_Client_Xlsx$H6 == "Yes",
  1, 0)


# Do you know of people close to you (family, friends) who have, or have had, high blood pressure?
JapanData_Client_Xlsx$HBP_Family <- ifelse(
  JapanData_Client_Xlsx$H7 == "Yes",
  1, 0)


# I have been diagnosed with: - [disease name]
JapanData_Client_Xlsx$Diagnosed_KidneyDisease <- ifelse(JapanData_Client_Xlsx$H8_1 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_HeartDisease <- ifelse(JapanData_Client_Xlsx$H8_2 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_Obesity <- ifelse(JapanData_Client_Xlsx$H8_3 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_Diabetes <- ifelse(JapanData_Client_Xlsx$H8_4 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_ParkinsonDisease <- ifelse(JapanData_Client_Xlsx$H8_5 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_AlzheimersDementias <- ifelse(JapanData_Client_Xlsx$H8_6 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_RheumatoidArthritis <- ifelse(JapanData_Client_Xlsx$H8_7 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_Respiratory <- ifelse(JapanData_Client_Xlsx$H8_8 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_Other <- ifelse(JapanData_Client_Xlsx$H8_9 == "Yes", 1, 0)


JapanData_Client_Xlsx$Diagnosed_None <- ifelse(JapanData_Client_Xlsx$H8_10 == "Yes", 1, 0)


# How would you describe your health in general?
JapanData_Client_Xlsx$Health_Own <- dplyr::recode(
  JapanData_Client_Xlsx$H9,
  "Excellent" = 5,
  "Good" = 4,
  "Fair" = 3,
  "Poor" = 2,
  "Very poor" = 1,
  "Prefer not to answer" = 0
)


# How would you describe your health relative to others of your age and gender in your country?
JapanData_Client_Xlsx$Health_Relative <- dplyr::recode(
  JapanData_Client_Xlsx$H10,
  "Well above average" = 5,
  "Above average" = 4,
  "Average" = 3,
  "Below average" = 2,
  "Well below average" = 1,
  "Prefer not to answer" = 0
)


## Classify
JapanData_Client_Xlsx$Health_Relative_BelowAverage <- ifelse(JapanData_Client_Xlsx$Health_Relative == 2, 1, 0)


JapanData_Client_Xlsx$Health_Relative_AboveAverage <- ifelse(JapanData_Client_Xlsx$Health_Relative == 4, 1, 0)


# How would you describe your weight relative to others of your height, age and gender in your country?
JapanData_Client_Xlsx$Health_Weight <- dplyr::recode(
  JapanData_Client_Xlsx$H11,
  "Less than average" = 3,
  "Average" = 2,
  "More than average" = 1,
  "Prefer not to answer" = 0
)


# Do you smoke?
JapanData_Client_Xlsx$Health_Smoke <- dplyr::recode(
  JapanData_Client_Xlsx$H12,
  "No" = 3,
  "Yes, occasionally" = 2,
  "Yes, regularly" = 1,
  "Prefer not to answer" = 0
)


# How many alcoholic drinks do you usually have in a normal week?
JapanData_Client_Xlsx$Health_Drink  <- dplyr::recode(
  JapanData_Client_Xlsx$H13,
  "I usually don’t drink alcohol" = 1,
  "1-4 drinks per week" = 2,
  "5-9 drinks per week" = 3,
  "More than 9 drinks per week" = 4,
  "999999" = 0
)


# How often do you exercise in a normal week? Please pick the answer that best describes a normal week.
JapanData_Client_Xlsx$Health_Exercise  <- dplyr::recode(
  JapanData_Client_Xlsx$H14,
  "More than 5 times per week" = 5,
  "3-5 times per week"       = 4,
  "1-2 times per week" = 3,
  "Less than 1 time per week" = 2,
  "I don’t exercise" = 1,
  "Prefer not to answer"     = 0)


# Do you use sunscreen?
JapanData_Client_Xlsx$Health_Sunscreen <- dplyr::recode(
  JapanData_Client_Xlsx$H15,
  "Yes, regularly" = 3,
  "Yes, occasionally" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Do you consider yourself to have a healthy lifestyle (exercise regularly, eat healthily, sleep more than 7 hours a night...)?
JapanData_Client_Xlsx$Health_Lifestyle <- dplyr::recode(
  JapanData_Client_Xlsx$H16,
  "Yes" = 4,
  "In general yes" = 3,
  "In general no" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Take a moment to think about the products you typically purchase. How do you feel about them?
JapanData_Client_Xlsx$Health_PurchaseProducts <- dplyr::recode(
  JapanData_Client_Xlsx$H17,
  "Totally safe for people’s health" = 4 ,
  "Rather safe for people’s health" = 3 ,
  "I don’t know" = 2,
  "Rather unsafe for people’s health" = 1,
  "Totally unsafe for people’s health" = 0
)


# Which of the following statements best describes your purchasing behaviour?
JapanData_Client_Xlsx$Health_PurchaseBehaviour <- dplyr::recode(
  JapanData_Client_Xlsx$H18,
  "I try to buy safer products even if it comes at an extra cost (e.g. by choosing safer products even if they are a bit mo" = 3,
  "I try to buy safer products if it doesn’t come at an extra cost (e.g. by choosing safer products if they are similar i" = 2,
  "I don’t know" = 1,
  "I do not pay much attention to the products’ effect on my health" = 0
)


# How would you describe your knowledge of high blood pressure before having taken this survey?
JapanData_Client_Xlsx$Response_HBP_Before <- dplyr::recode(
  JapanData_Client_Xlsx$A3,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
JapanData_Client_Xlsx$Response_HBP_After <- dplyr::recode(
  JapanData_Client_Xlsx$A4,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


JapanData_Client_Xlsx$Response_HBP_Change <-
  (JapanData_Client_Xlsx$Response_HBP_After -
     JapanData_Client_Xlsx$Response_HBP_Before)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
JapanData_Client_Xlsx$Health_Cost <- ifelse(
  JapanData_Client_Xlsx$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket",
      "A private health insurance scheme (e.g. through your employer) would cover all or most of the costs" ),
  1, 0)


## Own pocked specifically
JapanData_Client_Xlsx$Health_Cost_OwnPocket <- ifelse(
  JapanData_Client_Xlsx$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket"),
  1, 0)


## SE2 categorises nicely but let's add a dummy
JapanData_Client_Xlsx$Urban <- ifelse(
  JapanData_Client_Xlsx$SE2 == "Urban",
  1, 0)


# How certain are you of the maximum value you just indicated?
JapanData_Client_Xlsx$Certainty <- dplyr::recode(
  JapanData_Client_Xlsx$Q7,
  "Strongly certain" = 4,
  "Somewhat certain" = 3,
  "Neither certain nor uncertain" = 2,
  "Somewhat uncertain" = 1,
  "Strongly uncertain" = 0
)


## Define here for later
JapanData_Client_Xlsx$Spike <- ifelse(
  JapanData_Client_Xlsx$Q4 %in% c("No, I would not pay anything"),
  0, 1
)


# Debriefing questions **********************************************************************************
# Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?
JapanData_Client_Xlsx$HBP_10Yr <- ifelse(
  JapanData_Client_Xlsx$H3 == "Yes",
  1, 0)


# When you choose between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle to reduce your risk instead of paying?
JapanData_Client_Xlsx$HBP_Healthier <- ifelse(
  JapanData_Client_Xlsx$H4 == "Yes",
  1, 0)


# Please consider the statement: “I would pay almost anything to improve my health even by a small amount”. Do you…
JapanData_Client_Xlsx$HBP_PayAnything <- ifelse(
  JapanData_Client_Xlsx$H5 == "…agree",
  1, 0)


# How much do you agree or disagree with the following statements? - My survey responses reflect what I would have done in real life.
JapanData_Client_Xlsx$Response_RealLife <- dplyr::recode(
  JapanData_Client_Xlsx$A1_1,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - The survey provided me with enough information to make informed choices.
JapanData_Client_Xlsx$Response_Information <- dplyr::recode(
  JapanData_Client_Xlsx$A1_2,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - I find the description of high blood pressure provided in this survey to be accurate
JapanData_Client_Xlsx$Response_Accurate <- dplyr::recode(
  JapanData_Client_Xlsx$A1_3,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How confident are you that the information that has been provided in this survey is correct?
JapanData_Client_Xlsx$Response_Confident <- dplyr::recode(
  JapanData_Client_Xlsx$A2,
  "Very confident" = 4,
  "Somewhat confident"                 = 3,
  "Neither doubtful nor confident"  = 2,
  "Somewhat doubtful" = 1,
  "Very doubtful"   = 0
)


# How would you describe your knowledge of high blood pressure before having taken this survey?
JapanData_Client_Xlsx$Response_HBP_Before <- dplyr::recode(
  JapanData_Client_Xlsx$A3,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
JapanData_Client_Xlsx$Response_HBP_After <- dplyr::recode(
  JapanData_Client_Xlsx$A4,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


# *************************************************************
#### Section 12: Cleaning and adjusting income ####
# *************************************************************


# Recoding **************************************
JapanData_Client_Xlsx$Question1 <-
  ifelse(
    JapanData_Client_Xlsx$Q1 == "Current risk",
    0, ## Vote no
    1) ## Vote yes


JapanData_Client_Xlsx$Q2_Missing <- ifelse(JapanData_Client_Xlsx$Q2 %>% is.na(),
                                           NA, ## is.na = true so code as na
                                           JapanData_Client_Xlsx$Q2) ## put answer if not na


JapanData_Client_Xlsx$Q3_Missing <- ifelse(JapanData_Client_Xlsx$Q3 %>% is.na(),
                                           NA,
                                           JapanData_Client_Xlsx$Q3)


JapanData_Client_Xlsx$FollowUp <- coalesce(JapanData_Client_Xlsx$Q2_Missing, JapanData_Client_Xlsx$Q3_Missing)


JapanData_Client_Xlsx$FollowUp_Numeric <- ifelse(JapanData_Client_Xlsx$FollowUp == "Current risk",
                                                 0,
                                                 1)


JapanData_Client_Xlsx$Question2 <- JapanData_Client_Xlsx$FollowUp_Numeric


## Remove currency signs and convert to numeric
JapanData_Client_Xlsx$Bid1_Numeric <-
  JapanData_Client_Xlsx$BID1 %>%
  gsub(x = .,
       pattern = " 円",
       replacement = "") %>%
  gsub(x = .,
       pattern = ",",
       replacement = "") %>%
  as.numeric()


## DBDC second bound bid level
JapanData_Client_Xlsx$FollowUp_Bid <-
  ifelse(JapanData_Client_Xlsx$Q2_Missing %>% is.na(),
         JapanData_Client_Xlsx$BID3,
         JapanData_Client_Xlsx$BID2) %>%
  gsub(x = .,
       pattern = " 円",
       replacement = "") %>%
  gsub(x = .,
       pattern = ",",
       replacement = "") %>%
  as.numeric()


## Originally character for reasons
JapanData_Client_Xlsx$BASE_Numeric <- JapanData_Client_Xlsx$BASE %>% as.numeric()


JapanData_Client_Xlsx$RED_Numeric <- JapanData_Client_Xlsx$RED %>% as.numeric()


JapanData_Client_Xlsx$DBDC <- ifelse(
  (JapanData_Client_Xlsx$Question1 == 0) & (JapanData_Client_Xlsx$FollowUp_Numeric == 0),
  "NN",
  ifelse(
    (JapanData_Client_Xlsx$Question1 == 0) & (JapanData_Client_Xlsx$FollowUp_Numeric == 1),
    "NY",
    ifelse(
      (JapanData_Client_Xlsx$Question1 == 1) & (JapanData_Client_Xlsx$FollowUp_Numeric == 0),
      "YN",
      ifelse(
        (JapanData_Client_Xlsx$Question1 == 1) & (JapanData_Client_Xlsx$FollowUp_Numeric == 1),
        "YY",
        1))))


JapanData_Client_Xlsx$INCJP1_MissingDummy <- ifelse(
  JapanData_Client_Xlsx$INCJP1 %in%
    c("とは言いたくない。",
      "分からないよ。"
    ),
  1,
  0)


JapanData_Client_Xlsx$INCJP2_MissingDummy <- ifelse(
  JapanData_Client_Xlsx$INCJP2 %in%
    c("とは言いたくない。",
      "分からないよ。"
    ),
  1,
  0)


# Income recoding for income question 2 -------------------------------------------------------------

JapanData_Client_Xlsx_Backup <- JapanData_Client_Xlsx


## Very large recoding exercise taking the midpoints of each range
JapanData_Client_Xlsx <- JapanData_Client_Xlsx %>%
  dplyr::mutate(

    INCJP1_Numeric = dplyr::case_when(
      is.na(INCJP1) ~ NA,
      INCJP1 %in% "0円〜67,000円" ~ (0.5 * 0) + (0.5 * 67000),
      INCJP1 %in% "101,001円〜123,000円" ~ (0.5 * 101001) + (0.5 * 123000),
      INCJP1 %in% "123,001円〜151,000円" ~ (0.5 * 123001) + (0.5 * 151000),
      INCJP1 %in% "151,001円〜175,000円" ~ (0.5 * 151001) + (0.5 * 175000),
      INCJP1 %in% "175,001円〜204,000円" ~ (0.5 * 175001) + (0.5 * 204000),
      INCJP1 %in% "204,001円〜238,000円" ~ (0.5 * 204001) + (0.5 * 238000),
      INCJP1 %in% "238,001円〜279,000円" ~ (0.5 * 238001) + (0.5 * 279000),
      INCJP1 %in% "279,001円〜350,000円" ~ (0.5 * 279001) + (0.5 * 350000),
      INCJP1 %in% "350,001円〜431,000円" ~ (0.5 * 350001) + (0.5 * 431000),
      INCJP1 %in% "67,001円〜101,000円" ~ (0.5 * 67001) + (0.5 * 101000),
      INCJP1 %in% "431,001円以上" ~ 431001,
      INCJP1 %in% "とは言いたくない。" ~ NA,
      INCJP1 %in% "分からないよ。" ~ NA),

    INCJP2_Numeric = dplyr::case_when(
      is.na(INCJP2) ~ NA,
      INCJP2 %in% "0円〜101,000円" ~ (0.5* 0) + (0.5 * 101000),
      INCJP2 %in% "101,001円〜151,000円" ~ (0.5 * 101001) + (0.5 * 151000),
      INCJP2 %in% "151,001円〜204,000円" ~ (0.5 * 151001) + (0.5 * 204000),
      INCJP2 %in% "204,001円〜279,000円" ~ (0.5 * 204001) + (0.5 * 279000),
      INCJP2 %in% "279,001円〜431,000円" ~ (0.5 * 279001) + (0.5 * 431000),
      INCJP2 %in% "431,001円以上" ~ 431001,
      INCJP2 %in% "とは言いたくない。" ~ NA,
      INCJP2 %in% "分からないよ。" ~ NA)
    )


#
# ## Coalesce by country: fast way
# JapanData_Client_Xlsx$INC1_All <- coalesce(
#   JapanData_Client_Xlsx$INCJP1_Numeric
# )
#
# JapanData_Client_Xlsx$INC1_Missing <- coalesce(
#   JapanData_Client_Xlsx$INCJP1_MissingDummy
# )
#
# ## Coalesce by country: fast way
# JapanData_Client_Xlsx$INC2_All <- coalesce(
#   JapanData_Client_Xlsx$INCJP2_Numeric
# )
#
# JapanData_Client_Xlsx$INC2_Missing <- coalesce(
#   JapanData_Client_Xlsx$INCJP2_MissingDummy
# )


## TASK E -- assert every observed non-missing Japanese income label got a
## mapping.  Japanese labels are the highest-risk of the lot for encoding drift.
NA_LABELS_JP <- c("とは言いたくない。",
                  "分からないよ。")


assert_recode_family(JapanData_Client_Xlsx, list(
  INCJP1 = list(out = "INCJP1_Numeric", na = NA_LABELS_JP),
  INCJP2 = list(out = "INCJP2_Numeric", na = NA_LABELS_JP)
))


message("  [assert] 08_: both Japanese income recodes are complete.")


## Coalesce by country: fast way
JapanData_Client_Xlsx$INCJP_All <- coalesce(
  JapanData_Client_Xlsx$INCJP1_Numeric,
  JapanData_Client_Xlsx$INCJP2_Numeric
)


JapanData_Client_Xlsx$INCJP_All_MissingDummy <- JapanData_Client_Xlsx$INCJP2_MissingDummy


JapanData_Client_Xlsx$INC1_All <- JapanData_Client_Xlsx$INCJP_All


# Japan
## Predict missing income
Model_PredictMissingIncome_JP <- lm(
  formula = log(INCJP_All) ~
    AgeGroup +
    Dummy_HighEducation_JP +
    MarriedDummy +
    HH_PersonsNumeric +
    EmployDummy_Retired +
    EmployDummy_PT +
    EmployDummy_Employed,
  data = JapanData_Client_Xlsx[JapanData_Client_Xlsx$Country == "Japan", ]
)


## Match imputed income by Country and Respondent_Serial, not row position.
JapanData_Client_Xlsx$INCJP1_Numeric_PlusMissing <- impute_income_keyed(
  data      = JapanData_Client_Xlsx,
  model     = Model_PredictMissingIncome_JP,
  need_mask = JapanData_Client_Xlsx$Country == "Japan" &
              JapanData_Client_Xlsx$INCJP2_MissingDummy == 1,
  reported  = JapanData_Client_Xlsx$INC1_All,
  label     = "Japan")


JapanData_Client_Xlsx$INCAT1_Numeric <- 0


JapanData_Client_Xlsx$INCUK1_Numeric <- 0


JapanData_Client_Xlsx$INCCA1_Numeric <- 0


JapanData_Client_Xlsx$INCSI1_Numeric <- 0


JapanData_Client_Xlsx$INCBE1_Always_Numeric <- 0


JapanData_Client_Xlsx$INCAT2_Numeric <- 0


JapanData_Client_Xlsx$INCUK2_Numeric <- 0


JapanData_Client_Xlsx$INCCA2_Numeric <- 0


JapanData_Client_Xlsx$INCSI2_Numeric <- 0


JapanData_Client_Xlsx$INCBE2_Always_Numeric <- 0


JapanData_Client_Xlsx$INCUK1_Numeric_PlusMissing <- 0


JapanData_Client_Xlsx$INCBE1_Always_Numeric_PlusMissing <- 0


JapanData_Client_Xlsx$INCAT1_Numeric_PlusMissing <- 0


JapanData_Client_Xlsx$INCSI1_Numeric_PlusMissing <- 0


JapanData_Client_Xlsx$INCCA1_Numeric_PlusMissing <- 0


JapanData_Client_Xlsx$INCUK1_Numeric_PlusMissing_Log <- 0


JapanData_Client_Xlsx <- JapanData_Client_Xlsx %>%
  dplyr::mutate(
    BIDTOT1_Numeric = gsub("[^0-9.]", "", BIDTOT1) %>% as.numeric(),
    BIDTOT2_Numeric = gsub("[^0-9.]", "", BIDTOT2) %>% as.numeric(),
    BIDTOT3_Numeric = gsub("[^0-9.]", "", BIDTOT3) %>% as.numeric())


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
JapanData_Client_Xlsx %>%
  fwrite(sep = ",",
         WORK("JapanData_Client_Xlsx_New_Cleaned_Step1.csv"))


JapanData_Timers %>%
  fwrite(sep = ",",
         WORK("JapanData_Main_Timers_Cleaned.csv"))


## OLD Factors from OECD
# ConversionFactors <- data.frame(
#   "Austria" = 0.803598,
#   "Belgium" = 0.796006,
#   "Canada" = 1.281466,
#   "Finland" = 0.872997,
#   "Greece" = 0.576645,
#   "Japan" = 105.200318,
#   "Portugal" = 0.590032,
#   "Slovenia" = 0.587038,
#   "United Kingdom" = 0.747639,
#   "United States" =	1
# )


## 2023 Factors from OECD
Conversion_Factors <- data.frame(
  "Austria" = 0.72035,
  "Belgium" = 0.719916,
  "Canada" = 1.163492,
  "Finland" = 0.790539,
  "Greece" = 0.520812,
  "Japan" = 99.211289,
  "Portugal" = 0.540472,
  "Slovenia" = 0.537477,
  "United Kingdom" = 0.668934,
  "United States" = 1
)


## divide bid level by country-specific PPP factor
JapanData_Client_Xlsx$BIDTOT1_PPP <- JapanData_Client_Xlsx$BIDTOT1_Numeric %>%
  divide_by(Conversion_Factors$Japan)


JapanData_Client_Xlsx$BIDTOT2_PPP <- JapanData_Client_Xlsx$BIDTOT2_Numeric %>%
  divide_by(Conversion_Factors$Japan)


JapanData_Client_Xlsx$BIDTOT3_PPP <- JapanData_Client_Xlsx$BIDTOT3_Numeric %>%
  divide_by(Conversion_Factors$Japan)


## divide bid level by country-specific PPP factor
JapanData_Client_Xlsx$Bid1_Numeric_PPP <- JapanData_Client_Xlsx$Bid1_Numeric %>%
  divide_by(Conversion_Factors$Japan)


JapanData_Client_Xlsx$Bid2_Numeric_PPP <- JapanData_Client_Xlsx$FollowUp_Bid %>%
  divide_by(Conversion_Factors$Japan)


## divide bid level by country-specific PPP factor
JapanData_Client_Xlsx$INCJP1_Numeric_PlusMissing_PPP <- JapanData_Client_Xlsx$INCJP1_Numeric_PlusMissing %>%
  divide_by(Conversion_Factors$Japan)


## Coalesce stitches things together when there are NAs
JapanData_Client_Xlsx$INC1_All_Adjusted <- coalesce(
  JapanData_Client_Xlsx$INCJP1_Numeric_PlusMissing_PPP
)


## Log monetary variables
JapanData_Client_Xlsx$LogIncome <- JapanData_Client_Xlsx$INC1_All_Adjusted %>% log()


JapanData_Client_Xlsx$LogBid1 <- JapanData_Client_Xlsx$Bid1_Numeric_PPP %>% log()


JapanData_Client_Xlsx$LogBid2 <- JapanData_Client_Xlsx$Bid2_Numeric_PPP %>% log()


## Missing income indicators here
JapanData_Client_Xlsx$INC1_MissingDummy <- coalesce(
  JapanData_Client_Xlsx$INCJP2_MissingDummy ## changing to JP2 for accuracy
)


JapanData_Client_Xlsx$Japan <- 1


# *************************************************************
#### Section 13: Count and proportion by two groups ####
# *************************************************************
## Table 2. Probability test number and percentage per country.


JapanData_Client_Xlsx %>%
  count(Country, T1) %>%
  group_by(Country) %>%
  mutate(
    Total = sum(n),
    Data = sprintf("%d (%s)", n, percent(n / Total, accuracy = 0.1))
  ) %>%
  select(-n, -Total) %>%
  pivot_wider(names_from = T1, values_from = Data) %>%
  ungroup()


# *************************************************************
#### Section 14: Export Cleaned Data ####
# *************************************************************


JapanData_Client_Xlsx %>%
  fwrite(sep = ",",
         WORK("JapanData_Main_Data_Cleaned.csv"))


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
JapanData_Client_Xlsx %>%
  fwrite(sep = ",",
         WORK("Data_Japan_New_Cleaned_Step2PPP.csv"))


# *************************************************************
#### Section 15: DBDC, CV, Bid recoding ####
# *************************************************************


# *************************************************************
#### Section 16: TABLE 3 ####
# *************************************************************
## Output matrix by country
JapanData_Client_Xlsx %>%
  count(Country, DBDC) %>%
  group_by(Country) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, percent(Percentage, accuracy = 0.1))
  ) %>%
  select(-n, -Total, -Percentage) %>%
  pivot_wider(names_from = DBDC, values_from = Result) %>%
  ungroup()


# Table 4. Number of respondents by bid level and by base risk.
with(JapanData_Client_Xlsx,
     table(BASE, Q1, Bid1_Numeric)) %>%
  as.data.frame() %>%
  group_by(BASE, Bid1_Numeric) %>%  # Changed grouping here
  mutate(pct = sprintf("%.1f%%", (Freq/sum(Freq))*100)) %>%
  ungroup() %>%
  mutate(value = paste0(Freq, " (", pct, ")")) %>%
  pivot_wider(
    id_cols = c(BASE, Q1),
    names_from = Bid1_Numeric,
    values_from = value
  ) %>%
  arrange(BASE) %>%
  dplyr::select(BASE, Q1, `2000`, `6000`, `18000`, `56000`, `166000`)


# paste0(JapanData_Client_Xlsx$DBDC %>% table() %>% names(),
#        ": ",
#        JapanData_Client_Xlsx$DBDC %>% table(),
#        " (",
#        JapanData_Client_Xlsx$DBDC %>% table() %>% prop.table() %>% multiply_by(100) %>% round(2),
#        "%)"
# )


# A1_1
# A1_2
# A1_3
# A2
# A3
# A4
# H3
# H4
# H5


# *************************************************************
#### Section 17: Plot general ####
# *************************************************************


TextSize <- 12


TextType <- element_text(size = TextSize,
                         colour = "black",
                         family = "serif")


Fig1 <- JapanData_Client_Xlsx %>%
  ggplot(
    aes(x = Bid1_Numeric, y = Question1)) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial")) +
  scale_y_continuous(labels = c("Current\nrisk",
                                "Risk\nreduction"),
                     breaks = c(0, 1),
                     limits = seq(0, 1, 1)) +
  theme_bw() +
  facet_wrap(~ paste0(ProbabilityTest_Categorical, " - ", BASE),
             ncol = 2) +
  xlab("Bid level") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),

    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    legend.text = TextType,
    legend.title = TextType
  )


ggsave(
  Fig1,
  device = "png",
  filename = paste0(here(), "/Output/Figures/", "Hypertension_Diagnostic_JapanScope.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# *************************************************************
#### Section 18: A2, A3, A4 ####
# *************************************************************


# Table  7)	Majority were confident in the information provided and learned something.
JapanData_Client_Xlsx %>%
  pivot_longer(cols = A2,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response, levels =
                             c("Very confident",
                               "Somewhat confident",
                               "Neither doubtful nor confident",
                               "Somewhat doubtful",
                               "Very doubtful"))) %>%
  dplyr::select(-n, -Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>%
  dplyr::select(
    Country,
    Question,
    `Very confident`,
    `Somewhat confident`,
    `Neither doubtful nor confident`,
    `Somewhat doubtful`,
    `Very doubtful`
  ) %>%
  write.csv(quote = FALSE, row.names = FALSE)


# *************************************************************
#### Section 19: A3 ####
# *************************************************************
JapanData_Client_Xlsx %>%
  pivot_longer(cols = A3,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response, levels =
                             c(  "Excellent" ,
                                 "Good",
                                 "Fair",
                                 "Poor",
                                 "Very poor"))) %>%
  dplyr::select(-n, -Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)


# *************************************************************
#### Section 20: A4 ####
# *************************************************************
JapanData_Client_Xlsx %>%
  pivot_longer(cols = A4,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response, levels =
                             c(  "Excellent" ,
                                 "Good",
                                 "Fair",
                                 "Poor",
                                 "Very poor"))) %>%
  dplyr::select(-n, -Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)


# *************************************************************
#### Section 21: Plot A3 + A4 ####
# *************************************************************
Test_B <- JapanData_Client_Xlsx %>%
  pivot_longer(cols = c(A3, A4),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Response = factor(Response,
                           levels = c("Very poor",
                                      "Poor",
                                      "Fair",
                                      "Good",
                                      "Excellent"))) %>%
  ggplot(aes(x = Response, fill = Response)) +
  geom_bar() +
  facet_wrap(~Question, ncol = 1, labeller = labeller(Question = c(A3 = "Before", A4 = "After"))) +
  theme_bw() +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        axis.text.x = TextType,
        axis.text.y = TextType,
        text = TextType,
        legend.text = TextType)


ggsave(
  Test_B,
  device = "png",
  filename = paste0(here(),
                    "/Output/Figures/",
                    "Hypertension_Diagnostic_JapanResponseCheckB.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# *************************************************************
#### Section 22: H3, H4, H5 ####
# *************************************************************


# *************************************************************
#### Section 23: Table Data ####
# *************************************************************
JapanData_Client_Xlsx %>%
  pivot_longer(cols = H3:H4,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  dplyr::select(-n,-Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)


# *************************************************************
#### Section 24: Table Data for H5 ####
# *************************************************************
JapanData_Client_Xlsx %>%
  pivot_longer(cols = H5,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  dplyr::select(-n,-Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)


# *************************************************************
#### Section 25: A1_* ####
# *************************************************************


# *************************************************************
#### Section 26: A1_* table data ####
# *************************************************************
JapanData_Client_Xlsx %>%
  pivot_longer(cols = starts_with("A1_"),
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)",
                         n,
                         scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response,
                           levels = c(
                             "Strongly disagree",
                             "Somewhat disagree",
                             "Neither agree nor disagree",
                             "Somewhat agree",
                             "Strongly agree"
                           ))) %>%
  dplyr::select(-n, -Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>%
  dplyr::select(Country, Question,
                "Strongly agree",
                "Somewhat agree",
                "Neither agree nor disagree",
                "Somewhat disagree",
                "Strongly disagree") %>%
  write.csv(quote = FALSE)


# *************************************************************
#### Section 27: Plot A1_* ####
# *************************************************************
Test_A <- JapanData_Client_Xlsx %>%
  pivot_longer(cols = starts_with("A1_"), names_to = "Question", values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response, levels = c("Strongly disagree", "Somewhat disagree",  "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>%
  ggplot(aes(x = Country, y = n, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Count", title = "Responses by Country and Question", fill = "Response") +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  facet_wrap(~ Question,
             labeller = as_labeller(c("A1_1" = "A1_1: Real life",
                                      "A1_2" = "A1_2: Informed",
                                      "A1_3" = "A1_3: Accurate"))) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        axis.text.x = TextType,
        axis.text.y = TextType,
        text = TextType,
        legend.text = TextType)


# *************************************************************
#### Section 28: Export ####
# *************************************************************
ggsave(
  Test_A,
  device = "png",
  filename = paste0(here(),
                    "/Output/Figures/",
                    "Hypertension_Diagnostic_JapanResponseCheckA.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)
