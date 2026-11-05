#### Hypertension: Merge First Eight Plus United States ####
# Purpose: Impute United States income, convert purchasing power and merge with the first eight countries.
# Author: Anonymised for peer review.
# Prerequisites: Run scripts 08 and 09.
# Run from the project root: source("Scripts/Setup/10_Hypertension_Setup_MergeFirstEightPlusUnitedStates.R", encoding = "UTF-8")
# Inputs:
#   Data/Intermediate/Data_ABCSUKGRFIPT_Step3.csv, USData_Main_Data_Cleaned.csv and USData_Main_Timers_Cleaned.csv.
# Outputs:
#   Data/Intermediate/Data_AllCountries_Step3.csv (merged nine-country data).
#   Data/Intermediate/Data_US_New_Cleaned_Step1.csv and Data_US_New_Cleaned_Step2PPP.csv.
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


# *************************************************************
#### Section 1: Path configuration ####
# *************************************************************
## Duplicated identically in every Setup script so each one can be read and run
## on its own; see Data/RAW_MANIFEST.md for the raw inputs it resolves against.

## Where pipeline intermediates are written (git-ignored).
if (!dir.exists(here::here("Data", "Intermediate"))) {
  dir.create(here::here("Data", "Intermediate"), recursive = TRUE, showWarnings = FALSE)
}


#' Path to a pipeline intermediate (read or write).
WORK <- function(...) file.path(here::here("Data", "Intermediate"), ...)


# *************************************************************
#### Section 2: Recode assertions -- make silent fall-through impossible ####
# *************************************************************
## Every income/education/health recode below is exact string matching against
## multi-language survey labels. dplyr::case_when() with no .default returns NA
## on fall-through, and that is exactly how the Canadian INCCA2 defect (UK GBP
## labels matched against CAD data) survived to publication. After every
## recode, every observed non-missing label must produce a number or be a
## declared don't-know/refuse label; anything else is a hard stop.

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
#### Section 4: Import datasets ####
# *************************************************************


## Start by importing SAV data
## here() is the best practice for filepaths
## and I use %>% everywhere as it's easier to read
Data_US <-
  WORK("USData_Main_Data_Cleaned.csv") %>%
  fread() %>%
  data.frame()


Data_US$UnitedStates <- 1


Data_Rest <- WORK("Data_ABCSUKGRFIPT_Step3.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 5: Add US Timers ####
# *************************************************************


USData_Timers <-
  WORK("USData_Main_Timers_Cleaned.csv") %>%
  fread() %>%
  data.frame()


Data_US$Speeders_Survey_CompletionTime <- USData_Timers$Speeders_Survey_CompletionTime


Data_US$Speeders_Survey_CountryThreshold <- USData_Timers$Speeders_Survey_CountryThreshold


Data_US$Speeders_Survey_TestDummy <- USData_Timers$Speeders_Survey_TestDummy


Data_US$Slowers_Survey_CountryThreshold <- USData_Timers$Slowers_Survey_CountryThreshold


Data_US$Slowers_Survey_TestDummy <- USData_Timers$Slowers_Survey_TestDummy


Data_US$Speeders_Valuation_CompletionTime <- USData_Timers$Speeders_Valuation_CompletionTime


Data_US$Speeders_Valuation_CountryThreshold <- USData_Timers$Speeders_Valuation_CountryThreshold


Data_US$Speeders_Valuation_TestDummy <- USData_Timers$Speeders_Valuation_TestDummy


Data_US$Slowers_Valuation_CountryThreshold <- USData_Timers$Slowers_Valuation_CountryThreshold


Data_US$Slowers_Valuation_TestDummy <- USData_Timers$Slowers_Valuation_TestDummy


Data_US$INCFI1_Numeric_PlusMissing_PPP <- 0


Data_US$INCGR1_Numeric_PlusMissing_PPP <- 0


Data_US$INCPT1_Numeric_PlusMissing_PPP <- 0


Data_US$INCAT1_Numeric_PlusMissing_PPP <- 0


Data_US$INCUK1_Numeric_PlusMissing_PPP <- 0


Data_US$INCCA1_Numeric_PlusMissing_PPP <- 0


Data_US$INCSI1_Numeric_PlusMissing_PPP <- 0


Data_US$INCBE1_Always_Numeric_PlusMissing_PPP <- 0


# *************************************************************
#### Section 6: Update ELPTFI ####
# *************************************************************


# Misc dummies -------------------------------------------------------------


## From character to number
Data_US$Kids_Numeric <- Data_US$KIDS02 %>% as.numeric()


## Categorising partner or no
Data_US$MarriedDummy <- ifelse(
  !Data_US$S4 %in%
    c("Not in a relationship",
      "Other"),
  1,
  0)


Data_US$EmployDummy_Employed <-
  ifelse(Data_US$EMP01 %in% c("Employed full-time",
                                  "Self employed"),
         1,
         0)


Data_US$EmployDummy_Retired <-
  ifelse(Data_US$EMP01 %in% c("Retired"),
         1,
         0)


Data_US$EmployDummy_PT <-
  ifelse(Data_US$EMP01 %in% c("Employed part-time"),
         1,
         0)


Data_US$Dummy_Female <-
  ifelse(Data_US$GENDER_NonBinary %in% c("Female"),
         1,
         0)


Data_US$HH_PersonsNumeric <-
  ifelse(Data_US$SE3 %in% c("7 or more"),
         7,
         Data_US$SE3) %>% as.numeric()


# Age group recoding
Data_US$Age_1829Dummy <-
  ifelse(Data_US$AgeGroup %in% c("18-29"),
         1,
         0) %>% as.numeric()


Data_US$Age_3044Dummy <-
  ifelse(Data_US$AgeGroup %in% c("30-44"),
         1,
         0) %>% as.numeric()


Data_US$Age_4560Dummy <-
  ifelse(Data_US$AgeGroup %in% c("45-60"),
         1,
         0) %>% as.numeric()


Data_US$Age_60Dummy <-
  ifelse(Data_US$AgeGroup %in% c("60+"),
         1,
         0) %>% as.numeric()


Data_US$AgeGroup_Numeric <- dplyr::recode(Data_US$AgeGroup,
                                              '18-29' = 23.50,
                                              '30-44' = 37,
                                              '45-60' = 52.5,
                                              '60+' = 60)


# BY QUOTA -------------------------------------------------------------
Data_US$AgeQuota_1826 <-
  ifelse(Data_US$AgeQuota %in% c("18-26"),
         1,
         0) %>% as.numeric()


Data_US$AgeQuota_2734 <-
  ifelse(Data_US$AgeQuota %in% c("27-34"),
         1,
         0) %>% as.numeric()


Data_US$AgeQuota_3539 <-
  ifelse(Data_US$AgeQuota %in% c("35-39"),
         1,
         0) %>% as.numeric()


Data_US$AgeQuota_4044 <-
  ifelse(Data_US$AgeQuota %in% c("40-44"),
         1,
         0) %>% as.numeric()


Data_US$AgeQuota_4559 <-
  ifelse(Data_US$AgeQuota %in% c("45-59"),
         1,
         0) %>% as.numeric()


Data_US$AgeQuota_6065 <-
  ifelse(Data_US$AgeQuota %in% c("60-65"),
         1,
         0) %>% as.numeric()


Data_US$AgeQuota_65 <-
  ifelse(Data_US$AgeQuota %in% c("65+"),
         1,
         0) %>% as.numeric()


Data_US$Age_1829Dummy_Adjusted <- Data_US$Age_1829Dummy + 1


Data_US$Age_3044Dummy_Adjusted <- Data_US$Age_3044Dummy + 1


Data_US$Age_4560Dummy_Adjusted <- Data_US$Age_4560Dummy + 1


Data_US$Age_60Dummy_Adjusted <- Data_US$Age_60Dummy + 1


# Education dummies -------------------------------------------------------------
Data_US$Dummy_HighEducation_Fi <- 0


Data_US$Dummy_HighEducation_EL <- 0


Data_US$Dummy_HighEducation_PT <- 0


Data_US$Dummy_HighEducation_US <-
  ifelse(Data_US$ISCED == "High",
         1,
         0)


Data_US$EDU_All <-
  ifelse(
    Data_US$Dummy_HighEducation_US == 1,
    1, 0
  )


Data_US$RED_Numeric <- Data_US$RED %>% as.numeric()


Data_US$Question2 <- Data_US$FollowUp_Numeric


## Dummy categorise education levels
Data_US$EducationCategory <- Data_US$ISCED


Data_US$EducationCategory_Dummy_High <- ifelse(
  Data_US$ISCED == "High", 1, 0)


Data_US$EducationCategory_Dummy_Medium <- ifelse(
  Data_US$ISCED == "Medium", 1, 0)


Data_US$EducationCategory_Dummy_Low <- ifelse(
  Data_US$ISCED == "Low", 1, 0)


# *************************************************************
#### Section 7: Recoding health questions ####
# *************************************************************


# How much do you agree or disagree with the following statements? - My survey responses reflect what I would have done in real life.
Data_US$Response_RealLife <- dplyr::recode(
  Data_US$A1_1,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - The survey provided me with enough information to make informed choices.
Data_US$Response_Information <- dplyr::recode(
  Data_US$A1_2,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - I find the description of high blood pressure provided in this survey to be accurate
Data_US$Response_Accurate <- dplyr::recode(
  Data_US$A1_3,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How confident are you that the information that has been provided in this survey is correct?
Data_US$Response_Confident <- dplyr::recode(
  Data_US$A2,
  "Very confident" = 4,
  "Somewhat confident"                 = 3,
  "Neither doubtful nor confident"  = 2,
  "Somewhat doubtful" = 1,
  "Very doubtful"   = 0
)


# Q02 and DX
## Yes if heard of high blood pressure, zero if not
Data_US$Health_HeardOfHBP <- ifelse(
  Data_US$Q02 == "Yes", 1, 0)


## Yes if they had high blood pressure
Data_US$Health_HadHBP <- ifelse(
  Data_US$Q03 == "Yes", 1, 0)


# Before answering this survey, did you know about the health risks associated with untreated high blood pressure?
Data_US$Health_HBP_D1 <- ifelse(
  Data_US$D1 == "No, I did not know", 0, 1)


# Before answering this survey, did you know that high blood pressure could be treated by having a healthier lifestyle or medical treatments?
Data_US$Health_HBP_D2 <- ifelse(
  Data_US$D2 == "No, I did not know", 1, 0)


## Diagnosed and stuck to it
Data_US$Health_HBP_DiagnosedStuck <- ifelse(
  Data_US$D2 == "Yes", 1, 0)


Data_US$Health_HBP_SideEffects <- ifelse(
  Data_US$D2 == "Yes", 1, 0)


# Dummy coding knowledge of risks
## Can do faster with %>% mutate() but more legible here
## Recoding Q04 here
Data_US$Risks_Genetic <- ifelse(
  Data_US$Q04_1 == "Yes", 1, 0)


Data_US$Risks_Obesity <- ifelse(
  Data_US$Q04_2 == "Yes", 1, 0)


Data_US$Risks_Alcohol <- ifelse(
  Data_US$Q04_3 == "Yes", 1, 0)


Data_US$Risks_Exposure <- ifelse(
  Data_US$Q04_4 == "Yes", 1, 0)


Data_US$Risks_Insulin <- ifelse(
  Data_US$Q04_5 == "Yes", 1, 0)


Data_US$Risks_Aging <- ifelse(
  Data_US$Q04_6 == "Yes", 1, 0)


Data_US$Risks_Exercise <- ifelse(
  Data_US$Q04_7 == "Yes", 1, 0)


Data_US$Risks_Smoking <- ifelse(
  Data_US$Q04_8 == "Yes", 1, 0)


Data_US$Risks_Stress <- ifelse(
  Data_US$Q04_9 == "Yes", 1, 0)


Data_US$Risks_Deprive <- ifelse(
  Data_US$Q04_10 == "Yes", 1, 0)


# Recoding H series Qs


# How do you rate your risk of developing high blood pressure compared to people of your age and gender?
Data_US$HBP_RelativeRiskHigh <- ifelse(
  Data_US$H2 %in%
    c("My risk is clearly larger",
      "My risk is somewhat larger"),
  1, 0)


# Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?
Data_US$HBP_10Yr <- ifelse(
  Data_US$H3 == "Yes",
  1, 0)


# When you choose between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle to reduce your risk instead of paying?
Data_US$HBP_Healthier <- ifelse(
  Data_US$H4 == "Yes",
  1, 0)


# Please consider the statement: “I would pay almost anything to improve my health even by a small amount”. Do you…
Data_US$HBP_PayAnything <- ifelse(
  Data_US$H5 == "…agree",
  1, 0)


# Is high blood pressure a genetic condition in your family?
Data_US$HBP_Genetic <- ifelse(
  Data_US$H6 == "Yes",
  1, 0)


# Do you know of people close to you (family, friends) who have, or have had, high blood pressure?
Data_US$HBP_Family <- ifelse(
  Data_US$H7 == "Yes",
  1, 0)


# I have been diagnosed with: - [disease name]
Data_US$Diagnosed_KidneyDisease <- ifelse(Data_US$H8_1 == "Yes", 1, 0)


Data_US$Diagnosed_HeartDisease <- ifelse(Data_US$H8_2 == "Yes", 1, 0)


Data_US$Diagnosed_Obesity <- ifelse(Data_US$H8_3 == "Yes", 1, 0)


Data_US$Diagnosed_Diabetes <- ifelse(Data_US$H8_4 == "Yes", 1, 0)


Data_US$Diagnosed_ParkinsonDisease <- ifelse(Data_US$H8_5 == "Yes", 1, 0)


Data_US$Diagnosed_AlzheimersDementias <- ifelse(Data_US$H8_6 == "Yes", 1, 0)


Data_US$Diagnosed_RheumatoidArthritis <- ifelse(Data_US$H8_7 == "Yes", 1, 0)


Data_US$Diagnosed_Respiratory <- ifelse(Data_US$H8_8 == "Yes", 1, 0)


Data_US$Diagnosed_Other <- ifelse(Data_US$H8_9 == "Yes", 1, 0)


Data_US$Diagnosed_None <- ifelse(Data_US$H8_10 == "Yes", 1, 0)


# How would you describe your health in general?
Data_US$Health_Own <- dplyr::recode(
  Data_US$H9,
  "Excellent" = 5,
  "Good" = 4,
  "Fair" = 3,
  "Poor" = 2,
  "Very poor" = 1,
  "Prefer not to answer" = 0
)


# How would you describe your health relative to others of your age and gender in your country?
Data_US$Health_Relative <- dplyr::recode(
  Data_US$H10,
  "Well above average" = 5,
  "Above average" = 4,
  "Average" = 3,
  "Below average" = 2,
  "Well below average" = 1,
  "Prefer not to answer" = 0
)


## Classify
Data_US$Health_Relative_BelowAverage <- ifelse(Data_US$Health_Relative == 2, 1, 0)


Data_US$Health_Relative_AboveAverage <- ifelse(Data_US$Health_Relative == 4, 1, 0)


# How would you describe your weight relative to others of your height, age and gender in your country?
Data_US$Health_Weight <- dplyr::recode(
  Data_US$H11,
  "Less than average" = 3,
  "Average" = 2,
  "More than average" = 1,
  "Prefer not to answer" = 0
)


# Do you smoke?
Data_US$Health_Smoke <- dplyr::recode(
  Data_US$H12,
  "No" = 3,
  "Yes, occasionally" = 2,
  "Yes, regularly" = 1,
  "Prefer not to answer" = 0
)


# How many alcoholic drinks do you usually have in a normal week?
Data_US$Health_Drink  <- dplyr::recode(
  Data_US$H13,
  "I usually don’t drink alcohol" = 1,
  "1-4 drinks per week" = 2,
  "5-9 drinks per week" = 3,
  "More than 9 drinks per week" = 4,
  "999999" = 0
)


# How often do you exercise in a normal week? Please pick the answer that best describes a normal week.
Data_US$Health_Exercise  <- dplyr::recode(
  Data_US$H14,
  "More than 5 times per week" = 5,
  "3-5 times per week"       = 4,
  "1-2 times per week" = 3,
  "Less than 1 time per week" = 2,
  "I don’t exercise" = 1,
  "Prefer not to answer"     = 0)


# Do you use sunscreen?
Data_US$Health_Sunscreen <- dplyr::recode(
  Data_US$H15,
  "Yes, regularly" = 3,
  "Yes, occasionally" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Do you consider yourself to have a healthy lifestyle (exercise regularly, eat healthily, sleep more than 7 hours a night...)?
Data_US$Health_Lifestyle <- dplyr::recode(
  Data_US$H16,
  "Yes" = 4,
  "In general yes" = 3,
  "In general no" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Take a moment to think about the products you typically purchase. How do you feel about them?
Data_US$Health_PurchaseProducts <- dplyr::recode(
  Data_US$H17,
  "Totally safe for people’s health" = 4 ,
  "Rather safe for people’s health" = 3 ,
  "I don’t know" = 2,
  "Rather unsafe for people’s health" = 1,
  "Totally unsafe for people’s health" = 0
)


# Which of the following statements best describes your purchasing behaviour?
Data_US$Health_PurchaseBehaviour <- dplyr::recode(
  Data_US$H18,
  "I try to buy safer products even if it comes at an extra cost (e.g. by choosing safer products even if they are a bit mo" = 3,
  "I try to buy safer products if it doesn’t come at an extra cost (e.g. by choosing safer products if they are similar i" = 2,
  "I don’t know" = 1,
  "I do not pay much attention to the products’ effect on my health" = 0
)


# How would you describe your knowledge of high blood pressure before having taken this survey?
Data_US$Response_HBP_Before <- dplyr::recode(
  Data_US$A3,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
Data_US$Response_HBP_After <- dplyr::recode(
  Data_US$A4,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


Data_US$Response_HBP_Change <-
  (Data_US$Response_HBP_After -
     Data_US$Response_HBP_Before)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
Data_US$Health_Cost <- ifelse(
  Data_US$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket",
      "A private health insurance scheme (e.g. through your employer) would cover all or most of the costs" ),
  1, 0)


## Own pocked specifically
Data_US$Health_Cost_OwnPocket <- ifelse(
  Data_US$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket"),
  1, 0)


## SE2 categorises nicely but let's add a dummy
Data_US$Urban <- ifelse(
  Data_US$SE2 == "Urban",
  1, 0)


# How certain are you of the maximum value you just indicated?
Data_US$Certainty <- dplyr::recode(
  Data_US$Q7,
  "Strongly certain" = 4,
  "Somewhat certain" = 3,
  "Neither certain nor uncertain" = 2,
  "Somewhat uncertain" = 1,
  "Strongly uncertain" = 0
)


## Define here for later
Data_US$Spike <- ifelse(
  Data_US$Q4 %in% c("No, I would not pay anything"),
  0, 1
)


# *************************************************************
#### Section 8: Weights ####
# *************************************************************


## Simple interactions with existing weights
Data_US$Weights_Austria <- 0


Data_US$Weights_Belgium <- 0


Data_US$Weights_Canada <- 0


Data_US$Weights_Slovenia <- 0


Data_US$Weights_UnitedKingdom <- 0


Data_US$Weights_Finland <- 0


Data_US$Weights_Greece <- 0


Data_US$Weights_Portugal <- 0


Data_US$Weights_US <- ((Data_US$weight %>% as.numeric()) * Data_US$UnitedStates %>% as.numeric())


Data_US$Austria <- 0


Data_US$Belgium <- 0


Data_US$Canada <- 0


Data_US$Slovenia <- 0


Data_US$UnitedKingdom <- 0


Data_US$Finland <- 0


Data_US$Greece <- 0


Data_US$Portugal <- 0


Data_US$Dummy_HighEducation_UK <- 0


Data_US$Dummy_HighEducation_AUT <- 0


Data_US$Dummy_HighEducation_Bel <- 0


Data_US$Dummy_HighEducation_Slo <- 0


Data_US$Dummy_HighEducation_CAD <- 0


# Data_US$Dummy_HighEducation_CAD <- 0
# Data_US$Dummy_HighEducation_CAD <- 0
# Data_US$Dummy_HighEducation_CAD <- 0

# *************************************************************
#### Section 9: Scaling ####
# *************************************************************


# Income recoding for income question 1 -------------------------------------------------------------


## Recode Austrian missing data
Data_US$INCFI1_MissingDummy <- 0


Data_US$INCGR1_MissingDummy <- 0


Data_US$INCPT1_MissingDummy <- 0


Data_US$INCUS1_MissingDummy <- ifelse(
  Data_US$INCUS1 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


# Income recoding for income question 2 -------------------------------------------------------------


## Recode Austrian missing data
Data_US$INCFI2_MissingDummy <- 0


Data_US$INCGR2_MissingDummy <- 0


Data_US$INCPT1_MissingDummy <- 0


Data_US$INCUS2_MissingDummy <- ifelse(
  Data_US$INCUS2 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


Data_US$INCBE1_Always <- 0


Data_US$INCBE2_Always <- 0


Data_US$INCAT1_MissingDummy <- 0


Data_US$INCUK1_MissingDummy <- 0


Data_US$INCCA1_MissingDummy <- 0


Data_US$INCSI1_MissingDummy <- 0


Data_US$INCBE1_Always_MissingDummy <- 0


Data_US$INCAT2_MissingDummy <- 0


Data_US$INCUK2_MissingDummy <- 0


Data_US$INCCA2_MissingDummy <- 0


Data_US$INCSI2_MissingDummy <- 0


Data_US$INCBE2_Always_MissingDummy <- 0


# Income recoding for income question 2 -------------------------------------------------------------

Data_US_Backup <- Data_US


## Very large recoding exercise taking the midpoints of each range
Data_US <- Data_US %>%
  dplyr::mutate(

    INCUS1_Numeric = dplyr::case_when(
      is.na(INCUS1) ~ NA,
      INCUS1 %in% "$2,801 to $3,400" ~ (0.5 * 2801) + (0.5 * 3400),
      INCUS1 %in% "$0 to $2,000" ~ (0.5 * 0) + (0.5 * 2000),
      INCUS1 %in% "$5,701 to $6,700" ~ (0.5 * 5701) + (0.5 * 6700),
      INCUS1 %in% "$6,701 to $8,200" ~ (0.5 * 6701) + (0.5 * 8200),
      INCUS1 %in% "$10,901 to $13,400" ~ (0.5 * 10901) + (0.5 * 13400),
      INCUS1 %in% "$2,001 to $2,800" ~ (0.5 * 2001) + (0.5 * 2800),
      INCUS1 %in% "$4,101 to $4,900" ~ (0.5 * 4101) + (0.5 * 4900),
      INCUS1 %in% "$8,201 to $10,900" ~ (0.5 * 8201) + (0.5 * 10900),
      INCUS1 %in% "$3,401 to $4,100" ~ (0.5 * 3401) + (0.5 * 4100),
      INCUS1 %in% "$4,901 to $5,700" ~ (0.5 * 4901) + (0.5 * 5700),
      INCUS1 %in% "$13,401 or more" ~ 13401,
      INCUS1 %in% "Prefer not to answer" ~ NA,
      INCUS1 %in% "I don’t know" ~ NA),

      INCUS2_Numeric = dplyr::case_when(
        is.na(INCUS2) ~ NA,
      INCUS2 %in% "Prefer not to answer" ~ NA,
      INCUS2 %in% "I don’t know" ~ NA,
      INCUS2 %in% "$5,701 to $8,200" ~ (0.5 * 5701 ) + (0.5 * 8200),
      INCUS2 %in% "$13,401 or more" ~ 13401,
      INCUS2 %in% "$2,801 to $4,100" ~ (0.5 * 2801 ) + (0.5 * 4100),
      INCUS2 %in% "$4,101 to $5,700" ~ (0.5 * 4101 ) + (0.5 * 5700),
      INCUS2 %in% "$8,201 to $13,400" ~ (0.5 * 8201 ) + (0.5 * 13400),
      INCUS2 %in% "$0 to $2,800" ~ (0.5 * 0) + (0.5 * 2800)
      ))


## TASK E -- assert every observed non-missing US income label got a mapping.
assert_recode_family(Data_US, list(
  INCUS1 = list(out = "INCUS1_Numeric",
                na = c("Prefer not to answer", "I don’t know")),
  INCUS2 = list(out = "INCUS2_Numeric",
                na = c("Prefer not to answer", "I don’t know"))
))


message("  [assert] 06_: both US income recodes are complete.")


## Coalesce by country: fast way
Data_US$INCUS1_All <- coalesce(
  Data_US$INCUS1_Numeric,
  Data_US$INCUS2_Numeric
)


Data_US$INCUS1_Missing <- coalesce(
  Data_US$INCUS2_MissingDummy
)


## Coalesce by country: fast way
Data_US$INCUS2_All <- coalesce(
  Data_US$INCUS2_Numeric
)


Data_US$INCUS2_Missing <- coalesce(
  Data_US$INCUS2_MissingDummy
)


## Coalesce by country: fast way
Data_US$INC1_All <- coalesce(
  Data_US$INCUS1_All
)


Data_US$INC1_Missing <- coalesce(
  Data_US$INCUS1_MissingDummy
)


## Coalesce by country: fast way
Data_US$INC2_All <- coalesce(
  Data_US$INCUS2_Numeric
)


Data_US$INC2_Missing <- coalesce(
  Data_US$INCUS2_MissingDummy
)


# US
## Predict missing income
Model_PredictMissingIncome_US <- lm(
  formula = log(INCUS1_All) ~
    AgeGroup +
    Dummy_HighEducation_US +
    MarriedDummy +
    HH_PersonsNumeric +
    EmployDummy_Retired +
    EmployDummy_PT +
    EmployDummy_Employed,
  data = Data_US[Data_US$Country == "United States", ]
)


## Match imputed income by Country and Respondent_Serial, not row position.
Data_US$INCUS1_Numeric_PlusMissing <- impute_income_keyed(
  data      = Data_US,
  model     = Model_PredictMissingIncome_US,
  need_mask = Data_US$Country == "United States" &
              Data_US$INCUS2_MissingDummy == 1,
  reported  = Data_US$INCUS1_All,
  label     = "United States")


Data_US$INCAT1_Numeric <- 0


Data_US$INCUK1_Numeric <- 0


Data_US$INCCA1_Numeric <- 0


Data_US$INCSI1_Numeric <- 0


Data_US$INCBE1_Always_Numeric <- 0


Data_US$INCAT2_Numeric <- 0


Data_US$INCUK2_Numeric <- 0


Data_US$INCCA2_Numeric <- 0


Data_US$INCSI2_Numeric <- 0


Data_US$INCBE2_Always_Numeric <- 0


Data_US$INCUK1_Numeric_PlusMissing <- 0


Data_US$INCBE1_Always_Numeric_PlusMissing <- 0


Data_US$INCAT1_Numeric_PlusMissing <- 0


Data_US$INCSI1_Numeric_PlusMissing <- 0


Data_US$INCCA1_Numeric_PlusMissing <- 0


Data_US$INCUK1_Numeric_PlusMissing_Log <- 0


# *************************************************************
#### Section 10: Transform BIDTOT ####
# *************************************************************


Data_US <- Data_US %>%
  dplyr::mutate(
    BIDTOT1_Numeric = gsub("[^0-9.]", "", BIDTOT1) %>% as.numeric(),
    BIDTOT2_Numeric = gsub("[^0-9.]", "", BIDTOT2) %>% as.numeric(),
    BIDTOT3_Numeric = gsub("[^0-9.]", "", BIDTOT3) %>% as.numeric())


# *************************************************************
#### Section 11: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_US %>%
  fwrite(sep = ",",
         WORK("Data_US_New_Cleaned_Step1.csv"))


# *************************************************************
#### Section 12: Convert to PPP ####
# *************************************************************


## All code elsewhere just run it here
# *************************************************************
#### Section 13: Setup libraries and inputs ####
# *************************************************************


library(haven)


library(here)


library(magrittr)


library(data.table)


library(tidyverse)


library(readxl)


library(DCchoice)


# *************************************************************
#### Section 14: Path configuration (self-contained -- no shared helper file) ####
# *************************************************************
## Duplicated identically in every Setup script so each one can be read and run
## on its own. (Also sourced from inside 06_, which defines the same WORK() --
## redefining it here identically is harmless and keeps this script runnable
## standalone too.)

## Where pipeline intermediates are written (git-ignored).
if (!dir.exists(here::here("Data", "Intermediate"))) {
  dir.create(here::here("Data", "Intermediate"), recursive = TRUE, showWarnings = FALSE)
}


#' Path to a pipeline intermediate (read or write).
WORK <- function(...) file.path(here::here("Data", "Intermediate"), ...)


# *************************************************************
#### Section 15: Import data ####
# *************************************************************


## Start by importing cleaned data
## here() for filepaths
## data.table::fread() is the fastest way to import CSV files
Data <- WORK("Data_US_New_Cleaned_Step1.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 16: Country PPP data ####
# *************************************************************
## Here: https://data.oecd.org/conversion/purchasing-power-parities-ppp.htm


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


# *************************************************************
#### Section 17: BIDTOT levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
Data$BIDTOT1_PPP <- Data$BIDTOT1_Numeric %>%
      divide_by(Conversion_Factors$United.States)


Data$BIDTOT2_PPP <- Data$BIDTOT2_Numeric %>%
  divide_by(Conversion_Factors$United.States)


Data$BIDTOT3_PPP <- Data$BIDTOT3_Numeric %>%
  divide_by(Conversion_Factors$United.States)


# *************************************************************
#### Section 18: Bid levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
Data$Bid1_Numeric_PPP <- Data$Bid1_Numeric %>%
  divide_by(Conversion_Factors$United.States)


Data$Bid2_Numeric_PPP <- Data$FollowUp_Bid %>%
  divide_by(Conversion_Factors$United.States)


# *************************************************************
#### Section 19: Income levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
Data$INCUS1_Numeric_PlusMissing_PPP <- Data$INCUS1_Numeric_PlusMissing %>%
  divide_by(Conversion_Factors$United.States)


## Coalesce stitches things together when there are NAs
Data$INC1_All_Adjusted <- coalesce(
  Data$INCUS1_Numeric_PlusMissing_PPP
)


## Log monetary variables
Data$LogIncome <- Data$INC1_All_Adjusted %>% log()


Data$LogBid1 <- Data$Bid1_Numeric_PPP %>% log()


Data$LogBid2 <- Data$Bid2_Numeric_PPP %>% log()


# This will properly combine all missing indicators
Data$INC1_MissingDummy <- ifelse(
  Data$INCUS1_MissingDummy == 1,
  1, 0)


Data$INC2_MissingDummy <- ifelse(
  Data$INCUS2_MissingDummy == 1,
  1, 0)


# *************************************************************
#### Section 20: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data %>%
  fwrite(sep = ",",
         WORK("Data_US_New_Cleaned_Step2PPP.csv"))


# END OF SCRIPT ************************************************************** %>% %>%


Data_US <- WORK("Data_US_New_Cleaned_Step2PPP.csv") %>%
  fread() %>%
  data.frame()


Data_Rest <- WORK("Data_ABCSUKGRFIPT_Step3.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 21: Trim Data_Rest ####
# *************************************************************


#
# Data_US$INCAT1_Numeric_PlusMissing_PPP <- 0
# Data_US$INCUK1_Numeric_PlusMissing_PPP <- 0
# Data_US$INCCA1_Numeric_PlusMissing_PPP <- 0
# Data_US$INCSI1_Numeric_PlusMissing_PPP <- 0
# Data_US$INCBE1_Always_Numeric_PlusMissing_PPP <- 0
#
#
# Data_ABCSUK$Finland <- 0
# Data_ABCSUK$Greece <- 0
# Data_ABCSUK$Portugal <- 0
# Data_ABCSUK$Weights_Finland <- 0
# Data_ABCSUK$Weights_Greece <- 0
# Data_ABCSUK$Weights_Portugal <- 0
# Data_ABCSUK$INCFI1_Numeric_PlusMissing_PPP <- 0
# Data_ABCSUK$INCGR1_Numeric_PlusMissing_PPP <- 0
# Data_ABCSUK$INCPT1_Numeric_PlusMissing_PPP <- 0


CheckNames <- c(
  "Respondent_Serial",
  "Country",
  "Introduction",
  "GENDER_NonBinary",
  "resp_age",
  "AgeQuota",
  "AgeGroup",
  "ISCED",
  "S4",
  "KIDS02",
  "S7",
  "T1",
  "Q01_1",
  "Q01_2",
  "Q01_3",
  "Q01_4",
  "Q01_5",
  "Q01_999999",
  "Q02",
  "Q03",
  "Q04_1",
  "Q04_2",
  "Q04_3",
  "Q04_4",
  "Q04_5",
  "Q04_6",
  "Q04_7",
  "Q04_8",
  "Q04_9",
  "Q04_10",
  "Q04_888888",
  "D1",
  "D2",
  "rQ04_1",
  "Q05",
  "BT1",
  "BT2",
  "BASE",
  "RED",
  "RR",
  "X_Var",
  "BID1",
  "BIDTOT1",
  "BID2",
  "BIDTOT2",
  "BID3",
  "BIDTOT3",
  "Q1",
  "Q2",
  "Q3",
  "Q4",
  "Q5a",
  "Q5b",
  "Q5b_Other",
  "Q6a",
  "Q6b",
  "Q6c",
  "Q7",
  "H2",
  "H3",
  "H4",
  "H5",
  "H6",
  "H7",
  "H8_1",
  "H8_2",
  "H8_3",
  "H8_4",
  "H8_5",
  "H8_6",
  "H8_7",
  "H8_8",
  "H8_9",
  "H8_10",
  "H8_999999",
  "H8_9_Other",
  "H9",
  "H10",
  "H11",
  "H12",
  "H13",
  "H14",
  "H15",
  "H16",
  "H17",
  "H18",
  "A1_1",
  "A1_2",
  "A1_3",
  "A2",
  "A3",
  "A4",
  "SE1",
  "SE2",
  "EMP01",
  "SE3",
  "Deleeck",
  "SE4",
  "I1",
  "I2",
  "I3",
  "I4",
  "OE1",
  "RespondentDevice",
  "weight",
  "Duration",
  "Field",
  "Austria",
  "Belgium",
  "Canada",
  "Slovenia",
  "UnitedKingdom",
  "Finland",
  "Greece",
  "Portugal",
  "ProbabilityTest_Numeric",
  "ProbabilityTest_Categorical",
  "Speeders_Survey_CompletionTime",
  "Speeders_Survey_CountryThreshold",
  "Slowers_Survey_CountryThreshold",
  "Speeders_Survey_TestDummy",
  "Slowers_Survey_TestDummy",
  "Speeders_Valuation_CompletionTime",
  "Speeders_Valuation_CountryThreshold",
  "Slowers_Valuation_CountryThreshold",
  "Speeders_Valuation_TestDummy",
  "Slowers_Valuation_TestDummy",
  "Question1",
  "Q2_Missing",
  "Q3_Missing",
  "FollowUp",
  "FollowUp_Numeric",
  "Bid1_Numeric",
  "FollowUp_Bid",
  "BASE_Numeric",
  "DBDC",
  "INCBE1_Always",
  "INCBE2_Always",
  "INCAT1_MissingDummy",
  "INCUK1_MissingDummy",
  "INCCA1_MissingDummy",
  "INCSI1_MissingDummy",
  "INCBE1_Always_MissingDummy",
  "INCAT2_MissingDummy",
  "INCUK2_MissingDummy",
  "INCCA2_MissingDummy",
  "INCSI2_MissingDummy",
  "INCBE2_Always_MissingDummy",
  "INCAT1_Numeric",
  "INCUK1_Numeric",
  "INCCA1_Numeric",
  "INCSI1_Numeric",
  "INCBE1_Always_Numeric",
  "INC1_All",
  "INC1_Missing",
  "INCAT2_Numeric",
  "INCUK2_Numeric",
  "INCCA2_Numeric",
  "INCSI2_Numeric",
  "INCBE2_Always_Numeric",

  "INC2_All",
  "Kids_Numeric",
  "MarriedDummy",
  "EmployDummy_Employed",
  "EmployDummy_Retired",
  "EmployDummy_PT",
  "Dummy_Female",
  "HH_PersonsNumeric",
  "Age_1829Dummy",
  "Age_3044Dummy",
  "Age_4560Dummy",
  "Age_60Dummy",
  "AgeGroup_Numeric",
  "AgeQuota_1826",
  "AgeQuota_2734",
  "AgeQuota_3539",
  "AgeQuota_4044",
  "AgeQuota_4559",
  "AgeQuota_6065",
  "AgeQuota_65",
  "Dummy_HighEducation_UK",
  "Dummy_HighEducation_AUT",
  "Dummy_HighEducation_Bel",
  "Dummy_HighEducation_Slo",
  "Dummy_HighEducation_CAD",
  "EDU_All",
  "INCUK1_Numeric_PlusMissing",
  "INCBE1_Always_Numeric_PlusMissing",
  "INCAT1_Numeric_PlusMissing",
  "INCSI1_Numeric_PlusMissing",
  "INCCA1_Numeric_PlusMissing",
  "INCUK1_Numeric_PlusMissing_Log",
  "RED_Numeric",
  "Age_1829Dummy_Adjusted",
  "Age_3044Dummy_Adjusted",
  "Age_4560Dummy_Adjusted",
  "Age_60Dummy_Adjusted",
  "Question2",
  "EducationCategory",
  "EducationCategory_Dummy_High",
  "EducationCategory_Dummy_Medium",
  "EducationCategory_Dummy_Low",
  "Health_HeardOfHBP",
  "Health_HadHBP",
  "Health_HBP_D1",
  "Health_HBP_D2",
  "Health_HBP_DiagnosedStuck",
  "Health_HBP_SideEffects",
  "Risks_Genetic",
  "Risks_Obesity",
  "Risks_Alcohol",
  "Risks_Exposure",
  "Risks_Insulin",
  "Risks_Aging",
  "Risks_Exercise",
  "Risks_Smoking",
  "Risks_Stress",
  "Risks_Deprive",
  "HBP_RelativeRiskHigh",
  "HBP_10Yr",
  "HBP_Healthier",
  "HBP_PayAnything",
  "HBP_Genetic",
  "HBP_Family",
  "Diagnosed_KidneyDisease",
  "Diagnosed_HeartDisease",
  "Diagnosed_Obesity",
  "Diagnosed_Diabetes",
  "Diagnosed_ParkinsonDisease",
  "Diagnosed_AlzheimersDementias",
  "Diagnosed_RheumatoidArthritis",
  "Diagnosed_Respiratory",
  "Diagnosed_Other",
  "Diagnosed_None",
  "Health_Own",
  "Health_Relative",
  "Health_Relative_BelowAverage",
  "Health_Relative_AboveAverage",
  "Health_Weight",
  "Health_Smoke",
  "Health_Drink",
  "Health_Exercise",
  "Health_Sunscreen",
  "Health_Lifestyle",
  "Health_PurchaseProducts",
  "Health_PurchaseBehaviour",
  "Response_RealLife",
  "Response_Information",
  "Response_Accurate",
  "Response_Confident",
  "Response_HBP_Before",
  "Response_HBP_After",
  "Response_HBP_Change",
  "Health_Cost",
  "Health_Cost_OwnPocket",
  "Urban",
  "Certainty",
  "Spike",

  "Weights_Austria",
  "Weights_Belgium",
  "Weights_Canada",
  "Weights_Slovenia",
  "Weights_UnitedKingdom",
  "Weights_Finland",
  "Weights_Greece",
  "Weights_Portugal",

  "Bid1_Numeric_PPP",
  "Bid2_Numeric_PPP",
  "INCFI1_Numeric_PlusMissing_PPP",
  "INCGR1_Numeric_PlusMissing_PPP",
  "INCPT1_Numeric_PlusMissing_PPP",

  "INCAT1_Numeric_PlusMissing_PPP",
  "INCUK1_Numeric_PlusMissing_PPP",
  "INCCA1_Numeric_PlusMissing_PPP",
  "INCSI1_Numeric_PlusMissing_PPP",
  "INCBE1_Always_Numeric_PlusMissing_PPP",
  "INC1_All_Adjusted",
  "LogIncome",
  "LogBid1",
  "LogBid2",
  "INC1_MissingDummy",

  "BIDTOT1_Numeric",
  "BIDTOT2_Numeric",
  "BIDTOT3_Numeric",
  "BIDTOT1_PPP",
  "BIDTOT2_PPP",
  "BIDTOT3_PPP"
)


## Drop redundant columns
Data_Rest_Trimmed <- Data_Rest[, CheckNames]


# *************************************************************
#### Section 22: Prep Data_US ####
# *************************************************************

## First this will tell us which column names are NOT present
CheckNames %>% sapply(function(col) col %in% colnames(Data_US)) %>%
  .[. == FALSE] %>%
  names()


Data_US_Trimmed <- Data_US[, CheckNames]


## Row bind
Data_AllCountries <- rbind(Data_Rest_Trimmed,
                           Data_US_Trimmed)


Data_AllCountries$UnitedStates <- ifelse(Data_AllCountries$Country == "United States", 1, 0)


# *************************************************************
#### Section 23: Misc fixes ####
# *************************************************************

## Rescale risk reduction to be 0 - 1 i.e. 1 in 1000 or 2 in 1000
Data_AllCountries$RR_Scaled <- Data_AllCountries$RR  %>% divide_by(1000)


Data_AllCountries$BASE_Numeric_Scaled <- Data_AllCountries$BASE_Numeric  %>% divide_by(1000)


## Create new country weighted variables
Data_AllCountries$Canada_Weighted <- (Data_AllCountries$Canada * Data_AllCountries$weight)


Data_AllCountries$Austria_Weighted <- (Data_AllCountries$Austria * Data_AllCountries$weight)


Data_AllCountries$Belgium_Weighted <- (Data_AllCountries$Belgium * Data_AllCountries$weight)


Data_AllCountries$Slovenia_Weighted <- (Data_AllCountries$Slovenia * Data_AllCountries$weight)


Data_AllCountries$UnitedKingdom_Weighted <- (Data_AllCountries$UnitedKingdom * Data_AllCountries$weight)


Data_AllCountries$Finland_Weighted <- (Data_AllCountries$Finland * Data_AllCountries$weight)


Data_AllCountries$Greece_Weighted <- (Data_AllCountries$Greece * Data_AllCountries$weight)


Data_AllCountries$Portugal_Weighted <- (Data_AllCountries$Portugal * Data_AllCountries$weight)


Data_AllCountries$UnitedStates_Weighted <- (Data_AllCountries$UnitedStates  * Data_AllCountries$weight)


Data_AllCountries <- Data_AllCountries %>%
  mutate(Q5a = as.numeric(na_if(Q5a, "I don't know"))) %>%
  mutate(Q5a = ifelse(is.na(Q5a), 0, Q5a)) %>%
  mutate(Q6a = as.numeric(na_if(Q6a, "I don't know"))) %>%
  mutate(Q6a = ifelse(is.na(Q6a), 0, Q6a)) %>%
  mutate(Q6b = as.numeric(na_if(Q6b, "I don't know"))) %>%
  mutate(Q6b = ifelse(is.na(Q6b), 0, Q6b)) %>%
  mutate(Q6c = as.numeric(na_if(Q6c, "I don't know"))) %>%
  mutate(Q6c = ifelse(is.na(Q6c), 0, Q6c))


# Data$Q5a_Numeric <- Data$Q5a %>% as.numeric()
# Data$Q6a_Numeric <- Data$Q6a %>% as.numeric()
# Data$Q6b_Numeric <- Data$Q6b %>% as.numeric()
# Data$Q6c_Numeric <- Data$Q6c %>% as.numeric()


# *************************************************************
#### Section 24: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_AllCountries %>%
  fwrite(sep = ",",
         WORK("Data_AllCountries_Step3.csv"))


# END OF SCRIPT ****************************

## OLD MISC CODE FIXING THE MISSING INCOME IN GREECE ISSUE

# Data_ELPTFI <- here("Data",
#                     "Data_ELPTFI_Cleaned_Step2PPP.csv") %>%
#   fread() %>%
#   data.frame()
#
# Data_ABCSUKGRFIPT <- here("Data",
#                           "Data_AllCountries_Step3.csv") %>%
#   fread() %>%
#   data.frame()
#
#
#
# # Add columns with default 0
# Data_ABCSUKGRFIPT <- Data_ABCSUKGRFIPT %>%
#   mutate(
#     INCGR1_MissingDummy = 0,
#     INCGR2_MissingDummy = 0,
#     INCGR1 = 0,
#     INCGR1_Numeric = 0,
#     INCGR1_Numeric_PlusMissing = 0,
#     INCGR1_Numeric_PlusMissing_PPP = 0,
#     INCGR2 = 0,
#     INCGR2_Numeric = 0,
#     INCGR2_Numeric_PlusMissing = 0,
#     INCGR2_Numeric_PlusMissing_PPP = 0
#   )
#
#
# # Update values for Greek observations only
# greek_indices <- which(Data_ABCSUKGRFIPT$Country == "Greece")
# greek_data_indices <- which(Data_ELPTFI$Country == "Greece")
#
# # Assuming same order of Greek observations in both dataframes
# # Data_ABCSUKGRFIPT$INCGR1_MissingDummy[greek_indices] <- Data_ELPTFI$INCGR1_MissingDummy[greek_data_indices]
# # Data_ABCSUKGRFIPT$INCGR2_MissingDummy[greek_indices] <- Data_ELPTFI$INCGR2_MissingDummy[greek_data_indices]
#
# Data_ABCSUKGRFIPT$INCGR1_MissingDummy[greek_indices] <- Data_ELPTFI$INCGR1_MissingDummy[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR2_MissingDummy[greek_indices] <- Data_ELPTFI$INCGR2_MissingDummy[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR1[greek_indices] <- Data_ELPTFI$INCGR1[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR1_Numeric[greek_indices] <- Data_ELPTFI$INCGR1_Numeric[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR1_Numeric_PlusMissing[greek_indices] <- Data_ELPTFI$INCGR1_Numeric_PlusMissing[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR1_Numeric_PlusMissing_PPP[greek_indices] <- Data_ELPTFI$INCGR1_Numeric_PlusMissing_PPP[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR2[greek_indices] <- Data_ELPTFI$INCGR2[greek_data_indices]
# Data_ABCSUKGRFIPT$INCGR2_Numeric[greek_indices] <- Data_ELPTFI$INCGR2_Numeric[greek_data_indices]
#
#
#
# Data_ABCSUKGRFIPT %>%
#   fwrite(sep = ",",
#          here("Data",
#               "Data_AllCountries_Step3.csv"))
