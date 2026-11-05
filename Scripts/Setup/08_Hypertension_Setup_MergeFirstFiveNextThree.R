#### Hypertension: Merge First Five Next Three ####
# Purpose: Impute income in the second wave, convert purchasing power and merge the first eight countries.
# Author: Anonymised for peer review.
# Prerequisites: Run scripts 05–07.
# Run from the project root: source("Scripts/Setup/08_Hypertension_Setup_MergeFirstFiveNextThree.R", encoding = "UTF-8")
# Inputs:
#   Data/Intermediate/AllCountries_CleanedData_PPP.csv.
#   Data/Intermediate/AllCountries_Main_EL_PT_FI_Data_Cleaned.csv and AllCountries_Main_EL_PT_FI_Timers_Cleaned.csv.
# Outputs:
#   Data/Intermediate/Data_ABCSUKGRFIPT_Step3.csv (merged eight-country data).
#   Data/Intermediate/Data_ELPTFI_Cleaned_Step1.csv and AllCountries_CleanedData_Step1.csv.
#   Data/Intermediate/Data_ELPTFI_Cleaned_Step2PPP.csv and Data_ABCSUK_Cleaned_Step2PPP.csv.
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
Data_ELPTFI <-
  WORK("AllCountries_Main_EL_PT_FI_Data_Cleaned.csv") %>%
  fread() %>%
  data.frame()


Data_ABCSUK <- WORK("AllCountries_CleanedData_PPP.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 5: Add ELPTFI Timers ####
# *************************************************************

Data_ELPTFI_Timers <- WORK("AllCountries_Main_EL_PT_FI_Timers_Cleaned.csv") %>%
  fread() %>%
  data.frame()


Data_ELPTFI$Duration <-
  Data_ELPTFI_Timers[2:89] %>% rowSums(na.rm = TRUE)


Data_ELPTFI$Speeders_Survey_CompletionTime <- Data_ELPTFI_Timers$Speeders_Survey_CompletionTime


Data_ELPTFI$Speeders_Survey_CountryThreshold <- Data_ELPTFI_Timers$Speeders_Survey_CountryThreshold


Data_ELPTFI$Speeders_Survey_TestDummy <- Data_ELPTFI_Timers$Speeders_Survey_TestDummy


Data_ELPTFI$Slowers_Survey_CountryThreshold <- Data_ELPTFI_Timers$Slowers_Survey_CountryThreshold


Data_ELPTFI$Slowers_Survey_TestDummy <- Data_ELPTFI_Timers$Slowers_Survey_TestDummy


Data_ELPTFI$Speeders_Valuation_CompletionTime <- Data_ELPTFI_Timers$Speeders_Valuation_CompletionTime


Data_ELPTFI$Speeders_Valuation_CountryThreshold <- Data_ELPTFI_Timers$Speeders_Valuation_CountryThreshold


Data_ELPTFI$Speeders_Valuation_TestDummy <- Data_ELPTFI_Timers$Speeders_Valuation_TestDummy


Data_ELPTFI$Slowers_Valuation_CountryThreshold <- Data_ELPTFI_Timers$Slowers_Valuation_CountryThreshold


Data_ELPTFI$Slowers_Valuation_TestDummy <- Data_ELPTFI_Timers$Slowers_Valuation_TestDummy


# *************************************************************
#### Section 6: Update ELPTFI ####
# *************************************************************


# Misc dummies -------------------------------------------------------------


## From character to number
Data_ELPTFI$Kids_Numeric <- Data_ELPTFI$KIDS02 %>% as.numeric()


## Categorising partner or no
Data_ELPTFI$MarriedDummy <- ifelse(
  !Data_ELPTFI$S4 %in%
    c("Not in a relationship",
      "Other"),
  1,
  0)


Data_ELPTFI$EmployDummy_Employed <-
  ifelse(Data_ELPTFI$EMP01 %in% c("Employed full-time",
                                  "Self employed"),
         1,
         0)


Data_ELPTFI$EmployDummy_Retired <-
  ifelse(Data_ELPTFI$EMP01 %in% c("Retired"),
         1,
         0)


Data_ELPTFI$EmployDummy_PT <-
  ifelse(Data_ELPTFI$EMP01 %in% c("Employed part-time"),
         1,
         0)


Data_ELPTFI$Dummy_Female <-
  ifelse(Data_ELPTFI$GENDER_NonBinary %in% c("Female"),
         1,
         0)


Data_ELPTFI$HH_PersonsNumeric <-
  ifelse(Data_ELPTFI$SE3 %in% c("7 or more"),
         7,
         Data_ELPTFI$SE3) %>% as.numeric()


# Age group recoding
Data_ELPTFI$Age_1829Dummy <-
  ifelse(Data_ELPTFI$AgeGroup %in% c("18-29"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$Age_3044Dummy <-
  ifelse(Data_ELPTFI$AgeGroup %in% c("30-44"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$Age_4560Dummy <-
  ifelse(Data_ELPTFI$AgeGroup %in% c("45-60"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$Age_60Dummy <-
  ifelse(Data_ELPTFI$AgeGroup %in% c("60+"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeGroup_Numeric <- dplyr::recode(Data_ELPTFI$AgeGroup,
                                       '18-29' = 23.50,
                                       '30-44' = 37,
                                       '45-60' = 52.5,
                                       '60+' = 60)


# BY QUOTA -------------------------------------------------------------
Data_ELPTFI$AgeQuota_1826 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("18-26"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeQuota_2734 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("27-34"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeQuota_3539 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("35-39"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeQuota_4044 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("40-44"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeQuota_4559 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("45-59"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeQuota_6065 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("60-65"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$AgeQuota_65 <-
  ifelse(Data_ELPTFI$AgeQuota %in% c("65+"),
         1,
         0) %>% as.numeric()


Data_ELPTFI$Age_1829Dummy_Adjusted <- Data_ELPTFI$Age_1829Dummy + 1


Data_ELPTFI$Age_3044Dummy_Adjusted <- Data_ELPTFI$Age_3044Dummy + 1


Data_ELPTFI$Age_4560Dummy_Adjusted <- Data_ELPTFI$Age_4560Dummy + 1


Data_ELPTFI$Age_60Dummy_Adjusted <- Data_ELPTFI$Age_60Dummy + 1


# Education dummies -------------------------------------------------------------
Data_ELPTFI$Dummy_HighEducation_Fi <-
  ifelse(Data_ELPTFI$Finland == 1 &
           Data_ELPTFI$ISCED == "High",
         1,
         0)


Data_ELPTFI$Dummy_HighEducation_EL <-
  ifelse(Data_ELPTFI$Greece == 1 &
           Data_ELPTFI$ISCED == "High",
         1,
         0)


Data_ELPTFI$Dummy_HighEducation_PT <-
  ifelse(Data_ELPTFI$Portugal == 1 &
           Data_ELPTFI$ISCED == "High",
         1,
         0)


Data_ELPTFI$EDU_All <-
  ifelse(
    Data_ELPTFI$Dummy_HighEducation_Fi == 1 |
      Data_ELPTFI$Dummy_HighEducation_EL == 1 |
      Data_ELPTFI$Dummy_HighEducation_PT == 1,
    1, 0
  )


Data_ELPTFI$RED_Numeric <- Data_ELPTFI$RED %>% as.numeric()


Data_ELPTFI$Question2 <- Data_ELPTFI$FollowUp_Numeric


## Dummy categorise education levels
Data_ELPTFI$EducationCategory <- Data_ELPTFI$ISCED


Data_ELPTFI$EducationCategory_Dummy_High <- ifelse(
  Data_ELPTFI$ISCED == "High", 1, 0)


Data_ELPTFI$EducationCategory_Dummy_Medium <- ifelse(
  Data_ELPTFI$ISCED == "Medium", 1, 0)


Data_ELPTFI$EducationCategory_Dummy_Low <- ifelse(
  Data_ELPTFI$ISCED == "Low", 1, 0)


# *************************************************************
#### Section 7: Recoding health questions ####
# *************************************************************

# Q02 and DX
## Yes if heard of high blood pressure, zero if not
Data_ELPTFI$Health_HeardOfHBP <- ifelse(
  Data_ELPTFI$Q02 == "Yes", 1, 0)


## Yes if they had high blood pressure
Data_ELPTFI$Health_HadHBP <- ifelse(
  Data_ELPTFI$Q03 == "Yes", 1, 0)


# Before answering this survey, did you know about the health risks associated with untreated high blood pressure?
Data_ELPTFI$Health_HBP_D1 <- ifelse(
  Data_ELPTFI$D1 == "No, I did not know", 0, 1)


# Before answering this survey, did you know that high blood pressure could be treated by having a healthier lifestyle or medical treatments?
Data_ELPTFI$Health_HBP_D2 <- ifelse(
  Data_ELPTFI$D2 == "No, I did not know", 1, 0)


## Diagnosed and stuck to it
Data_ELPTFI$Health_HBP_DiagnosedStuck <- ifelse(
  Data_ELPTFI$D2 == "Yes", 1, 0)


Data_ELPTFI$Health_HBP_SideEffects <- ifelse(
  Data_ELPTFI$D2 == "Yes", 1, 0)


# Dummy coding knowledge of risks
## Can do faster with %>% mutate() but more legible here
## Recoding Q04 here
Data_ELPTFI$Risks_Genetic <- ifelse(
  Data_ELPTFI$Q04_1 == "Yes", 1, 0)


Data_ELPTFI$Risks_Obesity <- ifelse(
  Data_ELPTFI$Q04_2 == "Yes", 1, 0)


Data_ELPTFI$Risks_Alcohol <- ifelse(
  Data_ELPTFI$Q04_3 == "Yes", 1, 0)


Data_ELPTFI$Risks_Exposure <- ifelse(
  Data_ELPTFI$Q04_4 == "Yes", 1, 0)


Data_ELPTFI$Risks_Insulin <- ifelse(
  Data_ELPTFI$Q04_5 == "Yes", 1, 0)


Data_ELPTFI$Risks_Aging <- ifelse(
  Data_ELPTFI$Q04_6 == "Yes", 1, 0)


Data_ELPTFI$Risks_Exercise <- ifelse(
  Data_ELPTFI$Q04_7 == "Yes", 1, 0)


Data_ELPTFI$Risks_Smoking <- ifelse(
  Data_ELPTFI$Q04_8 == "Yes", 1, 0)


Data_ELPTFI$Risks_Stress <- ifelse(
  Data_ELPTFI$Q04_9 == "Yes", 1, 0)


Data_ELPTFI$Risks_Deprive <- ifelse(
  Data_ELPTFI$Q04_10 == "Yes", 1, 0)


# Recoding H series Qs


# How do you rate your risk of developing high blood pressure compared to people of your age and gender?
Data_ELPTFI$HBP_RelativeRiskHigh <- ifelse(
  Data_ELPTFI$H2 %in%
    c("My risk is clearly larger",
      "My risk is somewhat larger"),
  1, 0)


# Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?
Data_ELPTFI$HBP_10Yr <- ifelse(
  Data_ELPTFI$H3 == "Yes",
  1, 0)


# When you choose between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle to reduce your risk instead of paying?
Data_ELPTFI$HBP_Healthier <- ifelse(
  Data_ELPTFI$H4 == "Yes",
  1, 0)


# Please consider the statement: “I would pay almost anything to improve my health even by a small amount”. Do you…
Data_ELPTFI$HBP_PayAnything <- ifelse(
  Data_ELPTFI$H5 == "…agree",
  1, 0)


# Is high blood pressure a genetic condition in your family?
Data_ELPTFI$HBP_Genetic <- ifelse(
  Data_ELPTFI$H6 == "Yes",
  1, 0)


# Do you know of people close to you (family, friends) who have, or have had, high blood pressure?
Data_ELPTFI$HBP_Family <- ifelse(
  Data_ELPTFI$H7 == "Yes",
  1, 0)


# I have been diagnosed with: - [disease name]
Data_ELPTFI$Diagnosed_KidneyDisease <- ifelse(Data_ELPTFI$H8_1 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_HeartDisease <- ifelse(Data_ELPTFI$H8_2 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_Obesity <- ifelse(Data_ELPTFI$H8_3 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_Diabetes <- ifelse(Data_ELPTFI$H8_4 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_ParkinsonDisease <- ifelse(Data_ELPTFI$H8_5 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_AlzheimersDementias <- ifelse(Data_ELPTFI$H8_6 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_RheumatoidArthritis <- ifelse(Data_ELPTFI$H8_7 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_Respiratory <- ifelse(Data_ELPTFI$H8_8 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_Other <- ifelse(Data_ELPTFI$H8_9 == "Yes", 1, 0)


Data_ELPTFI$Diagnosed_None <- ifelse(Data_ELPTFI$H8_10 == "Yes", 1, 0)


# How would you describe your health in general?
Data_ELPTFI$Health_Own <- dplyr::recode(
  Data_ELPTFI$H9,
  "Excellent" = 5,
  "Good" = 4,
  "Fair" = 3,
  "Poor" = 2,
  "Very poor" = 1,
  "Prefer not to answer" = 0
)


# How would you describe your health relative to others of your age and gender in your country?
Data_ELPTFI$Health_Relative <- dplyr::recode(
  Data_ELPTFI$H10,
  "Well above average" = 5,
  "Above average" = 4,
  "Average" = 3,
  "Below average" = 2,
  "Well below average" = 1,
  "Prefer not to answer" = 0
)


## Classify
Data_ELPTFI$Health_Relative_BelowAverage <- ifelse(Data_ELPTFI$Health_Relative == 2, 1, 0)


Data_ELPTFI$Health_Relative_AboveAverage <- ifelse(Data_ELPTFI$Health_Relative == 4, 1, 0)


# How would you describe your weight relative to others of your height, age and gender in your country?
Data_ELPTFI$Health_Weight <- dplyr::recode(
  Data_ELPTFI$H11,
  "Less than average" = 3,
  "Average" = 2,
  "More than average" = 1,
  "Prefer not to answer" = 0
)


# Do you smoke?
Data_ELPTFI$Health_Smoke <- dplyr::recode(
  Data_ELPTFI$H12,
  "No" = 3,
  "Yes, occasionally" = 2,
  "Yes, regularly" = 1,
  "Prefer not to answer" = 0
)


# How many alcoholic drinks do you usually have in a normal week?
Data_ELPTFI$Health_Drink  <- dplyr::recode(
  Data_ELPTFI$H13,
  "I usually don’t drink alcohol" = 1,
  "1-4 drinks per week" = 2,
  "5-9 drinks per week" = 3,
  "More than 9 drinks per week" = 4,
  "999999" = 0
)


# How often do you exercise in a normal week? Please pick the answer that best describes a normal week.
Data_ELPTFI$Health_Exercise  <- dplyr::recode(
  Data_ELPTFI$H14,
  "More than 5 times per week" = 5,
  "3-5 times per week"       = 4,
  "1-2 times per week" = 3,
  "Less than 1 time per week" = 2,
  "I don’t exercise" = 1,
  "Prefer not to answer"     = 0)


# Do you use sunscreen?
Data_ELPTFI$Health_Sunscreen <- dplyr::recode(
  Data_ELPTFI$H15,
  "Yes, regularly" = 3,
  "Yes, occasionally" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Do you consider yourself to have a healthy lifestyle (exercise regularly, eat healthily, sleep more than 7 hours a night...)?
Data_ELPTFI$Health_Lifestyle <- dplyr::recode(
  Data_ELPTFI$H16,
  "Yes" = 4,
  "In general yes" = 3,
  "In general no" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Take a moment to think about the products you typically purchase. How do you feel about them?
Data_ELPTFI$Health_PurchaseProducts <- dplyr::recode(
  Data_ELPTFI$H17,
  "Totally safe for people’s health" = 4 ,
  "Rather safe for people’s health" = 3 ,
  "I don’t know" = 2,
  "Rather unsafe for people’s health" = 1,
  "Totally unsafe for people’s health" = 0
)


# Which of the following statements best describes your purchasing behaviour?
Data_ELPTFI$Health_PurchaseBehaviour <- dplyr::recode(
  Data_ELPTFI$H18,
  "I try to buy safer products even if it comes at an extra cost (e.g. by choosing safer products even if they are a bit mo" = 3,
  "I try to buy safer products if it doesn’t come at an extra cost (e.g. by choosing safer products if they are similar i" = 2,
  "I don’t know" = 1,
  "I do not pay much attention to the products’ effect on my health" = 0
)


Data_ELPTFI$Response_HBP_Change <-
  (Data_ELPTFI$Response_HBP_After -
     Data_ELPTFI$Response_HBP_Before)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
Data_ELPTFI$Health_Cost <- ifelse(
  Data_ELPTFI$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket",
      "A private health insurance scheme (e.g. through your employer) would cover all or most of the costs" ),
  1, 0)


## Own pocked specifically
Data_ELPTFI$Health_Cost_OwnPocket <- ifelse(
  Data_ELPTFI$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket"),
  1, 0)


## SE2 categorises nicely but let's add a dummy
Data_ELPTFI$Urban <- ifelse(
  Data_ELPTFI$SE2 == "Urban",
  1, 0)


# How certain are you of the maximum value you just indicated?
Data_ELPTFI$Certainty <- dplyr::recode(
  Data_ELPTFI$Q7,
  "Strongly certain" = 4,
  "Somewhat certain" = 3,
  "Neither certain nor uncertain" = 2,
  "Somewhat uncertain" = 1,
  "Strongly uncertain" = 0
)


## Define here for later
Data_ELPTFI$Spike <- ifelse(
  Data_ELPTFI$Q4 %in% c("No, I would not pay anything"),
  0, 1
)


# *************************************************************
#### Section 8: Weights ####
# *************************************************************


## Simple interactions with existing weights
Data_ELPTFI$Weights_Austria <- 0


Data_ELPTFI$Weights_Belgium <- 0


Data_ELPTFI$Weights_Canada <- 0


Data_ELPTFI$Weights_Slovenia <- 0


Data_ELPTFI$Weights_UnitedKingdom <- 0


Data_ELPTFI$Weights_Finland <- ((Data_ELPTFI$weight %>% as.numeric()) * (Data_ELPTFI$Finland %>% as.numeric()))


Data_ELPTFI$Weights_Greece <- ((Data_ELPTFI$weight %>% as.numeric()) * Data_ELPTFI$Greece %>% as.numeric())


Data_ELPTFI$Weights_Portugal <- ((Data_ELPTFI$weight %>% as.numeric()) * Data_ELPTFI$Portugal %>% as.numeric())


Data_ELPTFI$Austria <- 0


Data_ELPTFI$Belgium <- 0


Data_ELPTFI$Canada <- 0


Data_ELPTFI$Slovenia <- 0


Data_ELPTFI$UnitedKingdom <- 0


Data_ELPTFI$Dummy_HighEducation_UK <- 0


Data_ELPTFI$Dummy_HighEducation_AUT <- 0


Data_ELPTFI$Dummy_HighEducation_Bel <- 0


Data_ELPTFI$Dummy_HighEducation_Slo <- 0


Data_ELPTFI$Dummy_HighEducation_CAD <- 0


# *************************************************************
#### Section 9: Scaling ####
# *************************************************************


# Income recoding for income question 1 -------------------------------------------------------------


## Recode Austrian missing data
Data_ELPTFI$INCFI1_MissingDummy <-
  ifelse(
    Data_ELPTFI$INCFI1 %in%
      c("En halua sanoa",
        "En tiedä"
      ),
    1,
    0)


## Recode Finlands missing data
Data_ELPTFI$INCFI2_MissingDummy <-
  ifelse(
    Data_ELPTFI$INCFI2 %in%
      c("En halua sanoa",
        "En tiedä"
      ),
    1,
    0)


## Update GREECE to use actual language
Data_ELPTFI$INCGR1_MissingDummy <-
  ifelse(
    Data_ELPTFI$INCGR1 %in%
      c("En halua sanoa",
        "En tiedä"
      ),
    1,
    0)


Data_ELPTFI$INCGR2_MissingDummy <-  ifelse(
  Data_ELPTFI$INCGR2 %in%
    c("En halua sanoa",
      "En tiedä"
    ),
  1,
  0)


## Portugal
Data_ELPTFI$INCPT1_MissingDummy <- ifelse(
  Data_ELPTFI$INCPT1 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


Data_ELPTFI$INCPT2_MissingDummy <- ifelse(
  Data_ELPTFI$INCPT2 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


Data_ELPTFI$INCBE1_Always <- 0


Data_ELPTFI$INCBE2_Always <- 0


Data_ELPTFI$INCAT1_MissingDummy <- 0


Data_ELPTFI$INCUK1_MissingDummy <- 0


Data_ELPTFI$INCCA1_MissingDummy <- 0


Data_ELPTFI$INCSI1_MissingDummy <- 0


Data_ELPTFI$INCBE1_Always_MissingDummy <- 0


Data_ELPTFI$INCAT2_MissingDummy <- 0


Data_ELPTFI$INCUK2_MissingDummy <- 0


Data_ELPTFI$INCCA2_MissingDummy <- 0


Data_ELPTFI$INCSI2_MissingDummy <- 0


Data_ELPTFI$INCBE2_Always_MissingDummy <- 0


# Income recoding for income question 2 -------------------------------------------------------------

Data_ELPTFI_Backup <- Data_ELPTFI


## Very large recoding exercise taking the midpoints of each range
Data_ELPTFI <- Data_ELPTFI %>%
  dplyr::mutate(

    INCFI1_Numeric = dplyr::case_when(
      is.na(INCFI1) ~ NA,
      INCFI1 %in% "0 € - 1 500 €"  ~ (0.5 * 0) + (0.5 * 1500),
      INCFI1 %in% "1 501 € - 1 800 €" ~ (0.5 * 1501) + (0.5 * 1800),
      INCFI1 %in% "1 801 € - 2 000 €" ~ (0.5 * 1801) + (0.5 * 2000),
      INCFI1 %in% "2 001 € - 2 300 €" ~ (0.5 * 2001) + (0.5 * 2300),
      INCFI1 %in% "2 301 € - 2 600 €" ~ (0.5 * 2301) + (0.5 * 2600),
      INCFI1 %in% "2 601 € - 2 900 €" ~ (0.5 * 2601) + (0.5 * 2900),
      INCFI1 %in% "2 901 € - 3 200 €" ~ (0.5 * 2901) + (0.5 * 3200),
      INCFI1 %in% "3 201 € - 3 700 €" ~ (0.5 * 3201) + (0.5 * 3700),
      INCFI1 %in% "3 701 € - 4 500 €" ~ (0.5 * 3701) + (0.5 * 4500),
      INCFI1 %in% "4 501 € - 5 500 €" ~ (0.5 * 4501) + (0.5 * 5500),
      ## Bug 3 (README V2): was 5101, the UK top-band value copy-pasted into the
      ## Finnish block. INCFI2's open top band at :864 already uses 5501 for
      ## the same label -- that is the convention this open band should follow.
      INCFI1 %in% "5 501 € tai enemmän" ~ 5501,
      INCFI1 %in% "En halua sanoa" ~ NA,
      INCFI1 %in% "En tiedä" ~ NA
    ),

    INCGR1_Numeric = dplyr::case_when(
      is.na(INCGR1) ~ NA,
      INCGR1 %in% "0 € έως 500 €" ~ (0.5 * 0) + (0.5 * 500),
      INCGR1 %in% "1 001 € έως 1 100 €" ~ (0.5 * 1001) + (0.5 * 1100),
      INCGR1 %in% "1 101 € έως 1 300 €" ~ (0.5 * 1101) + (0.5 * 1300),
      INCGR1 %in% "1 301 € έως 1 500 €" ~ (0.5 * 1301) + (0.5 * 1500),
      INCGR1 %in% "1 501 € έως 1 900 €" ~ (0.5 * 1501) + (0.5 * 1900),
      INCGR1 %in% "1 901 € έως 2 300 €" ~ (0.5 * 1901) + (0.5 * 2300),
      INCGR1 %in% "2 301 € ή περισσότερο" ~ 2301,
      INCGR1 %in% "501 € έως 600 €" ~ (0.5 * 501) + (0.5 * 600),
      INCGR1 %in% "601 € έως 700 €" ~ (0.5 * 601) + (0.5 * 700),
      INCGR1 %in% "701 € έως 900 €" ~ (0.5 * 701) + (0.5 * 900),
      INCGR1 %in% "901 € έως 1 000 €" ~ (0.5 * 901) + (0.5 * 1000),
      INCGR1 %in% "En halua sanoa" ~ NA,
      INCGR1 %in% "En tiedä"  ~ NA
    ),


    INCPT1_Numeric = dplyr::case_when(
      is.na(INCPT1) ~ NA,
      INCPT1 %in% "0 € a 600 €" ~ (0.5 * 0) + (0.5 * 600),
      INCPT1 %in% "601 € a 800 €" ~ (0.5 * 601) + (0.5 * 800),
      INCPT1 %in% "801 € a 900 €" ~ (0.5 * 801) + (0.5 * 900),
      INCPT1 %in% "1 101 € a 1 200 €" ~ (0.5 * 1101) + (0.5 * 1200),
      INCPT1 %in% "1 201 € a 1 400 €" ~ (0.5 * 1201) + (0.5 * 1400),
      INCPT1 %in% "1 401 € a 1 600 €" ~ (0.5 * 1401) + (0.5 * 1600),
      INCPT1 %in% "1 601 € a 1 900 €" ~ (0.5 * 1601) + (0.5 * 1900),
      INCPT1 %in% "1 901 € a 2 500 €" ~ (0.5 * 1901) + (0.5 * 2500),
      INCPT1 %in% "2 501 € a 3 000 €" ~ (0.5 * 2501) + (0.5 * 3000),
      INCPT1 %in% "901 € a 1 100 €" ~ (0.5 * 901) + (0.5 * 1100),
      INCPT1 %in% "3 001 € ou mais" ~ 3001,
      INCPT1 %in% "I don’t know" ~ NA,
      INCPT1 %in% "Prefer not to answer" ~ NA
    ),

    INCFI2_Numeric = dplyr::case_when(
      is.na(INCFI2) ~ NA,
      INCFI2 %in% "5 501 € tai enemmän" ~ 5501,
      INCFI2 %in% "En tiedä"            ~ NA,
      INCFI2 %in% "En halua sanoa"      ~ NA,
      INCFI2 %in% "2 901 € - 3 700 €"   ~ (0.5 * 2901) + (0.5 * 3700),
      INCFI2 %in% "0 € - 1 800 €"       ~ (0.5 * 0) + (0.5 * 1800),
      INCFI2 %in% "2 301 € - 2 900 €"   ~ (0.5 * 2301) + (0.5 * 2900),
      INCFI2 %in% "1 801 € - 2 300 €"   ~ (0.5 * 1801) + (0.5 * 2300),
      INCFI2 %in% "3 701 € - 5 500 €" ~ (0.5 * 3701) + (0.5 * 5500)
    ),

    INCGR2_Numeric = dplyr::case_when(
      is.na(INCGR2) ~ NA,
      INCGR2 %in% "En tiedä" ~ NA,
      INCGR2 %in% "601 € έως 900 €" ~ (0.5 * 601) + (0.5 * 900),
      INCGR2 %in% "1 501 € έως 2 300 €" ~ (0.5 * 1501) + (0.5 * 2300),
      INCGR2 %in% "2 301 € ή περισσότερο" ~ 2301,
      INCGR2 %in% "En halua sanoa" ~ NA,
      INCGR2 %in% "0 € έως 600 €" ~ (0.5 * 0) + (0.5 * 600),
      INCGR2 %in% "901 € έως 1 100 €" ~ (0.5 * 901) + (0.5 * 1100),
      INCGR2 %in% "1 101 € έως 1 500 €" ~ (0.5 * 1101) + (0.5 * 1500)
    ),

    INCPT2_Numeric = dplyr::case_when(
      is.na(INCPT2) ~ NA,
      INCPT2 %in% "Prefer not to answer" ~ NA,
      INCPT2 %in% "I don’t know"         ~ NA,
      INCPT2 %in% "1 901 € a 3 000 €"   ~ (0.5 * 1901) + (0.5 * 3000),
      INCPT2 %in% "1 101 € a 1 400 €"   ~ (0.5 * 1101) + (0.5 * 1400),
      INCPT2 %in% "3 001 € ou mais"     ~ 3001,
      INCPT2 %in% "0 € a 800 €"         ~ (0.5 * 0) + (0.5 * 800),
      INCPT2 %in% "1 401 € a 1 900 €"   ~ (0.5 * 1401) + (0.5 * 1900),
      INCPT2 %in% "801 € a 1 100 €" ~ (0.5 * 801) + (0.5 * 1100)
    )
  )


## TASK E -- assert every observed non-missing income label got a mapping.
## NOTE: INCGR1/INCGR2 declare FINNISH don't-know labels ("En tiedae"/"En halua
## sanoa") for GREEK respondents -- the same copy-paste family as the Canadian
## defect.  If the Greek data ever carries Greek refusal labels these assertions
## will stop the run and name them, instead of silently NA-ing the rows.
NA_LABELS_FI <- c("En halua sanoa", "En tiedä")


NA_LABELS_EN <- c("Prefer not to answer", "I don’t know")


assert_recode_family(Data_ELPTFI, list(
  INCFI1 = list(out = "INCFI1_Numeric", na = NA_LABELS_FI),
  INCGR1 = list(out = "INCGR1_Numeric", na = NA_LABELS_FI),
  INCPT1 = list(out = "INCPT1_Numeric", na = NA_LABELS_EN),
  INCFI2 = list(out = "INCFI2_Numeric", na = NA_LABELS_FI),
  INCGR2 = list(out = "INCGR2_Numeric", na = NA_LABELS_FI),
  INCPT2 = list(out = "INCPT2_Numeric", na = NA_LABELS_EN)
))


message("  [assert] 05_: all six EL/PT/FI income recodes are complete.")


## Coalesce by country: fast way
Data_ELPTFI$INCFI1_All <- coalesce(
  Data_ELPTFI$INCFI1_Numeric,
  Data_ELPTFI$INCFI2_Numeric
)


Data_ELPTFI$INCGR1_All <- coalesce(
  Data_ELPTFI$INCGR1_Numeric,
  Data_ELPTFI$INCGR2_Numeric
)


Data_ELPTFI$INCPT1_All <- coalesce(
  Data_ELPTFI$INCPT1_Numeric,
  Data_ELPTFI$INCPT2_Numeric
)


## Coalesce by country: fast way
Data_ELPTFI$INC1_All <- coalesce(
  Data_ELPTFI$INCFI1_All,
  Data_ELPTFI$INCGR1_All,
  Data_ELPTFI$INCPT1_All
)


Data_ELPTFI$INC1_Missing <- coalesce(
  Data_ELPTFI$INCFI1_MissingDummy,
  Data_ELPTFI$INCGR1_MissingDummy,
  Data_ELPTFI$INCPT1_MissingDummy
)


## Coalesce by country: fast way
Data_ELPTFI$INC2_All <- coalesce(
  Data_ELPTFI$INCFI2_Numeric,
  Data_ELPTFI$INCGR2_Numeric,
  Data_ELPTFI$INCPT2_Numeric
)


Data_ELPTFI$INC2_Missing <- coalesce(
  Data_ELPTFI$INCFI2_MissingDummy,
  Data_ELPTFI$INCGR2_MissingDummy,
  Data_ELPTFI$INCPT2_MissingDummy
)


# Fi
## Predict missing income
Model_PredictMissingIncome_FI <- lm(
  formula = log(INCFI1_All) ~
    AgeGroup +
    Dummy_HighEducation_Fi +
    MarriedDummy +
    HH_PersonsNumeric +
    EmployDummy_Retired +
    EmployDummy_PT +
    EmployDummy_Employed,
  data = Data_ELPTFI[Data_ELPTFI$Country == "Finland", ]
)


## Match imputed income by Country and Respondent_Serial, not row position.
Data_ELPTFI$INCFI1_Numeric_PlusMissing <- impute_income_keyed(
  data      = Data_ELPTFI,
  model     = Model_PredictMissingIncome_FI,
  need_mask = Data_ELPTFI$Country == "Finland" &
              Data_ELPTFI$INCFI2_MissingDummy == 1,
  reported  = Data_ELPTFI$INCFI1_All,
  label     = "Finland")


# PT
## Predict missing income
Model_PredictMissingIncome_PT <- lm(
  formula = log(INCPT1_All) ~
    AgeGroup +
    Dummy_HighEducation_PT +
    MarriedDummy +
    HH_PersonsNumeric +
    EmployDummy_Retired +
    EmployDummy_PT +
    EmployDummy_Employed,
  data = Data_ELPTFI[Data_ELPTFI$Country == "Portugal", ]
)


## Match imputed income by Country and Respondent_Serial, not row position.
Data_ELPTFI$INCPT1_Numeric_PlusMissing <- impute_income_keyed(
  data      = Data_ELPTFI,
  model     = Model_PredictMissingIncome_PT,
  need_mask = Data_ELPTFI$Country == "Portugal" &
              Data_ELPTFI$INCPT2_MissingDummy == 1,
  reported  = Data_ELPTFI$INCPT1_All,
  label     = "Portugal")


# Greece
Model_PredictMissingIncome_GR <- lm(formula = log(INCGR1_All) ~
                                      AgeGroup +
                                      Dummy_HighEducation_EL +
                                      MarriedDummy +
                                      HH_PersonsNumeric +
                                      EmployDummy_Retired +
                                      EmployDummy_PT +
                                      EmployDummy_Employed,
                                    data = Data_ELPTFI[Data_ELPTFI$Country == "Greece", ])


## Match imputed income by Country and Respondent_Serial, not row position.
Data_ELPTFI$INCGR1_Numeric_PlusMissing <- impute_income_keyed(
  data      = Data_ELPTFI,
  model     = Model_PredictMissingIncome_GR,
  need_mask = Data_ELPTFI$Country == "Greece" &
              Data_ELPTFI$INCGR2_MissingDummy == 1,
  reported  = Data_ELPTFI$INCGR1_All,
  label     = "Greece")


Data_ELPTFI$INCAT1_Numeric <- 0


Data_ELPTFI$INCUK1_Numeric <- 0


Data_ELPTFI$INCCA1_Numeric <- 0


Data_ELPTFI$INCSI1_Numeric <- 0


Data_ELPTFI$INCBE1_Always_Numeric <- 0


Data_ELPTFI$INCAT2_Numeric <- 0


Data_ELPTFI$INCUK2_Numeric <- 0


Data_ELPTFI$INCCA2_Numeric <- 0


Data_ELPTFI$INCSI2_Numeric <- 0


Data_ELPTFI$INCBE2_Always_Numeric <- 0


Data_ELPTFI$INCUK1_Numeric_PlusMissing <- 0


Data_ELPTFI$INCBE1_Always_Numeric_PlusMissing <- 0


Data_ELPTFI$INCAT1_Numeric_PlusMissing <- 0


Data_ELPTFI$INCSI1_Numeric_PlusMissing <- 0


Data_ELPTFI$INCCA1_Numeric_PlusMissing <- 0


Data_ELPTFI$INCUK1_Numeric_PlusMissing_Log <- 0


# *************************************************************
#### Section 10: Transform BIDTOT ####
# *************************************************************


Data_ABCSUK <- Data_ABCSUK %>%
  dplyr::mutate(
    BIDTOT1_Numeric = gsub("[^0-9.]", "", BIDTOT1) %>% as.numeric(),
    BIDTOT2_Numeric = gsub("[^0-9.]", "", BIDTOT2) %>% as.numeric(),
    BIDTOT3_Numeric = gsub("[^0-9.]", "", BIDTOT3) %>% as.numeric())


Data_ELPTFI <- Data_ELPTFI %>%
  dplyr::mutate(
    BIDTOT1_Numeric = gsub("[^0-9.]", "", BIDTOT1) %>% as.numeric(),
    BIDTOT2_Numeric = gsub("[^0-9.]", "", BIDTOT2) %>% as.numeric(),
    BIDTOT3_Numeric = gsub("[^0-9.]", "", BIDTOT3) %>% as.numeric())


# *************************************************************
#### Section 11: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_ELPTFI %>%
  fwrite(sep = ",",
         WORK("Data_ELPTFI_Cleaned_Step1.csv"))


Data_ABCSUK %>%
  fwrite(sep = ",",
         WORK("AllCountries_CleanedData_Step1.csv"))


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
## on its own; see Data/RAW_MANIFEST.md for the raw inputs it resolves against.
## (This script is also sourced from inside 05_, which defines the same WORK()
## -- redefining it here identically is harmless and keeps this script runnable
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
Data <- WORK("Data_ELPTFI_Cleaned_Step1.csv") %>%
  fread() %>%
  data.frame()


Data_ABCSUK <- WORK("AllCountries_CleanedData_Step1.csv") %>%
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
Data$BIDTOT1_PPP <-
  ifelse(
    Data$Country == "Finland",
    Data$BIDTOT1_Numeric %>%
      divide_by(Conversion_Factors$Finland),
    ifelse(
      Data$Country == "Portugal",
      Data$BIDTOT1_Numeric %>% divide_by(Conversion_Factors$Portugal),
      ifelse(
        Data$Country == "Greece",
        Data$BIDTOT1_Numeric %>%
          divide_by(Conversion_Factors$Greece),
        Data$BIDTOT1_Numeric
      )
    )
  )


Data$BIDTOT2_PPP <-
  ifelse(
    Data$Country == "Finland",
    Data$BIDTOT2_Numeric %>%
      divide_by(Conversion_Factors$Finland),
    ifelse(
      Data$Country == "Portugal",
      Data$BIDTOT2_Numeric %>% divide_by(Conversion_Factors$Portugal),
      ifelse(
        Data$Country == "Greece",
        Data$BIDTOT2_Numeric %>%
          divide_by(Conversion_Factors$Greece),
        Data$BIDTOT2_Numeric
      )
    )
  )


Data$BIDTOT3_PPP <-
  ifelse(
    Data$Country == "Finland",
    Data$BIDTOT3_Numeric %>%
      divide_by(Conversion_Factors$Finland),
    ifelse(
      Data$Country == "Portugal",
      Data$BIDTOT3_Numeric %>% divide_by(Conversion_Factors$Portugal),
      ifelse(
        Data$Country == "Greece",
        Data$BIDTOT3_Numeric %>%
          divide_by(Conversion_Factors$Greece),
        Data$BIDTOT3_Numeric
      )
    )
  )


# *************************************************************
#### Section 18: BIDTOT levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
Data_ABCSUK$BIDTOT1_PPP <-
  ifelse(
    Data_ABCSUK$Country == "Austria",
    Data_ABCSUK$BIDTOT1_Numeric %>%
      divide_by(Conversion_Factors$Austria),
    ifelse(
      Data_ABCSUK$Country == "Belgium",
      Data_ABCSUK$BIDTOT1_Numeric %>% divide_by(Conversion_Factors$Belgium),
      ifelse(
        Data_ABCSUK$Country == "Canada",
        Data_ABCSUK$BIDTOT1_Numeric %>%
          divide_by(Conversion_Factors$Canada),
        ifelse(
          Data_ABCSUK$Country == "Slovenia",
          Data_ABCSUK$BIDTOT1_Numeric %>%
            divide_by(Conversion_Factors$Slovenia),
          ifelse(
            Data_ABCSUK$Country == "United Kingdom",
            Data_ABCSUK$BIDTOT1_Numeric %>%
              divide_by(Conversion_Factors$United.Kingdom),
            Data_ABCSUK$BIDTOT1_Numeric
          )))))


Data_ABCSUK$BIDTOT2_PPP <-
  ifelse(
    Data_ABCSUK$Country == "Austria",
    Data_ABCSUK$BIDTOT2_Numeric %>%
      divide_by(Conversion_Factors$Austria),
    ifelse(
      Data_ABCSUK$Country == "Belgium",
      Data_ABCSUK$BIDTOT2_Numeric %>% divide_by(Conversion_Factors$Belgium),
      ifelse(
        Data_ABCSUK$Country == "Canada",
        Data_ABCSUK$BIDTOT2_Numeric %>%
          divide_by(Conversion_Factors$Canada),
        ifelse(
          Data_ABCSUK$Country == "Slovenia",
          Data_ABCSUK$BIDTOT2_Numeric %>%
            divide_by(Conversion_Factors$Slovenia),
          ifelse(
            Data_ABCSUK$Country == "United Kingdom",
            Data_ABCSUK$BIDTOT2_Numeric %>%
              divide_by(Conversion_Factors$United.Kingdom),
            Data_ABCSUK$BIDTOT2_Numeric
          )))))


Data_ABCSUK$BIDTOT3_PPP <-
  ifelse(
    Data_ABCSUK$Country == "Austria",
    Data_ABCSUK$BIDTOT3_Numeric %>%
      divide_by(Conversion_Factors$Austria),
    ifelse(
      Data_ABCSUK$Country == "Belgium",
      Data_ABCSUK$BIDTOT3_Numeric %>% divide_by(Conversion_Factors$Belgium),
      ifelse(
        Data_ABCSUK$Country == "Canada",
        Data_ABCSUK$BIDTOT3_Numeric %>%
          divide_by(Conversion_Factors$Canada),
        ifelse(
          Data_ABCSUK$Country == "Slovenia",
          Data_ABCSUK$BIDTOT3_Numeric %>%
            divide_by(Conversion_Factors$Slovenia),
          ifelse(
            Data_ABCSUK$Country == "United Kingdom",
            Data_ABCSUK$BIDTOT3_Numeric %>%
              divide_by(Conversion_Factors$United.Kingdom),
            Data_ABCSUK$BIDTOT3_Numeric
          )))))


# *************************************************************
#### Section 19: Bid levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
Data$Bid1_Numeric_PPP <-
    ifelse(
      Data$Country == "Finland",
      Data$Bid1_Numeric %>%
        divide_by(Conversion_Factors$Finland),
      ifelse(
        Data$Country == "Portugal",
        Data$Bid1_Numeric %>% divide_by(Conversion_Factors$Portugal),
        ifelse(
          Data$Country == "Greece",
          Data$Bid1_Numeric %>%
            divide_by(Conversion_Factors$Greece),
          Data$Bid1_Numeric
        )
      )
    )


## Bid levels for the follow up question
Data$Bid2_Numeric_PPP <-
  ifelse(
    Data$Country == "Finland",
    Data$FollowUp_Bid %>%
      divide_by(Conversion_Factors$Finland),
    ifelse(
      Data$Country == "Portugal",
      Data$FollowUp_Bid %>% divide_by(Conversion_Factors$Portugal),
      ifelse(
        Data$Country == "Greece",
        Data$FollowUp_Bid %>%
          divide_by(Conversion_Factors$Greece),
        Data$FollowUp_Bid
      )
    )
  )


# *************************************************************
#### Section 20: Income levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
Data$INCFI1_Numeric_PlusMissing_PPP <- Data$INCFI1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Finland)


Data$INCGR1_Numeric_PlusMissing_PPP <- Data$INCGR1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Greece)


Data$INCPT1_Numeric_PlusMissing_PPP <- Data$INCPT1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Portugal)


## Coalesce stitches things together when there are NAs
Data$INC1_All_Adjusted <- coalesce(
  Data$INCFI1_Numeric_PlusMissing_PPP,
  Data$INCGR1_Numeric_PlusMissing_PPP,
  Data$INCPT1_Numeric_PlusMissing_PPP
)


## Log monetary variables
Data$LogIncome <- Data$INC1_All_Adjusted %>% log()


Data$LogBid1 <- Data$Bid1_Numeric_PPP %>% log()


Data$LogBid2 <- Data$Bid2_Numeric_PPP %>% log()


# This will properly combine all missing indicators
Data$INC1_MissingDummy <- ifelse(
  Data$INCFI1_MissingDummy == 1 |
    Data$INCGR1_MissingDummy == 1 |
    Data$INCPT1_MissingDummy == 1,
  1, 0)


Data$INC2_MissingDummy <- ifelse(
  Data$INCFI2_MissingDummy == 1 |
    Data$INCGR2_MissingDummy == 1 |
    Data$INCPT2_MissingDummy == 1,
  1, 0)


# ## Missing income indicators here
# Data$INC1_MissingDummy <- coalesce(
#   Data$INCFI1_MissingDummy,
#   Data$INCGR1_MissingDummy,
#   Data$INCPT1_MissingDummy
# )


# *************************************************************
#### Section 21: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data %>%
  fwrite(sep = ",",
         WORK("Data_ELPTFI_Cleaned_Step2PPP.csv"))


Data_ABCSUK %>%
  fwrite(sep = ",",
         WORK("Data_ABCSUK_Cleaned_Step2PPP.csv"))


# END OF SCRIPT **************************************************************


Data_ELPTFI <- WORK("Data_ELPTFI_Cleaned_Step2PPP.csv") %>%
  fread() %>%
  data.frame()


Data_ABCSUK <- WORK("Data_ABCSUK_Cleaned_Step2PPP.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 22: Merge with original ####
# *************************************************************


Data_ELPTFI$INCAT1_Numeric_PlusMissing_PPP <- 0


Data_ELPTFI$INCUK1_Numeric_PlusMissing_PPP <- 0


Data_ELPTFI$INCCA1_Numeric_PlusMissing_PPP <- 0


Data_ELPTFI$INCSI1_Numeric_PlusMissing_PPP <- 0


Data_ELPTFI$INCBE1_Always_Numeric_PlusMissing_PPP <- 0


#
#
# ## Recode UK missing data
# Data_ABCSUK$INCUK1_MissingDummy <-
#   ifelse(
#     Data_ABCSUK$INCUK1 %in%
#       c("I don’t know",
#         "Prefer not to answer"
#       ),
#     1,
#     0)
#
# ## Slovenia
# Data_ABCSUK$INCSI1_MissingDummy <-
#   ifelse(
#     Data_ABCSUK$INCSI1 %in%
#       c("Ne vem",
#         "Raje ne povem"
#       ),
#     1,
#     0)
#
# ## Canada
# Data_ABCSUK$INCCA1_MissingDummy <-
#   ifelse(
#     Data_ABCSUK$INCCA1 %in%
#       c("I don’t know",
#         "Prefer not to answer"
#       ),
#     1,
#     0)
#
#
# ## Belgium
# Data_ABCSUK$INCBE1_Always_MissingDummy <-
#   ifelse(
#     Data_ABCSUK$INCBE1_Always %in%
#       c("Ik weet het niet",
#         "Ik zeg liever niet",
#         "Je ne sais pas.",
#         "Je préfère ne pas répondre."
#       ),
#     1,
#     0)
#

# This will properly combine all missing indicators
Data_ABCSUK$INC1_MissingDummy <- ifelse(
  Data_ABCSUK$INCUK1_MissingDummy == 2 | ## UK Was coded 1,2 not 0,1
    Data_ABCSUK$INCAT1_MissingDummy == 1 |
    Data_ABCSUK$INCSI1_MissingDummy == 1 |
    Data_ABCSUK$INCCA1_MissingDummy == 1 |
    Data_ABCSUK$INCBE1_Always_MissingDummy == 1,
  1, 0)


Data_ABCSUK$INC2_MissingDummy <- ifelse(
  Data_ABCSUK$INCUK2_MissingDummy == 1 |
    Data_ABCSUK$INCAT2_MissingDummy == 1 |
    Data_ABCSUK$INCSI2_MissingDummy == 1 |
    Data_ABCSUK$INCCA2_MissingDummy == 1 |
    Data_ABCSUK$INCBE2_Always_MissingDummy == 1,
  1, 0)


Data_ABCSUK$Finland <- 0


Data_ABCSUK$Greece <- 0


Data_ABCSUK$Portugal <- 0


Data_ABCSUK$Weights_Finland <- 0


Data_ABCSUK$Weights_Greece <- 0


Data_ABCSUK$Weights_Portugal <- 0


Data_ABCSUK$INCFI1_Numeric_PlusMissing_PPP <- 0


Data_ABCSUK$INCGR1_Numeric_PlusMissing_PPP <- 0


Data_ABCSUK$INCPT1_Numeric_PlusMissing_PPP <- 0


## Drop redundant columns
Data_ABCSUK_Trimmed <- Data_ABCSUK[, c(
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
)]


Data_ELPTFI_Trimmed <- Data_ELPTFI[, c(
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
)]


## Row bind
Data_ABCSUKGRFIPT <- rbind(Data_ABCSUK_Trimmed,
                           Data_ELPTFI_Trimmed)


# *************************************************************
#### Section 23: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_ABCSUKGRFIPT %>%
  fwrite(sep = ",",
         WORK("Data_ABCSUKGRFIPT_Step3.csv"))


## Fixes later applied
# Data_ELPTFI <- here("Data",
#                     "Data_ELPTFI_Cleaned_Step2PPP.csv") %>%
#   fread() %>%
#   data.frame()
#
# Data_ABCSUKGRFIPT <- here("Data",
#                           "Data_ABCSUKGRFIPT_Step3.csv") %>%
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
#               "Data_ABCSUKGRFIPT_Step3.csv"))


# END OF SCRIPT ****************************
