#### Hypertension: Clean First Five Countries ####
# Purpose: Clean survey responses and impute missing income for Austria, Belgium, Canada, Slovenia and the United Kingdom.
# Author: Anonymised for peer review.
# Prerequisites: Run setup scripts 01–04.
# Run from the project root: source("Scripts/Setup/05_Hypertension_Setup_CleanFirstFiveCountries.R", encoding = "UTF-8")
# Inputs:
#   Data/SurveyData/SWACHE_Hypertension_{Country}_Responses.xlsx and *_Timers.csv:
#   Austria, Belgium, Canada, Slovenia and United_Kingdom.
# Outputs:
#   Data/Intermediate/AllCountries_CleanedData.csv and AllCountries_Timers_Cleaned.csv.
#   Output/Figures/Hypertension_Diagnostic_FirstFiveCountriesScope.png.
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

## Data/SurveyData/ holds one Responses and one Timers file per country,
## prepared from the vendor extracts listed in Data/RAW_MANIFEST.md.
## Where pipeline intermediates are written.  Default: Data/Intermediate/ in
## this repo (git-ignored).  Nothing here is committed.
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
## multi-language survey labels (typographic apostrophes U+2019, umlauts,
## Greek, Japanese). dplyr::case_when() with no .default returns NA on
## fall-through, and that is exactly how the Canadian INCCA2 defect (UK GBP
## labels matched against CAD data) survived to publication. After every
## recode, every observed non-missing label must produce a number or be a
## declared don't-know/refuse label; anything else is a hard stop with the
## offending labels printed.

#' Assert that a label -> value recode has no silent fall-through.
assert_no_fallthrough <- function(raw, recoded, varname, na_labels = character()) {
  stopifnot(length(raw) == length(recoded))
  ## An EMPTY cell is a missing value, not a label. Intermediates round-trip
  ## through CSV, and data.table::fread() reads an empty character cell as ""
  ## rather than NA, so a respondent never asked this question arrives as "".
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
## The original code did:
##   preds <- predict(model, newdata = df[df$Country == "X" & df$MISS == 1, ])
##   df$Income_PlusMissing <- ifelse(df$MISS == 1, exp(preds), df$Reported)
## ifelse(test, yes, no) evaluates rep(yes, length.out = length(test))[test].
## The predictions vector (length = number of MISSING respondents) is recycled
## across ALL rows and indexed by GLOBAL ROW POSITION, not by rank among the
## TRUEs, so every imputed income lands on the wrong respondent:
##   ifelse(c(F,T,F,T,F,T), c(100,200,300), rep(0,6))
##   #> [1]   0 200   0 100   0 300      correct: 0 100 0 200 0 300
## Fixed by a keyed join on (Country, Respondent_Serial) -- Respondent_Serial
## alone is NOT unique across waves, so the composite key is mandatory.
## predict.lm() names its output with the row names of `newdata`, which ties
## each prediction back to the respondent it belongs to; rows predict() drops
## (missing predictors) keep their reported value rather than a value stolen
## from someone else.

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

  ## Frame of respondents needing imputation, key retained.
  newdata <- data[need, , drop = FALSE]
  if (!nrow(newdata)) {
    return(as.numeric(reported))
  }

  ## predict.lm() returns a named vector; names are rownames(newdata).
  preds <- stats::predict(model, newdata = newdata)
  hit   <- match(names(preds), rownames(newdata))
  if (anyNA(hit)) {
    stop(sprintf("impute_income_keyed(%s): could not tie predictions back to rows.",
                 label), call. = FALSE)
  }

  ## Two-column frame: (composite key, predicted income).
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

  ## Keyed LEFT JOIN back onto the full frame.
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

  ## Imputed value wins wherever the model produced one; reported otherwise.
  out <- dplyr::coalesce(joined$.Imputed_Income, joined$.Reported)

  message(sprintf(
    "  [impute] %-14s imputed %d of %d flagged rows by keyed join (%d flagged rows had missing predictors and keep their reported value).",
    label, sum(!is.na(joined$.Imputed_Income)), sum(need),
    sum(need) - sum(!is.na(joined$.Imputed_Income))))

  as.numeric(out)
}


# *************************************************************
#### Section 4: Import SAV data ####
# *************************************************************


## Read the shared country response files.

## Wave 1 covers these five countries -- each read from its own
## Data/SurveyData/ file pair and stacked back into the combined shape the
## rest of this script expects. Stacking order is arbitrary (not load-bearing:
## client and timer are stacked in the SAME order for both, so per-row
## alignment holds regardless of what order that is) -- kept as the same
## order this script's own country-dummy block below already uses.
WAVE1_COUNTRIES <- c("Austria", "Belgium", "Canada", "Slovenia", "United Kingdom")


AllCountries_Client_Xlsx <- WAVE1_COUNTRIES %>%
  lapply(function(cc) {
    SURVEY_FILE(cc, "Responses") %>%
      readxl::read_xlsx(col_types = "text") %>%
      data.frame()
  }) %>%
  dplyr::bind_rows()


AllCountries_Timers <- WAVE1_COUNTRIES %>%
  lapply(function(cc) {
    SURVEY_FILE(cc, "Timers") %>%
      fread() %>%
      data.frame()
  }) %>%
  dplyr::bind_rows()


stopifnot("Client/Timers row counts diverged across the five per-country files" =
            nrow(AllCountries_Client_Xlsx) == nrow(AllCountries_Timers))


## Income imputation joins require a respondent identifier.
stopifnot("Respondent_Serial" %in% names(AllCountries_Client_Xlsx))


# *************************************************************
#### Section 5: Add misc columns for later ####
# *************************************************************


# *************************************************************
#### Section 6: COUNTRY SPECIFIC DUMMIES ####
# *************************************************************
AllCountries_Client_Xlsx$Austria <- ifelse(
  AllCountries_Client_Xlsx$Country == "Austria",
  1,
  0)#


AllCountries_Client_Xlsx$Belgium <- ifelse(
  AllCountries_Client_Xlsx$Country == "Belgium",
  1,
  0)


AllCountries_Client_Xlsx$Canada <- ifelse(
  AllCountries_Client_Xlsx$Country == "Canada",
  1,
  0)


AllCountries_Client_Xlsx$Slovenia <- ifelse(
  AllCountries_Client_Xlsx$Country == "Slovenia",
  1,
  0)


AllCountries_Client_Xlsx$UnitedKingdom <- ifelse(
  AllCountries_Client_Xlsx$Country == "United Kingdom",
  1,
  0)


# *************************************************************
#### Section 7: Identify prob test failures ####
# *************************************************************


## Recategorise so that pass  = 1
AllCountries_Client_Xlsx$ProbabilityTest_Numeric <- ifelse(
  AllCountries_Client_Xlsx$T1 ==
    "The risk of getting a cold is higher in group A",
  0 ,
  1)


AllCountries_Client_Xlsx$ProbabilityTest_Categorical <- ifelse(
  AllCountries_Client_Xlsx$T1 ==
    "The risk of getting a cold is higher in group A",
  "Fail",
  "Pass")


# *************************************************************
#### Section 8: Identify speeders ####
# *************************************************************
## First survey speeders then valuation section


## First append the country from the main data to the timer data
AllCountries_Timers$Country <- AllCountries_Client_Xlsx$Country


# *************************************************************
#### Section 9: SURVEY SPEEDERS ####
# *************************************************************
## Calculate each persons time
## by getting each rows sum of time in seconds
AllCountries_Timers$Speeders_Survey_CompletionTime <-
  AllCountries_Timers[2:92] %>%
  rowSums(na.rm = TRUE)


## Now calculate and append to data the
## country specific threshold using 48% of median (for speeding)
## of 148% for too slow
AllCountries_Timers <-
  AllCountries_Timers %>%
  group_by(Country) %>%
  mutate("Speeders_Survey_CountryThreshold" =
           median(Speeders_Survey_CompletionTime) %>% multiply_by(0.48),
         "Slowers_Survey_CountryThreshold" =
           median(Speeders_Survey_CompletionTime) %>% multiply_by(1.48)
         )


## Recode as fail (0) or pass (10)
AllCountries_Timers$Speeders_Survey_TestDummy <- ifelse(
  AllCountries_Timers$Speeders_Survey_CompletionTime <=
    AllCountries_Timers$Speeders_Survey_CountryThreshold, 0, 1)


## Test people going too slow
AllCountries_Timers$Slowers_Survey_TestDummy <- ifelse(
  AllCountries_Timers$Speeders_Survey_CompletionTime >=
    AllCountries_Timers$Slowers_Survey_CountryThreshold, 0, 1)


## Output survey speeder data
AllCountries_Timers %>%
  group_by(Country) %>%
  summarise(
    Threshold = first(Speeders_Survey_CountryThreshold),

    N_Fail = paste0(
      sum(Speeders_Survey_TestDummy == 0),
      " (", mean(Speeders_Survey_TestDummy == 0) %>%
        round(2) %>%
        multiply_by(100), "%)"),

    N_Pass = paste0(
      sum(Speeders_Survey_TestDummy == 1),
      " (", mean(Speeders_Survey_TestDummy == 1) %>%
        round(2) %>%
        multiply_by(100), "%)")) %>%
  write.csv(quote = FALSE)


# *************************************************************
#### Section 10: VALUATION SPEEDERS ####
# *************************************************************
## Valuation section
AllCountries_Timers$Speeders_Valuation_CompletionTime <-
  AllCountries_Timers[c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6a", "Q6b", "Q6c", "Q7")] %>%
  rowSums(na.rm = TRUE)


## Categorise valuation section threshold
AllCountries_Timers <- AllCountries_Timers %>%
  group_by(Country) %>%
  mutate("Speeders_Valuation_CountryThreshold" =
           median(Speeders_Valuation_CompletionTime) %>% multiply_by(0.48),
         "Slowers_Valuation_CountryThreshold" =
           median(Speeders_Valuation_CompletionTime) %>% multiply_by(1.48))


## Recode as fail (0) or pass (10)
AllCountries_Timers$Speeders_Valuation_TestDummy <- ifelse(
  AllCountries_Timers$Speeders_Valuation_CompletionTime <=
    AllCountries_Timers$Speeders_Valuation_CountryThreshold, 0, 1)


AllCountries_Timers$Slowers_Valuation_TestDummy <- ifelse(
  AllCountries_Timers$Speeders_Valuation_CompletionTime >=
    AllCountries_Timers$Slowers_Valuation_CountryThreshold, 0, 1)


AllCountries_Timers %>%
  group_by(Country) %>%
  summarise(
    Threshold = first(Speeders_Valuation_CountryThreshold),

    N_Fail = paste0(
      sum(Speeders_Valuation_TestDummy == 0),
      " (", mean(Speeders_Valuation_TestDummy == 0) %>%
        round(2) %>%
        multiply_by(100), "%)"),

    N_Pass = paste0(
      sum(Speeders_Valuation_TestDummy == 1),
      " (", mean(Speeders_Valuation_TestDummy == 1) %>%
        round(2) %>%
        multiply_by(100), "%)")
  ) %>%
  write.csv(quote = FALSE)


AllCountries_Timers %>%
  fwrite(sep = ",",
         WORK("AllCountries_Timers_Cleaned.csv"))


## Add timer data to main dataset
AllCountries_Client_Xlsx <- cbind(
  AllCountries_Client_Xlsx,
  AllCountries_Timers[, 94:103]
)


# *************************************************************
#### Section 11: DBDC Matrix ####
# *************************************************************


# Recoding -------------------------------------------------------------
AllCountries_Client_Xlsx$Question1 <-
  ifelse(
    AllCountries_Client_Xlsx$Q1 == "Current risk",
    0, ## Vote no
    1) ## Vote yes


AllCountries_Client_Xlsx$Q2_Missing <- ifelse(AllCountries_Client_Xlsx$Q2 %>% is.na(),
                                              NA, ## is.na = true so code as na
                                              AllCountries_Client_Xlsx$Q2) ## put answer if not na


AllCountries_Client_Xlsx$Q3_Missing <- ifelse(AllCountries_Client_Xlsx$Q3 %>% is.na(),
                                              NA,
                                              AllCountries_Client_Xlsx$Q3)


AllCountries_Client_Xlsx$FollowUp <- coalesce(AllCountries_Client_Xlsx$Q2_Missing, AllCountries_Client_Xlsx$Q3_Missing)


AllCountries_Client_Xlsx$FollowUp_Numeric <- ifelse(AllCountries_Client_Xlsx$FollowUp == "Current risk",
                                                    0,
                                                    1)


## Remove currency signs and convert to numeric
AllCountries_Client_Xlsx$Bid1_Numeric <- AllCountries_Client_Xlsx$BID1 %>%
  gsub(pattern = " €", replacement = "") %>%
  gsub(pattern = "£ ", replacement = "") %>%
  gsub(pattern = "$ ", replacement = "", fixed = TRUE) %>%
  gsub(pattern = ",", replacement = "") %>%
  as.numeric()


## DBDC second bound bid level
AllCountries_Client_Xlsx$FollowUp_Bid <-
ifelse(AllCountries_Client_Xlsx$Q2_Missing %>% is.na(),
       AllCountries_Client_Xlsx$BID3,
       AllCountries_Client_Xlsx$BID2) %>%
  gsub(pattern = " €", replacement = "") %>%
  gsub(pattern = "£ ", replacement = "") %>%
  gsub(pattern = "$ ", replacement = "", fixed = TRUE) %>%
  gsub(pattern = ",", replacement = "") %>%
  as.numeric()


## Originally character for reasons
AllCountries_Client_Xlsx$BASE_Numeric <- AllCountries_Client_Xlsx$BASE %>% as.numeric()


AllCountries_Client_Xlsx$DBDC <- ifelse(
  (AllCountries_Client_Xlsx$Question1 == 0) & (AllCountries_Client_Xlsx$FollowUp_Numeric == 0),
  "NN",
  ifelse(
    (AllCountries_Client_Xlsx$Question1 == 0) & (AllCountries_Client_Xlsx$FollowUp_Numeric == 1),
    "NY",
    ifelse(
      (AllCountries_Client_Xlsx$Question1 == 1) & (AllCountries_Client_Xlsx$FollowUp_Numeric == 0),
      "YN",
      ifelse(
        (AllCountries_Client_Xlsx$Question1 == 1) & (AllCountries_Client_Xlsx$FollowUp_Numeric == 1),
        "YY",
        1))))


# Create summaries  -------------------------------------------------------------


## DBDC matrix by country
AllCountries_Client_Xlsx %>% select("Country", "DBDC") %>% table(.)


## Output matrix by country
AllCountries_Client_Xlsx %>% group_by(Country) %>% summarise(
  "NN" = paste0(
    sum(DBDC == "NN"),
    " (", mean(DBDC == "NN") %>%
      round(2) %>%
      multiply_by(100), "%)"),

  "NY" = paste0(
    sum(DBDC == "NY"),
    " (", mean(DBDC == "NY") %>%
      round(2) %>%
      multiply_by(100), "%)"),

  "YN" = paste0(
    sum(DBDC == "YN"),
    " (", mean(DBDC == "YN") %>%
      round(2) %>%
      multiply_by(100), "%)"),

  "YY" = paste0(
    sum(DBDC == "YY"),
    " (", mean(DBDC == "YY") %>%
      round(2) %>%
      multiply_by(100), "%)")
) %>% write.csv(quote = FALSE)


## tabulate responses by bid level
rbind(
  cbind(table(AllCountries_Client_Xlsx$Q1[AllCountries_Client_Xlsx$BASE == 30],
              AllCountries_Client_Xlsx$Bid1_Numeric[AllCountries_Client_Xlsx$BASE == 30]), 0),
  cbind(table(AllCountries_Client_Xlsx$Q1[AllCountries_Client_Xlsx$BASE != 30],
              AllCountries_Client_Xlsx$Bid1_Numeric[AllCountries_Client_Xlsx$BASE != 30]), 0))


## Answer question 3
table(AllCountries_Client_Xlsx$Bid1_Numeric, AllCountries_Client_Xlsx$BASE) %>% t() %>% write.csv(quote = FALSE)


paste0(AllCountries_Client_Xlsx$DBDC %>% table() %>% names(),
       ": ",
       AllCountries_Client_Xlsx$DBDC %>% table(),
       " (",
       AllCountries_Client_Xlsx$DBDC %>% table() %>% prop.table() %>% multiply_by(100) %>% round(2),
       "%)"
)


# Plotting -------------------------------------------------------------

TextSize <- 6


Fig1 <- AllCountries_Client_Xlsx %>%
  ggplot(
    aes(x = Bid1_Numeric, y = Question1)) +
  geom_smooth(method = "glm",method.args = list(family = "binomial")) +
  scale_y_continuous(labels = c("Current\nrisk", "Risk\nreduction"), breaks = c(0, 1), limits = seq(0, 1, 1)) +
  theme_bw() +
  facet_wrap(~ paste0(Country, " - ", BASE),
             nrow = 5,
             ncol = 2) +
  scale_x_continuous(
    labels = seq.int(from = 0, to = 2000, by = 250),
    breaks = seq.int(from = 0, to = 2000, by = 250),
    limits = c(0, 2000)) +
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

    axis.text.x = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"),
    axis.text.y = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"),
    axis.title.y = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    axis.title.x = element_text(size = TextSize,
                                colour = "black",
                                family = "serif"),
    legend.text = element_text(size = TextSize,
                               colour = "black",
                               family = "serif"),
    legend.title = element_text(size = TextSize,
                                colour = "black",
                                family = "serif")

  )


ggsave(
  Fig1,
  device = "png",
  filename = paste0(here(), "/Output/Figures/", "Hypertension_Diagnostic_FirstFiveCountriesScope.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# *************************************************************
#### Section 12: Recoding ####
# *************************************************************


Dependent <- AllCountries_Client_Xlsx$Question1


## Coalesce the two belgium columns
AllCountries_Client_Xlsx$INCBE1_Always <- coalesce(
  AllCountries_Client_Xlsx$INCBE1_D,
  AllCountries_Client_Xlsx$INCBE1_F)


AllCountries_Client_Xlsx$INCBE2_Always <- coalesce(
  AllCountries_Client_Xlsx$INCBE2_D,
  AllCountries_Client_Xlsx$INCBE2_F)


# Income recoding for income question 1 -------------------------------------------------------------


## Recode Austrian missing data
AllCountries_Client_Xlsx$INCAT1_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCAT1 %in%
 c("Ich möchte es lieber nicht sagen",
   "Ich weiß es nicht"
 ),
  1,
  0)


## UK missing income data
AllCountries_Client_Xlsx$INCUK1_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCUK1 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


AllCountries_Client_Xlsx$INCCA1_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCCA1 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


AllCountries_Client_Xlsx$INCSI1_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCSI1 %in%
    c("Ne vem",
      "Raje ne povem"
    ),
  1,
  0)


AllCountries_Client_Xlsx$INCBE1_Always_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCBE1_Always %in%
    c("Ik weet het niet",
      "Ik zeg liever niet",
      "Je ne sais pas.",
      "Je préfère ne pas répondre."
    ),
  1,
  0)


# Income recoding for income question 2 -------------------------------------------------------------


## Recode Austrian missing data
AllCountries_Client_Xlsx$INCAT2_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCAT2 %in%
    c("Ich möchte es lieber nicht sagen",
      "Ich weiß es nicht"
    ),
  1,
  0)


## UK missing income data
AllCountries_Client_Xlsx$INCUK2_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCUK2 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


AllCountries_Client_Xlsx$INCCA2_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCCA2 %in%
    c("Prefer not to answer",
      "I don’t know"
    ),
  1,
  0)


AllCountries_Client_Xlsx$INCSI2_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCSI2 %in%
    c("Ne vem",
      "Raje ne povem"
    ),
  1,
  0)


AllCountries_Client_Xlsx$INCBE2_Always_MissingDummy <- ifelse(
  AllCountries_Client_Xlsx$INCBE2_Always %in%
    c("Ik weet het niet",
      "Ik zeg liever niet",
      "Je ne sais pas.",
      "Je préfère ne pas répondre."
    ),
  1,
  0)


# Income recoding for income question 2 -------------------------------------------------------------

AllCountries_Client_Xlsx_Backup <- AllCountries_Client_Xlsx


## Very large recoding exercise taking the midpoints of each range
AllCountries_Client_Xlsx <- AllCountries_Client_Xlsx %>%
  mutate(
    INCAT1_Numeric = case_when(
      is.na(INCAT1) ~ NA,
      INCAT1 %in% "2 701 € bis 3 000 €" ~ (0.5 * 2701) + (0.5 * 3000),
      INCAT1 %in% "2 001 € bis 2 400 €" ~ (0.5 * 2001) + (0.5 * 2400),
      INCAT1 %in% "3 401 € bis 3 800 €" ~ (0.5 * 3401) + (0.5 * 3800),
      INCAT1 %in% "5 401 € bis 6 700 €" ~ (0.5 * 5401) + (0.5 * 6700),
      INCAT1 %in% "4 401 € bis 5 400 €" ~ (0.5 * 4401) + (0.5 * 5400),
      INCAT1 %in% "1 501 € bis 2 000 €" ~ (0.5 * 1501) + (0.5 * 2000),
      INCAT1 %in% "2 401 € bis 2 700 €" ~ (0.5 * 2401) + (0.5 * 2700),
      INCAT1 %in% "3 801 € bis 4 400 €" ~ (0.5 * 3801) + (0.5 * 4400),
      INCAT1 %in% "3 001 € bis 3 400 €" ~ (0.5 * 3001) + (0.5 * 3400),
      INCAT1 %in% "0 € bis 1 500 €" ~ (0.5 * 0) + (0.5 * 1500),
      INCAT1 %in% "6 701 € oder mehr" ~ 6701,
      INCAT1 %in% "Ich möchte es lieber nicht sagen" ~ NA,
      INCAT1 %in% "Ich weiß es nicht" ~ NA),

    INCUK1_Numeric = case_when(
      is.na(INCUK1) ~ NA,
      INCUK1 %in% "£5,101 or more" ~ 5101 ,
      INCUK1 %in% "£2,101 to £2,400" ~ (0.5 * 2101) + (0.5 * 2400),
      INCUK1 %in% "£3,301 to £4,200" ~ (0.5 * 3301) + (0.5 * 4200),
      INCUK1 %in% "£1,301 to £1,500" ~ (0.5 * 1301) + (0.5 * 1500),
      INCUK1 %in% "£0 to £1,000" ~ (0.5 * 0) + (0.5 * 1000),
      INCUK1 %in% "£2,701 to £3,300" ~ (0.5 * 2701) + (0.5 * 3300),
      INCUK1 %in% "£1,501 to £1,800" ~ (0.5 * 1501) + (0.5 * 1800),
      INCUK1 %in% "£4,201 to £5,100" ~ (0.5 * 4201) + (0.5 * 5100),
      INCUK1 %in% "£2,401 to £2,700" ~ (0.5 * 2401) + (0.5 * 2700),
      INCUK1 %in% "£1,001 to £1,300" ~ (0.5 * 1001) + (0.5 * 1300),
      INCUK1 %in% "£1,801 to £2,100" ~ (0.5 * 1801) + (0.5 * 2100),
      INCUK1 %in% "Prefer not to answer" ~ NA,
      INCUK1 %in% "I don’t know" ~ NA
    ),

    INCCA1_Numeric = case_when(
      is.na(INCCA1) ~ NA,
      INCCA1 %in% "$ 0 to $ 2,800" ~ (0.5 * 0) + (0.5 * 2800),
      INCCA1 %in% "$ 9,601 to $ 11,800" ~ (0.5 * 9601) + (0.5 * 11800),
      INCCA1 %in% "$ 3,501 to $ 4,100" ~ (0.5 * 3501) + (0.5 * 4100),
      INCCA1 %in% "$ 2,801 to $ 3,500" ~ (0.5 * 2801) + (0.5 * 3500),
      INCCA1 %in% "$ 11,801 or more" ~ 11801,
      INCCA1 %in% "$ 7,901 to $ 9,600" ~ (0.5 * 7901) + (0.5 * 9600),
      INCCA1 %in% "$ 4,701 to $ 5,300" ~ (0.5 * 4701) + (0.5 * 5300),
      INCCA1 %in% "$ 5,301 to $ 6,000" ~ (0.5 * 5301) + (0.5 * 6000),
      INCCA1 %in% "$ 4,101 to $ 4,700" ~ (0.5 * 4101) + (0.5 * 4700),
      INCCA1 %in% "$ 6,001 to $ 6,800" ~ (0.5 * 6001) + (0.5 * 6800),
      INCCA1 %in% "$ 6,801 to $ 7,900" ~ (0.5 * 6801) + (0.5 * 7900),
      INCCA1 %in% "I don’t know" ~ NA,
      INCCA1 %in% "Prefer not to answer" ~ NA,
    ),

    INCSI1_Numeric = case_when(
      is.na(INCSI1) ~ NA,
      INCSI1 %in% "1 201 € do 1 400 €" ~ (0.5 * 1201) + (0.5 * 1400),
      INCSI1 %in% "1 501 € do 1 700 €" ~ (0.5 * 1501) + (0.5 * 1700),
      INCSI1 %in% "1 701 € do 1 900 €" ~ (0.5 * 1701) + (0.5 * 1900),
      INCSI1 %in% "2 901 € do 3 500 €" ~ (0.5 * 2901) + (0.5 * 3500),
      INCSI1 %in% "2 101 € do 2 400 €" ~ (0.5 * 2101) + (0.5 * 2400),
      INCSI1 %in% "1 901 € do 2 100 €" ~ (0.5 * 1901) + (0.5 * 2100),
      INCSI1 %in% "2 401 € do 2 900 €" ~ (0.5 * 2401) + (0.5 * 2900),
      INCSI1 %in% "1 401 € do 1 500 €" ~ (0.5 * 1401) + (0.5 * 1500),
      INCSI1 %in% "3 501 € ali več" ~ 3501,
      INCSI1 %in% "901 € do 1 200 €" ~ (0.5 * 901) + (0.5 * 1200),
      INCSI1 %in% "0 € do 900 €"~ (0.5 * 0) + (0.5 * 900),
      INCSI1 %in% "Raje ne povem" ~ NA,
      INCSI1 %in% "Ne vem" ~ NA
    ),

    INCBE1_Always_Numeric = case_when(
      is.na(INCBE1_Always) ~ NA,
      INCBE1_Always %in% "4 101 € à 4 900 €" ~ (0.5 * 4101) + (0.5 * 4900) ,
      INCBE1_Always %in% "4 901 € en 6 000 €" ~ (0.5 * 4901) + (0.5 * 6000) ,
      INCBE1_Always %in% "2 901 € en 3 200 €" ~ (0.5 * 2901) + (0.5 * 3200) ,
      INCBE1_Always %in% "1 501 € à 1 900 €" ~ (0.5 * 1501) + (0.5 * 1900) ,
      INCBE1_Always %in% "2 201 € en 2 600 €" ~ (0.5 * 2201) + (0.5 * 2600) ,
      INCBE1_Always %in% "1 901 € en 2 200 €" ~ (0.5 * 1901) + (0.5 * 2200) ,
      INCBE1_Always %in% "1 501 € en 1 900 €" ~ (0.5 * 1501) + (0.5 * 1900) ,
      INCBE1_Always %in% "1 901 € à 2 200 €" ~ (0.5 * 1901) + (0.5 * 2200) ,
      INCBE1_Always %in% "4 101 € en 4 900 €" ~ (0.5 * 4101) + (0.5 * 4900) ,
      INCBE1_Always %in% "0 € en 1 500 €" ~ (0.5 * 0) + (0.5 * 1500),
      INCBE1_Always %in% "6 001 € ou plus" ~ 6001 ,
      INCBE1_Always %in% "2 601 € en 2 900 €" ~ (0.5 * 2601) + (0.5 * 2900) ,
      INCBE1_Always %in% "4 901 € à 6 000 €" ~ (0.5 * 4901) + (0.5 * 6000) ,
      INCBE1_Always %in% "3 201 € à 3 600 €" ~ (0.5 * 3201) + (0.5 * 3600) ,
      INCBE1_Always %in% "3 201 € en 3 600 €" ~ (0.5 * 3201) + (0.5 * 3600) ,
      INCBE1_Always %in% "3 601 € à 4 100 €" ~ (0.5 * 3601) + (0.5 * 4100) ,
      INCBE1_Always %in% "2 201 € à 2 600 €" ~ (0.5 * 2201) + (0.5 * 2600) ,
      INCBE1_Always %in% "0 € à 1 500 €" ~ (0.5 * 0) + (0.5 * 1500),
      INCBE1_Always %in% "2 601 € à 2 900 €" ~ (0.5 * 2601) + (0.5 * 2900) ,
      INCBE1_Always %in% "3 601 € en 4 100 €" ~ (0.5 * 3601) + (0.5 * 4100) ,
      INCBE1_Always %in% "2 901 € à 3 200 €" ~ (0.5 * 2901) + (0.5 * 3200) ,
      INCBE1_Always %in% "6 001 € of meer" ~ 6001,
      INCBE1_Always %in% "Je préfère ne pas répondre." ~ NA,
      INCBE1_Always %in% "Ik zeg liever niet" ~ NA,
      INCBE1_Always %in% "Ik weet het niet" ~ NA,
      INCBE1_Always %in% "Je ne sais pas." ~ NA
    )
  )


## Now above but with inc2
AllCountries_Client_Xlsx <- AllCountries_Client_Xlsx %>%
  mutate(
    INCAT2_Numeric = case_when(
      is.na(INCAT2) ~ NA,
      INCAT2 %in% "Ich möchte es lieber nicht sagen" ~ NA,
      INCAT2 %in% "4 401 € bis 6 700 €" ~ 5550,
      INCAT2 %in% "0 € bis 2 000 €" ~ 1000,
      INCAT2 %in% "2 701 € bis 3 400 €" ~ 3075,
      INCAT2 %in% "6 701 € oder mehr" ~ 6701,
      INCAT2 %in% "2 001 € bis 2 700 €" ~ 2350,
      INCAT2 %in% "Ich weiß es nicht" ~ NA,
      INCAT2 %in% "3 401 € bis 4 400 €" ~ 3875),

    INCUK2_Numeric = case_when(
      is.na(INCUK2) ~ NA,
      INCUK2 %in% "I don’t know" ~ NA,
      INCUK2 %in% "Prefer not to answer" ~ NA,
      INCUK2 %in% "£5,101 or more" ~ 5101,
      INCUK2 %in% "£0 to £1,300" ~ 650,
      INCUK2 %in% "£1,801 to £2,400" ~ 2100,
      INCUK2 %in% "£3,301 to £5,100" ~ 4200,
      INCUK2 %in% "£1,301 to £1,800" ~ 1550,
      INCUK2 %in% "£2,401 to £3,300" ~ 2775),

    ## Canadian income brackets use CAD labels from the codebook.
    ## Use midpoints for closed bands and the lower bound for the open top band.
    INCCA2_Numeric = case_when(
      is.na(INCCA2) ~ NA,
      INCCA2 %in% "I don’t know" ~ NA,
      INCCA2 %in% "Prefer not to answer" ~ NA,
      INCCA2 %in% "$ 0 to $ 3,500"       ~ (0.5 * 0)    + (0.5 * 3500),   # 1750.0
      INCCA2 %in% "$ 3,501 to $ 4,700"   ~ (0.5 * 3501) + (0.5 * 4700),   # 4100.5
      INCCA2 %in% "$ 4,701 to $ 6,000"   ~ (0.5 * 4701) + (0.5 * 6000),   # 5350.5
      INCCA2 %in% "$ 6,001 to $ 7,900"   ~ (0.5 * 6001) + (0.5 * 7900),   # 6950.5
      INCCA2 %in% "$ 7,901 to $ 11,800"  ~ (0.5 * 7901) + (0.5 * 11800),  # 9850.5
      INCCA2 %in% "$ 11,801 or more"     ~ 11801),                        # lower bound

    INCSI2_Numeric = case_when(
      is.na(INCSI2) ~ NA,
      INCSI2 %in% "Raje ne povem" ~ NA,
      INCSI2 %in% "1 501 € do 1 900 €" ~ 1750, # Midpoint of the interval
      INCSI2 %in% "2 401 € do 3 500 €" ~ 2950, # Midpoint of the interval
      INCSI2 %in% "Ne vem" ~ NA,
      INCSI2 %in% "3 501 € ali več" ~ 3501, # Not specified, return NA
      INCSI2 %in% "0 € do 1 200 €" ~ 600, # Midpoint of the interval
      INCSI2 %in% "1 901 € do 2 400 €" ~ 2200, # Midpoint of the interval
      INCSI2 %in% "1 201 € do 1 500 €" ~ 1350),

    INCBE2_Always_Numeric = case_when(
      is.na(INCBE2_Always) ~ NA,
      INCBE2_Always %in% "Ik zeg liever niet" ~ NA,
      INCBE2_Always %in% "3 201 € en 4 100 €" ~ 3650,
      INCBE2_Always %in% "Je préfère ne pas répondre." ~ NA,
      INCBE2_Always %in% "0 € à 1 900 €" ~ 950,
      INCBE2_Always %in% "Ik weet het niet" ~ NA,
      INCBE2_Always %in% "4 101 € en 6 000 €" ~ 5500,
      INCBE2_Always %in% "3 201 € à 4 100 €" ~ 3650,
      INCBE2_Always %in% "1 901 € en 2 600 €" ~ 2250,
      INCBE2_Always %in% "Je ne sais pas." ~ NA,
      INCBE2_Always %in% "2 601 € à 3 200 €" ~ 2800,
      INCBE2_Always %in% "1 901 € à 2 600 €" ~ 2250,
      INCBE2_Always %in% "2 601 € en 3 200 €" ~ 2800,
      INCBE2_Always %in% "4 101 € à 6 000 €" ~ 5500,
      INCBE2_Always %in% "0 € en 1 900 €" ~ 950,
      INCBE2_Always %in% "6 001 € of meer" ~ 6001,
    )
  )


## TASK E -- assert every observed non-missing income label got a mapping.
## This is the guard that would have caught Bug 2 the day it was written: every
## Canadian INCCA2 respondent carried a "$ ..." label that no arm matched, and
## nothing counted the fall-through.  Any unmatched label is now a hard stop
## with the offending strings printed.
NA_LABELS_AT <- c("Ich möchte es lieber nicht sagen", "Ich weiß es nicht")


NA_LABELS_EN <- c("Prefer not to answer", "I don’t know")


NA_LABELS_SI <- c("Ne vem", "Raje ne povem")


NA_LABELS_BE <- c("Ik weet het niet", "Ik zeg liever niet",
                  "Je ne sais pas.", "Je préfère ne pas répondre.")


assert_recode_family(AllCountries_Client_Xlsx, list(
  INCAT1        = list(out = "INCAT1_Numeric",        na = NA_LABELS_AT),
  INCUK1        = list(out = "INCUK1_Numeric",        na = NA_LABELS_EN),
  INCCA1        = list(out = "INCCA1_Numeric",        na = NA_LABELS_EN),
  INCSI1        = list(out = "INCSI1_Numeric",        na = NA_LABELS_SI),
  INCBE1_Always = list(out = "INCBE1_Always_Numeric", na = NA_LABELS_BE),
  INCAT2        = list(out = "INCAT2_Numeric",        na = NA_LABELS_AT),
  INCUK2        = list(out = "INCUK2_Numeric",        na = NA_LABELS_EN),
  INCCA2        = list(out = "INCCA2_Numeric",        na = NA_LABELS_EN),
  INCSI2        = list(out = "INCSI2_Numeric",        na = NA_LABELS_SI),
  INCBE2_Always = list(out = "INCBE2_Always_Numeric", na = NA_LABELS_BE)
))


message("  [assert] 01_: all ten wave-1 income recodes are complete.")


## Coalesce by country: fast way
AllCountries_Client_Xlsx$INCUK1_All <- coalesce(
  AllCountries_Client_Xlsx$INCUK1_Numeric,
  AllCountries_Client_Xlsx$INCUK2_Numeric
)


AllCountries_Client_Xlsx$INCAT1_All <- coalesce(
  AllCountries_Client_Xlsx$INCAT1_Numeric,
  AllCountries_Client_Xlsx$INCAT2_Numeric
)


AllCountries_Client_Xlsx$INCCA1_All <- coalesce(
  AllCountries_Client_Xlsx$INCCA1_Numeric,
  AllCountries_Client_Xlsx$INCCA2_Numeric
)


AllCountries_Client_Xlsx$INCSI1_All <- coalesce(
  AllCountries_Client_Xlsx$INCSI1_Numeric,
  AllCountries_Client_Xlsx$INCSI2_Numeric
)


AllCountries_Client_Xlsx$INCBE1_Always_All <- coalesce(
  AllCountries_Client_Xlsx$INCBE1_Always_Numeric,
  AllCountries_Client_Xlsx$INCBE2_Always_Numeric
)


## Coalesce by country: fast way
AllCountries_Client_Xlsx$INC1_All <- coalesce(
  AllCountries_Client_Xlsx$INCAT1_All,
  AllCountries_Client_Xlsx$INCUK1_All,
  AllCountries_Client_Xlsx$INCCA1_All,
  AllCountries_Client_Xlsx$INCSI1_All,
  AllCountries_Client_Xlsx$INCBE1_Always_All
)


AllCountries_Client_Xlsx$INC1_Missing <- coalesce(
  AllCountries_Client_Xlsx$INCAT1_MissingDummy,
  AllCountries_Client_Xlsx$INCUK1_MissingDummy,
  AllCountries_Client_Xlsx$INCCA1_MissingDummy,
  AllCountries_Client_Xlsx$INCSI1_MissingDummy,
  AllCountries_Client_Xlsx$INCBE1_Always_MissingDummy
)


## Combine second-stage income responses for the subsequent merge.
AllCountries_Client_Xlsx$INC2_All <- coalesce(
  AllCountries_Client_Xlsx$INCAT2_Numeric,
  AllCountries_Client_Xlsx$INCUK2_Numeric,
  AllCountries_Client_Xlsx$INCCA2_Numeric,
  AllCountries_Client_Xlsx$INCSI2_Numeric,
  AllCountries_Client_Xlsx$INCBE2_Always_Numeric
)


## Gathering correct missing info
AllCountries_Client_Xlsx$INC2_Missing <- coalesce(
  AllCountries_Client_Xlsx$INCAT2_MissingDummy,
  AllCountries_Client_Xlsx$INCUK2_MissingDummy,
  AllCountries_Client_Xlsx$INCCA2_MissingDummy,
  AllCountries_Client_Xlsx$INCSI2_MissingDummy,
  AllCountries_Client_Xlsx$INCBE2_Always_MissingDummy
)


# ## NEW
# AllCountries_Client_Xlsx$INC1_All_Coalesced <- coalesce(
#   AllCountries_Client_Xlsx$INC1_All,
#   AllCountries_Client_Xlsx$INC2_All
# )


# Misc dummies -------------------------------------------------------------


## From character to number
AllCountries_Client_Xlsx$Kids_Numeric <- AllCountries_Client_Xlsx$KIDS02 %>% as.numeric()


## Categorising partner or no
AllCountries_Client_Xlsx$MarriedDummy <- ifelse(
  !AllCountries_Client_Xlsx$S4 %in%
    c("Not in a relationship",
      "Other"),
  1,
  0)


AllCountries_Client_Xlsx$EmployDummy_Employed <-
  ifelse(AllCountries_Client_Xlsx$EMP01 %in% c("Employed full-time",
                                               "Self employed"),
         1,
         0)


AllCountries_Client_Xlsx$EmployDummy_Retired <-
  ifelse(AllCountries_Client_Xlsx$EMP01 %in% c("Retired"),
         1,
         0)


AllCountries_Client_Xlsx$EmployDummy_PT <-
  ifelse(AllCountries_Client_Xlsx$EMP01 %in% c("Employed part-time"),
         1,
         0)


AllCountries_Client_Xlsx$Dummy_Female <-
  ifelse(AllCountries_Client_Xlsx$GENDER_NonBinary %in% c("Female"),
         1,
         0)


AllCountries_Client_Xlsx$HH_PersonsNumeric <-
  ifelse(AllCountries_Client_Xlsx$SE3 %in% c("7 or more"),
         7,
         AllCountries_Client_Xlsx$SE3) %>% as.numeric()


# Age group recoding
AllCountries_Client_Xlsx$Age_1829Dummy <-
  ifelse(AllCountries_Client_Xlsx$AgeGroup %in% c("18-29"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$Age_3044Dummy <-
  ifelse(AllCountries_Client_Xlsx$AgeGroup %in% c("30-44"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$Age_4560Dummy <-
  ifelse(AllCountries_Client_Xlsx$AgeGroup %in% c("45-60"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$Age_60Dummy <-
  ifelse(AllCountries_Client_Xlsx$AgeGroup %in% c("60+"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeGroup_Numeric <- dplyr::recode(AllCountries_Client_Xlsx$AgeGroup,
       '18-29' = 23.50,
       '30-44' = 37,
       '45-60' = 52.5,
       '60+' = 60)


# BY QUOTA -------------------------------------------------------------
AllCountries_Client_Xlsx$AgeQuota_1826 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("18-26"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeQuota_2734 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("27-34"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeQuota_3539 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("35-39"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeQuota_4044 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("40-44"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeQuota_4559 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("45-59"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeQuota_6065 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("60-65"),
         1,
         0) %>% as.numeric()


AllCountries_Client_Xlsx$AgeQuota_65 <-
  ifelse(AllCountries_Client_Xlsx$AgeQuota %in% c("65+"),
         1,
         0) %>% as.numeric()


# Education dummies -------------------------------------------------------------
AllCountries_Client_Xlsx$Dummy_HighEducation_UK <-
  ifelse(
    AllCountries_Client_Xlsx$UK02EDU %in% c("NVQ5 or post-graduate diploma.",
                                                 "NVQ4 / HNC / HND / Bachelor's degree or similar."),
    1,
    0)


AllCountries_Client_Xlsx$Dummy_HighEducation_AUT <-ifelse(
  AllCountries_Client_Xlsx$AT01EDU %in% c(
    "Baccalaureat (Hochschule, Universität, Fachhochschule) (12010)",
    "Magisterium/Diplom (Hochschule, Universität, Fachhochschule) (12011)","Berufsakademie oder Kolleg (mit Diplomabschluss) (12009)",
    "Matura oder Hochschulreife im zweiten Bildungsweg (12007)","Gesundheits- und Krankenpflegeschule, Schulen für medizinisch-technischen Fachdienst (12008)",
    "Promotion od. Habilitation (Hochschule, Universität, Fachhochschule) (12012)"),
  1,
  0)


AllCountries_Client_Xlsx$Dummy_HighEducation_Bel <- ifelse(
  AllCountries_Client_Xlsx$BE02EDU %in%
c("Hoger niet universitair onderwijs van het korte type (2-3 jaar) (1009)",
"Hoger niet universitair onderwijs van het lange  type (4 jaar) (1010)",
"Universitair onderwijs: kandidaat of bachelor (1011)",
"Universitair onderwijs: licentiaat of master (1012)",
"Universitair onderwijs: doctoraat (1013)"),
1,
0)


AllCountries_Client_Xlsx$Dummy_HighEducation_Slo <- ifelse(
  AllCountries_Client_Xlsx$SI02EDU %in%
    c("dokončana višja ali visoka šola (42005)",
      "dokončana fakulteta ali akademija (42006)",
      "specializacija ali magisterij (42007)",
      "doktorat (42008)"),
  1,
  0)


AllCountries_Client_Xlsx$Dummy_HighEducation_CAD <- ifelse(
  AllCountries_Client_Xlsx$CAEDU2 %in%
    c("University undergraduate degree",
      "University graduate degree"),
  1,
  0)


AllCountries_Client_Xlsx$EDU_All <-
ifelse(
  AllCountries_Client_Xlsx$Dummy_HighEducation_AUT == 1 |
  AllCountries_Client_Xlsx$Dummy_HighEducation_Bel == 1 |
  AllCountries_Client_Xlsx$Dummy_HighEducation_Slo == 1 |
  AllCountries_Client_Xlsx$Dummy_HighEducation_CAD == 1 |
  AllCountries_Client_Xlsx$Dummy_HighEducation_UK == 1,
  1, 0
)


# UK
## Predict missing income
Model_PredictMissingIncome_UK <- lm(
  formula = log(INCUK1_All) ~
    AgeGroup +
    Dummy_HighEducation_UK +
    MarriedDummy +
    HH_PersonsNumeric +
    EmployDummy_Retired +
    EmployDummy_PT +
    EmployDummy_Employed,
  data = AllCountries_Client_Xlsx[AllCountries_Client_Xlsx$Country == "United Kingdom",]
)


## Match imputed income by Country and Respondent_Serial, not row position.
AllCountries_Client_Xlsx$INCUK1_Numeric_PlusMissing <- impute_income_keyed(
  data      = AllCountries_Client_Xlsx,
  model     = Model_PredictMissingIncome_UK,
  need_mask = AllCountries_Client_Xlsx$Country == "United Kingdom" &
              AllCountries_Client_Xlsx$INCUK2_MissingDummy == 1,
  reported  = AllCountries_Client_Xlsx$INCUK1_All,
  label     = "UK")


# BELGIUM
Model_PredictMissingIncome_Bel <- lm(formula = log(INCBE1_Always_All) ~
                                      AgeGroup +
                                      Dummy_HighEducation_Bel +
                                      MarriedDummy +
                                      HH_PersonsNumeric +
                                      EmployDummy_Retired +
                                      EmployDummy_PT +
                                      EmployDummy_Employed,
                                    data = AllCountries_Client_Xlsx[AllCountries_Client_Xlsx$Country == "Belgium", ])


## Match imputed income by Country and Respondent_Serial, not row position.
AllCountries_Client_Xlsx$INCBE1_Always_Numeric_PlusMissing <- impute_income_keyed(
  data      = AllCountries_Client_Xlsx,
  model     = Model_PredictMissingIncome_Bel,
  need_mask = AllCountries_Client_Xlsx$Country == "Belgium" &
              AllCountries_Client_Xlsx$INCBE2_Always_MissingDummy == 1,
  reported  = AllCountries_Client_Xlsx$INCBE1_Always_All,
  label     = "Belgium")


# AUSTRIA
Model_PredictMissingIncome_Aut <- lm(formula = log(INCAT1_All) ~
                                      AgeGroup +
                                      Dummy_HighEducation_AUT +
                                      MarriedDummy +
                                      HH_PersonsNumeric +
                                      EmployDummy_Retired +
                                      EmployDummy_PT +
                                      EmployDummy_Employed,
                                    data = AllCountries_Client_Xlsx[AllCountries_Client_Xlsx$Country == "Austria", ])


## Match imputed income by Country and Respondent_Serial, not row position.
AllCountries_Client_Xlsx$INCAT1_Numeric_PlusMissing <- impute_income_keyed(
  data      = AllCountries_Client_Xlsx,
  model     = Model_PredictMissingIncome_Aut,
  need_mask = AllCountries_Client_Xlsx$Country == "Austria" &
              AllCountries_Client_Xlsx$INCAT2_MissingDummy == 1,
  reported  = AllCountries_Client_Xlsx$INCAT1_All,
  label     = "Austria")


# SLOVENIA
Model_PredictMissingIncome_Slo <- lm(formula = log(INCSI1_All) ~
                                      AgeGroup +
                                      Dummy_HighEducation_Slo +
                                      MarriedDummy +
                                      HH_PersonsNumeric +
                                      EmployDummy_Retired +
                                      EmployDummy_PT +
                                      EmployDummy_Employed,
                                    data = AllCountries_Client_Xlsx[AllCountries_Client_Xlsx$Country == "Slovenia", ])


## Match imputed income by Country and Respondent_Serial, not row position.
AllCountries_Client_Xlsx$INCSI1_Numeric_PlusMissing <- impute_income_keyed(
  data      = AllCountries_Client_Xlsx,
  model     = Model_PredictMissingIncome_Slo,
  need_mask = AllCountries_Client_Xlsx$Country == "Slovenia" &
              AllCountries_Client_Xlsx$INCSI2_MissingDummy == 1,
  reported  = AllCountries_Client_Xlsx$INCSI1_All,
  label     = "Slovenia")


# CANADA
Model_PredictMissingIncome_Cad <- lm(formula = log(INCCA1_All) ~
                                      AgeGroup +
                                      Dummy_HighEducation_CAD +
                                      MarriedDummy +
                                      HH_PersonsNumeric +
                                      EmployDummy_Retired +
                                      EmployDummy_PT +
                                      EmployDummy_Employed,
                                    data = AllCountries_Client_Xlsx[AllCountries_Client_Xlsx$Country == "Canada", ])


## Match imputed income by Country and Respondent_Serial, not row position.
AllCountries_Client_Xlsx$INCCA1_Numeric_PlusMissing <- impute_income_keyed(
  data      = AllCountries_Client_Xlsx,
  model     = Model_PredictMissingIncome_Cad,
  need_mask = AllCountries_Client_Xlsx$Country == "Canada" &
              AllCountries_Client_Xlsx$INCCA2_MissingDummy == 1,
  reported  = AllCountries_Client_Xlsx$INCCA1_All,
  label     = "Canada")


## Derive recoded values from a snapshot so rerunning this block does not
## increment them again. UK missing-income codes are 1/2 here; the merge
## interprets 2 as missing.
if (!exists(".ORIG_01_SHIFTCOLS")) {
  .ORIG_01_SHIFTCOLS <- list(
    Kids_Numeric           = AllCountries_Client_Xlsx$Kids_Numeric,
    INCUK1_MissingDummy    = AllCountries_Client_Xlsx$INCUK1_MissingDummy,
    Dummy_HighEducation_UK = AllCountries_Client_Xlsx$Dummy_HighEducation_UK,
    Age_1829Dummy          = AllCountries_Client_Xlsx$Age_1829Dummy,
    Age_3044Dummy          = AllCountries_Client_Xlsx$Age_3044Dummy,
    Age_4560Dummy          = AllCountries_Client_Xlsx$Age_4560Dummy,
    Age_60Dummy            = AllCountries_Client_Xlsx$Age_60Dummy
  )
}


AllCountries_Client_Xlsx$Kids_Numeric           <- .ORIG_01_SHIFTCOLS$Kids_Numeric ## DO NOT CHANGE THIS FU OPUS5


# AllCountries_Client_Xlsx$Kids_Numeric           <- .ORIG_01_SHIFTCOLS$Kids_Numeric + 1
AllCountries_Client_Xlsx$INCUK1_MissingDummy    <- .ORIG_01_SHIFTCOLS$INCUK1_MissingDummy + 1


AllCountries_Client_Xlsx$Dummy_HighEducation_UK <- .ORIG_01_SHIFTCOLS$Dummy_HighEducation_UK + 1


## Log of UK income after imputation, retained for the subsequent merge.
AllCountries_Client_Xlsx$INCUK1_Numeric_PlusMissing_Log <- AllCountries_Client_Xlsx$INCUK1_Numeric_PlusMissing %>% log()


AllCountries_Client_Xlsx$RED_Numeric            <- AllCountries_Client_Xlsx$RED %>% as.numeric()


AllCountries_Client_Xlsx$Age_1829Dummy_Adjusted <- .ORIG_01_SHIFTCOLS$Age_1829Dummy + 1


AllCountries_Client_Xlsx$Age_3044Dummy_Adjusted <- .ORIG_01_SHIFTCOLS$Age_3044Dummy + 1


AllCountries_Client_Xlsx$Age_4560Dummy_Adjusted <- .ORIG_01_SHIFTCOLS$Age_4560Dummy + 1


AllCountries_Client_Xlsx$Age_60Dummy_Adjusted   <- .ORIG_01_SHIFTCOLS$Age_60Dummy + 1


## Coalesce income type by country: slow way
# ifelse(AllCountries_Client_Xlsx$Country == "Austria",
#        AllCountries_Client_Xlsx$INCAT1,
#        ifelse(AllCountries_Client_Xlsx$Country == "Canada",
#               AllCountries_Client_Xlsx$INCCA1,
#               ifelse(AllCountries_Client_Xlsx$Country == "Slovenia",
#                      AllCountries_Client_Xlsx$INCSI1,
#                      ifelse(AllCountries_Client_Xlsx$Country == "United Kingdom",
#                             AllCountries_Client_Xlsx$INCUK1,
#                      AllCountries_Client_Xlsx$INCBE1_Always))))


## Recoding second question for ease of reference
AllCountries_Client_Xlsx$Question2 <- AllCountries_Client_Xlsx$FollowUp_Numeric


## Dummy categorise education levels
AllCountries_Client_Xlsx$EducationCategory <- AllCountries_Client_Xlsx$ISCED


AllCountries_Client_Xlsx$EducationCategory_Dummy_High <- ifelse(
  AllCountries_Client_Xlsx$ISCED == "High", 1, 0)


AllCountries_Client_Xlsx$EducationCategory_Dummy_Medium <- ifelse(
  AllCountries_Client_Xlsx$ISCED == "Medium", 1, 0)


AllCountries_Client_Xlsx$EducationCategory_Dummy_Low <- ifelse(
  AllCountries_Client_Xlsx$ISCED == "Low", 1, 0)


# *************************************************************
#### Section 13: Recoding health questions ####
# *************************************************************

# Q02 and DX
## Yes if heard of high blood pressure, zero if not
AllCountries_Client_Xlsx$Health_HeardOfHBP <- ifelse(
  AllCountries_Client_Xlsx$Q02 == "Yes", 1, 0)


## Yes if they had high blood pressure
AllCountries_Client_Xlsx$Health_HadHBP <- ifelse(
  AllCountries_Client_Xlsx$Q03 == "Yes", 1, 0)


# Before answering this survey, did you know about the health risks associated with untreated high blood pressure?
AllCountries_Client_Xlsx$Health_HBP_D1 <- ifelse(
  AllCountries_Client_Xlsx$D1 == "No, I did not know", 0, 1)


# Before answering this survey, did you know that high blood pressure could be treated by having a healthier lifestyle or medical treatments?
AllCountries_Client_Xlsx$Health_HBP_D2 <- ifelse(
  AllCountries_Client_Xlsx$D2 == "No, I did not know", 1, 0)


## Diagnosed and stuck to it
AllCountries_Client_Xlsx$Health_HBP_DiagnosedStuck <- ifelse(
  AllCountries_Client_Xlsx$D2 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Health_HBP_SideEffects <- ifelse(
  AllCountries_Client_Xlsx$D2 == "Yes", 1, 0)


# Dummy coding knowledge of risks
## Can do faster with %>% mutate() but more legible here
## Recoding Q04 here
AllCountries_Client_Xlsx$Risks_Genetic <- ifelse(
  AllCountries_Client_Xlsx$Q04_1 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Obesity <- ifelse(
  AllCountries_Client_Xlsx$Q04_2 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Alcohol <- ifelse(
  AllCountries_Client_Xlsx$Q04_3 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Exposure <- ifelse(
  AllCountries_Client_Xlsx$Q04_4 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Insulin <- ifelse(
  AllCountries_Client_Xlsx$Q04_5 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Aging <- ifelse(
  AllCountries_Client_Xlsx$Q04_6 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Exercise <- ifelse(
  AllCountries_Client_Xlsx$Q04_7 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Smoking <- ifelse(
  AllCountries_Client_Xlsx$Q04_8 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Stress <- ifelse(
  AllCountries_Client_Xlsx$Q04_9 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Risks_Deprive <- ifelse(
  AllCountries_Client_Xlsx$Q04_10 == "Yes", 1, 0)


# Recoding H series Qs


# How do you rate your risk of developing high blood pressure compared to people of your age and gender?
AllCountries_Client_Xlsx$HBP_RelativeRiskHigh <- ifelse(
  AllCountries_Client_Xlsx$H2 %in%
    c("My risk is clearly larger",
      "My risk is somewhat larger"),
  1, 0)


# Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?
AllCountries_Client_Xlsx$HBP_10Yr <- ifelse(
  AllCountries_Client_Xlsx$H3 == "Yes",
  1, 0)


# When you choose between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle to reduce your risk instead of paying?
AllCountries_Client_Xlsx$HBP_Healthier <- ifelse(
  AllCountries_Client_Xlsx$H4 == "Yes",
  1, 0)


# Please consider the statement: “I would pay almost anything to improve my health even by a small amount”. Do you…
AllCountries_Client_Xlsx$HBP_PayAnything <- ifelse(
  AllCountries_Client_Xlsx$H5 == "…agree",
  1, 0)


# Is high blood pressure a genetic condition in your family?
AllCountries_Client_Xlsx$HBP_Genetic <- ifelse(
  AllCountries_Client_Xlsx$H6 == "Yes",
  1, 0)


# Do you know of people close to you (family, friends) who have, or have had, high blood pressure?
AllCountries_Client_Xlsx$HBP_Family <- ifelse(
  AllCountries_Client_Xlsx$H7 == "Yes",
  1, 0)


# I have been diagnosed with: - [disease name]
AllCountries_Client_Xlsx$Diagnosed_KidneyDisease <- ifelse(AllCountries_Client_Xlsx$H8_1 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_HeartDisease <- ifelse(AllCountries_Client_Xlsx$H8_2 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_Obesity <- ifelse(AllCountries_Client_Xlsx$H8_3 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_Diabetes <- ifelse(AllCountries_Client_Xlsx$H8_4 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_ParkinsonDisease <- ifelse(AllCountries_Client_Xlsx$H8_5 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_AlzheimersDementias <- ifelse(AllCountries_Client_Xlsx$H8_6 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_RheumatoidArthritis <- ifelse(AllCountries_Client_Xlsx$H8_7 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_Respiratory <- ifelse(AllCountries_Client_Xlsx$H8_8 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_Other <- ifelse(AllCountries_Client_Xlsx$H8_9 == "Yes", 1, 0)


AllCountries_Client_Xlsx$Diagnosed_None <- ifelse(AllCountries_Client_Xlsx$H8_10 == "Yes", 1, 0)


# How would you describe your health in general?
AllCountries_Client_Xlsx$Health_Own <- dplyr::recode(
  AllCountries_Client_Xlsx$H9,
  "Excellent" = 5,
  "Good" = 4,
  "Fair" = 3,
  "Poor" = 2,
  "Very poor" = 1,
  "Prefer not to answer" = 0
  )


# How would you describe your health relative to others of your age and gender in your country?
AllCountries_Client_Xlsx$Health_Relative <- dplyr::recode(
  AllCountries_Client_Xlsx$H10,
  "Well above average" = 5,
  "Above average" = 4,
  "Average" = 3,
  "Below average" = 2,
  "Well below average" = 1,
  "Prefer not to answer" = 0
)


## Classify
AllCountries_Client_Xlsx$Health_Relative_BelowAverage <- ifelse(AllCountries_Client_Xlsx$Health_Relative == 2, 1, 0)


AllCountries_Client_Xlsx$Health_Relative_AboveAverage <- ifelse(AllCountries_Client_Xlsx$Health_Relative == 4, 1, 0)


# How would you describe your weight relative to others of your height, age and gender in your country?
AllCountries_Client_Xlsx$Health_Weight <- dplyr::recode(
  AllCountries_Client_Xlsx$H11,
  "Less than average" = 3,
  "Average" = 2,
  "More than average" = 1,
  "Prefer not to answer" = 0
)


# Do you smoke?
AllCountries_Client_Xlsx$Health_Smoke <- dplyr::recode(
  AllCountries_Client_Xlsx$H12,
  "No" = 3,
  "Yes, occasionally" = 2,
  "Yes, regularly" = 1,
  "Prefer not to answer" = 0
)


# How many alcoholic drinks do you usually have in a normal week?
AllCountries_Client_Xlsx$Health_Drink  <- dplyr::recode(
  AllCountries_Client_Xlsx$H13,
  "I usually don’t drink alcohol" = 1,
  "1-4 drinks per week" = 2,
  "5-9 drinks per week" = 3,
  "More than 9 drinks per week" = 4,
  "999999" = 0
)


# How often do you exercise in a normal week? Please pick the answer that best describes a normal week.
AllCountries_Client_Xlsx$Health_Exercise  <- dplyr::recode(
  AllCountries_Client_Xlsx$H14,
  "More than 5 times per week" = 5,
  "3-5 times per week"       = 4,
  "1-2 times per week" = 3,
  "Less than 1 time per week" = 2,
  "I don’t exercise" = 1,
  "Prefer not to answer"     = 0)


# Do you use sunscreen?
AllCountries_Client_Xlsx$Health_Sunscreen <- dplyr::recode(
  AllCountries_Client_Xlsx$H15,
  "Yes, regularly" = 3,
  "Yes, occasionally" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
  )


# Do you consider yourself to have a healthy lifestyle (exercise regularly, eat healthily, sleep more than 7 hours a night...)?
AllCountries_Client_Xlsx$Health_Lifestyle <- dplyr::recode(
  AllCountries_Client_Xlsx$H16,
  "Yes" = 4,
  "In general yes" = 3,
  "In general no" = 2,
  "No" = 1,
  "Prefer not to answer" = 0
)


# Take a moment to think about the products you typically purchase. How do you feel about them?
AllCountries_Client_Xlsx$Health_PurchaseProducts <- dplyr::recode(
  AllCountries_Client_Xlsx$H17,
  "Totally safe for people’s health" = 4 ,
  "Rather safe for people’s health" = 3 ,
  "I don’t know" = 2,
  "Rather unsafe for people’s health" = 1,
  "Totally unsafe for people’s health" = 0
)


# Which of the following statements best describes your purchasing behaviour?
AllCountries_Client_Xlsx$Health_PurchaseBehaviour <- dplyr::recode(
  AllCountries_Client_Xlsx$H18,
  "I try to buy safer products even if it comes at an extra cost (e.g. by choosing safer products even if they are a bit mo" = 3,
  "I try to buy safer products if it doesn’t come at an extra cost (e.g. by choosing safer products if they are similar i" = 2,
  "I don’t know" = 1,
  "I do not pay much attention to the products’ effect on my health" = 0
)


# How much do you agree or disagree with the following statements? - My survey responses reflect what I would have done in real life.
AllCountries_Client_Xlsx$Response_RealLife <- dplyr::recode(
  AllCountries_Client_Xlsx$A1_1,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - The survey provided me with enough information to make informed choices.
AllCountries_Client_Xlsx$Response_Information <- dplyr::recode(
  AllCountries_Client_Xlsx$A1_2,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - I find the description of high blood pressure provided in this survey to be accurate
AllCountries_Client_Xlsx$Response_Accurate <- dplyr::recode(
  AllCountries_Client_Xlsx$A1_3,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How confident are you that the information that has been provided in this survey is correct?
AllCountries_Client_Xlsx$Response_Confident <- dplyr::recode(
  AllCountries_Client_Xlsx$A2,
  "Very confident" = 4,
  "Somewhat confident"                 = 3,
  "Neither doubtful nor confident"  = 2,
  "Somewhat doubtful" = 1,
  "Very doubtful"   = 0
)


# How would you describe your knowledge of high blood pressure before having taken this survey?
AllCountries_Client_Xlsx$Response_HBP_Before <- dplyr::recode(
  AllCountries_Client_Xlsx$A3,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
AllCountries_Client_Xlsx$Response_HBP_After <- dplyr::recode(
  AllCountries_Client_Xlsx$A4,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
AllCountries_Client_Xlsx$Response_HBP_Change <-
  (AllCountries_Client_Xlsx$Response_HBP_After -
     AllCountries_Client_Xlsx$Response_HBP_Before)


# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
AllCountries_Client_Xlsx$Health_Cost <- ifelse(
  AllCountries_Client_Xlsx$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket",
      "A public and a private health insurance would together cover all or most of the costs" ),
  1, 0)


## Own pocked specifically
AllCountries_Client_Xlsx$Health_Cost_OwnPocket <- ifelse(
  AllCountries_Client_Xlsx$SE1 %in%
    c("I would have to cover most of the costs out of my own pocket"),
  1, 0)


## SE2 categorises nicely but let's add a dummy
AllCountries_Client_Xlsx$Urban <- ifelse(
  AllCountries_Client_Xlsx$SE2 == "Urban",
  1, 0)


# How certain are you of the maximum value you just indicated?
AllCountries_Client_Xlsx$Certainty <- dplyr::recode(
  AllCountries_Client_Xlsx$Q7,
  "Strongly certain" = 4,
  "Somewhat certain" = 3,
  "Neither certain nor uncertain" = 2,
  "Somewhat uncertain" = 1,
  "Strongly uncertain" = 0
)


## Define here for later
AllCountries_Client_Xlsx$Spike <- ifelse(
  AllCountries_Client_Xlsx$Q4 %in% c("No, I would not pay anything"),
  0, 1
)


# *************************************************************
#### Section 14: Weights ####
# *************************************************************


## Simple interactions with existing weights
AllCountries_Client_Xlsx$Weights_Austria <- ((AllCountries_Client_Xlsx$weight %>% as.numeric()) * (AllCountries_Client_Xlsx$Austria %>% as.numeric()))


AllCountries_Client_Xlsx$Weights_Belgium <- ((AllCountries_Client_Xlsx$weight %>% as.numeric()) * AllCountries_Client_Xlsx$Belgium %>% as.numeric())


AllCountries_Client_Xlsx$Weights_Canada <- ((AllCountries_Client_Xlsx$weight %>% as.numeric()) * AllCountries_Client_Xlsx$Canada %>% as.numeric())


AllCountries_Client_Xlsx$Weights_Slovenia <- ((AllCountries_Client_Xlsx$weight %>% as.numeric()) * AllCountries_Client_Xlsx$Slovenia %>% as.numeric())


AllCountries_Client_Xlsx$Weights_UnitedKingdom <- ((AllCountries_Client_Xlsx$weight %>% as.numeric()) * AllCountries_Client_Xlsx$UnitedKingdom %>% as.numeric())


# *************************************************************
#### Section 15: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
AllCountries_Client_Xlsx %>%
  fwrite(sep = ",",
         WORK("AllCountries_CleanedData.csv"))


# *************************************************************
#### Section 16: Next step: purchasing-power conversion ####
# *************************************************************
## The master script next runs 06_Hypertension_Setup_PurchasingPowerFirstFiveCountries.R.

# *************************************************************
#### Section 17: Plot knowledge ####
# *************************************************************
#
# Plot_Left <- AllCountries_Client_Xlsx %>%
#   group_by(Country,
#            Response_HBP_Before ) %>%
#   summarise(n = n()) %>%
#   mutate("Variable" = Response_HBP_Before) %>%
#   select(-c(Response_HBP_Before)) %>%
#   pivot_wider(names_from = Country,
#               values_from = n) %>%
#   pivot_longer(cols = 2:(AllCountries_Client_Xlsx$Response_HBP_Before %>%
#                            unique() %>%
#                            length() %>%
#                            add(1)),
#                names_to = "Country") %>%
#   mutate(Variable = factor(Variable, levels = unique(Variable)),
#          "Measure" = "Before")
#
#
# Plot_Right <- AllCountries_Client_Xlsx %>%
#   group_by(Country,
#            Response_HBP_After ) %>%
#   summarise(n = n()) %>%
#   mutate("Variable" = Response_HBP_After) %>%
#   select(-c(Response_HBP_After)) %>%
#   pivot_wider(names_from = Country,
#               values_from = n) %>%
#   pivot_longer(cols = 2:(AllCountries_Client_Xlsx$Response_HBP_After %>%
#                            unique() %>%
#                            length() %>%
#                            add(1)),
#                names_to = "Country") %>%
#   mutate(Variable = factor(Variable, levels = unique(Variable)),
#          "Measure" = "After")
#
#
# PlotData <- rbind(Plot_Left, Plot_Right)
#
#
# PlotData %>% ggplot(aes(y = value %>% as.numeric(),
#                         x = Country %>% as.factor(), group = Variable %>% as.factor(), fill = Variable %>% as.factor())) +
#   geom_bar(stat = "identity", position = "dodge", colour = "black", alpha = 0.5) +
#   theme_bw() +
#   facet_wrap( ~ Measure) +
#   scale_x_discrete(name = "Country") +
#   scale_y_continuous(name = "N") +
#   scale_fill_brewer(name = "How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?" %>% stringr::str_wrap(., width = 15)) +
#   guides(fill = guide_legend(nrow = 3)) +
#   theme(legend.position = "right",
#         legend.background=element_blank(),
#         legend.box.background = element_rect(colour="black"),
#         axis.text.x = element_text(size = 10,
#                                    colour = "black",
#                                    family = "sans"),
#         axis.text.y = element_text(size = 10,
#                                    colour = "black",
#                                    family = "sans"),
#         text = element_text(size = 10,
#                             colour = "black",
#                             family = "sans"),
#         legend.text = element_text(size = 10,
#                                    colour = "black",
#                                    family = "sans"))


# ggplot(PlotData,
#        aes(
#          x = value,
#          y = rev(Country),
#          fill = rev(Country),
#          group = rev(Country)
#        )) +
#   ggdist::stat_halfeye(normalize = "groups",
#                        slab_linewidth = 0.5,
#                        slab_colour = "black",
#                        outline_bars = TRUE,
#                        position = "dodge") +
#   theme_bw() +
#   facet_wrap( ~ Measure)


# # END OF SCRIPT  **************************************************************
