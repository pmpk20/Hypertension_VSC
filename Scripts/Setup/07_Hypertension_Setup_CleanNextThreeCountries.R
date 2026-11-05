#### Hypertension: Clean Next Three Countries ####
# Purpose: Clean survey responses and response timings for Greece, Portugal and Finland.
# Author: Anonymised for peer review.
# Prerequisites: Run setup scripts 01–04.
# Run from the project root: source("Scripts/Setup/07_Hypertension_Setup_CleanNextThreeCountries.R", encoding = "UTF-8")
# Inputs:
#   Data/SurveyData/SWACHE_Hypertension_{Country}_Responses.xlsx and *_Timers.csv:
#   Greece, Portugal and Finland.
# Outputs:
#   Data/Intermediate/AllCountries_Main_EL_PT_FI_Data_Cleaned.csv and AllCountries_Main_EL_PT_FI_Timers_Cleaned.csv.
#   Output/Figures/Hypertension_Diagnostic_NextThreeCountriesScope.png.
#   Output/Figures/Hypertension_Diagnostic_ResponseCheckA.png and Hypertension_Diagnostic_ResponseCheckB.png (also written by script 09).
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


library(scales)  # Percentage labels for response summaries.


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
#### Section 2: Import SAV data ####
# *************************************************************

## Wave 2 covers these three countries -- each read from its own
## Data/SurveyData/ file pair and stacked back into the combined shape the
## rest of this script expects (see 05_Hypertension_Setup_CleanFirstFiveCountries.R for the
## fuller version of this comment; identical reasoning applies here).
WAVE2_COUNTRIES <- c("Greece", "Portugal", "Finland")


AllCountries_Client_Xlsx <- WAVE2_COUNTRIES %>%
  lapply(function(cc) {
    SURVEY_FILE(cc, "Responses") %>%
      readxl::read_xlsx(col_types = "text") %>%
      data.frame()
  }) %>%
  dplyr::bind_rows()


AllCountries_Timers <- WAVE2_COUNTRIES %>%
  lapply(function(cc) {
    SURVEY_FILE(cc, "Timers") %>%
      fread() %>%
      data.frame()
  }) %>%
  dplyr::bind_rows()


stopifnot("Client/Timers row counts diverged across the three per-country files" =
            nrow(AllCountries_Client_Xlsx) == nrow(AllCountries_Timers))


# *************************************************************
#### Section 3: COUNTRY SPECIFIC DUMMIES ####
# *************************************************************
AllCountries_Client_Xlsx$Finland <- ifelse(
  AllCountries_Client_Xlsx$Country == "Finland",
  1,
  0)#


AllCountries_Client_Xlsx$Greece <- ifelse(
  AllCountries_Client_Xlsx$Country == "Greece",
  1,
  0)


AllCountries_Client_Xlsx$Portugal <- ifelse(
  AllCountries_Client_Xlsx$Country == "Portugal",
  1,
  0)


# *************************************************************
#### Section 4: Identify prob test failures ####
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
#### Section 5: Identify speeders ####
# *************************************************************
## First survey speeders then valuation section


## First append the country from the main data to the timer data
AllCountries_Timers$Country <- AllCountries_Client_Xlsx$Country


# *************************************************************
#### Section 6: SURVEY SPEEDERS ####
# *************************************************************
## Calculate each persons time
## by getting each rows sum of time in seconds
AllCountries_Timers$Speeders_Survey_CompletionTime <-
  AllCountries_Timers[2:88] %>%
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
#### Section 7: VALUATION SPEEDERS ####
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
         WORK("AllCountries_Main_EL_PT_FI_Timers_Cleaned.csv"))


#
# ## Add timer data to main dataset
# AllCountries_Client_Xlsx <- cbind(
#   AllCountries_Client_Xlsx,
#   AllCountries_Timers[, 94:103]
# )


AllCountries_Timers$ProbabilityTest_Numeric <- AllCountries_Client_Xlsx$ProbabilityTest_Numeric


AllCountries_Timers$DBDC <- AllCountries_Client_Xlsx$DBDC


summary_function <- function(data, column) {
  data %>%
    group_by(Country) %>%
    summarise(across({{column}},
                     list(Min = min,
                          `1st Qu.` = ~quantile(.x, 0.25, na.rm = TRUE),
                          Median = ~round(median(.x, na.rm = TRUE), 3),
                          Mean = ~round(mean(.x, na.rm = TRUE), 3),
                          `3rd Qu.` = ~quantile(.x, 0.75, na.rm = TRUE),
                          Max = max,
                          SD = ~round(sd(.x, na.rm = TRUE), 3)), # Correct rounding within the list
                     .names = "{.fn}"),
              .groups = "drop")
}


rbind(
  "Survey time",
  summary_function(AllCountries_Timers, Speeders_Survey_CompletionTime),
  "Valuation time",
  summary_function(AllCountries_Timers, Speeders_Valuation_CompletionTime)
) %>% write.csv(quote = FALSE)


## Subset by prob test
summary_function <- function(data, column) {
  data %>%
    # filter(ProbabilityTest_Numeric == 1) %>%
    group_by(Country) %>%
    summarise(across({{column}},
                     list(Min = min,
                          `1st Qu.` = ~quantile(.x, 0.25, na.rm = TRUE),
                          Median = ~round(median(.x, na.rm = TRUE), 3),
                          Mean = ~round(mean(.x, na.rm = TRUE), 3),
                          `3rd Qu.` = ~quantile(.x, 0.75, na.rm = TRUE),
                          Max = max,
                          SD = ~round(sd(.x, na.rm = TRUE), 3)), # Correct rounding within the list
                     .names = "{.fn}"),
              .groups = "drop")
}


rbind(
  "ProbTest0",
  summary_function(AllCountries_Timers[AllCountries_Timers$ProbabilityTest_Numeric == 0,], Speeders_Survey_CompletionTime),
  summary_function(AllCountries_Timers[AllCountries_Timers$ProbabilityTest_Numeric == 0,], Speeders_Valuation_CompletionTime),
  "ProbTest1",
  summary_function(AllCountries_Timers[AllCountries_Timers$ProbabilityTest_Numeric == 1,], Speeders_Survey_CompletionTime),
  summary_function(AllCountries_Timers[AllCountries_Timers$ProbabilityTest_Numeric == 1,], Speeders_Valuation_CompletionTime)
) %>% write.csv(quote = FALSE)


Test <- c("YY",
          "NY",
          "NN",
          "YN")


#
# rbind(
#   "YY",
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "YY",], Speeders_Survey_CompletionTime),
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "YY",], Speeders_Valuation_CompletionTime),
#   "YN",
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "YN",], Speeders_Survey_CompletionTime),
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "YN",], Speeders_Valuation_CompletionTime),
#   "NY",
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "NY",], Speeders_Survey_CompletionTime),
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "NY",], Speeders_Valuation_CompletionTime),
#   "NN",
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "NN",], Speeders_Survey_CompletionTime),
#   summary_function(AllCountries_Timers[AllCountries_Timers$DBDC == "NN",], Speeders_Valuation_CompletionTime)
# ) %>% write.csv(quote = FALSE)
#


# *************************************************************
#### Section 8: Count and proportion by two groups ####
# *************************************************************


AllCountries_Client_Xlsx %>%
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
#### Section 9: DBDC Matrix ####
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
AllCountries_Client_Xlsx$Bid1_Numeric <-
  AllCountries_Client_Xlsx$BID1 %>%
  gsub(pattern = "€", replacement = "") %>% as.numeric()


## DBDC second bound bid level
AllCountries_Client_Xlsx$FollowUp_Bid <-
  ifelse(AllCountries_Client_Xlsx$Q2_Missing %>% is.na(),
         AllCountries_Client_Xlsx$BID3,
         AllCountries_Client_Xlsx$BID2) %>%
  gsub(pattern = "€", replacement = "") %>%
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
AllCountries_Client_Xlsx %>%
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


## Export after the health-perception recodes below; the merge uses
## Response_HBP_Before and Response_HBP_After to calculate the change.


# *************************************************************
#### Section 10: Debriefing questions ####
# *************************************************************


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


# *************************************************************
#### Section 11: Export (moved here from Section 4 -- see the note above) ####
# *************************************************************

AllCountries_Client_Xlsx %>%
  fwrite(sep = ",",
         WORK("AllCountries_Main_EL_PT_FI_Data_Cleaned.csv"))


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
#### Section 12: Plot general ####
# *************************************************************


TextSize <- 12


TextType <- element_text(size = TextSize,
             colour = "black",
             family = "serif")


Fig1 <- AllCountries_Client_Xlsx %>%
  ggplot(
    aes(x = Bid1_Numeric, y = Question1)) +
  geom_smooth(method = "glm",method.args = list(family = "binomial")) +
  scale_y_continuous(labels = c("Current\nrisk", "Risk\nreduction"), breaks = c(0, 1), limits = seq(0, 1, 1)) +
  theme_bw() +
  facet_wrap(~ paste0(Country, " - ", BASE),
             nrow = 5,
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
  filename = paste0(here(), "/Output/Figures/", "Hypertension_Diagnostic_NextThreeCountriesScope.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# *************************************************************
#### Section 13: Plot improved ####
# *************************************************************


Fig2 <- AllCountries_Client_Xlsx %>%
  mutate(BASE = as.factor(BASE)) %>%  # Ensure BASE is treated as a factor
  ggplot(aes(x = Bid1_Numeric, y = Question1, fill = BASE)) +
  geom_ribbon(stat='smooth',
              method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE,
              alpha=0.25,  colour = NA) +
  geom_line(stat='smooth',
            method = "glm",
            method.args = list(family = "binomial"),
            alpha = 1, linewidth = 1.5,
            aes(group = BASE, colour = BASE)) +
  scale_y_continuous(
    labels = c("Current\nrisk", "Risk\nreduction"),
    breaks = c(0, 1),
    limits = seq(0, 1, 1)
  ) +
  theme_bw() +
  facet_wrap(~Country) +
  scale_colour_manual(values = c("black", "blue")) +
  scale_fill_manual(values = c("black", "blue")) +
  coord_cartesian(xlim = c(0, 1000)) +
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
  Fig2,
  device = "png",
  filename = paste0(here(), "/Output/Figures/", "Hypertension_Diagnostic_NextThreeCountriesScope.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


TextFormat <- element_text(size = 10,
                           colour = "black",
                           family = "sans")


# *************************************************************
#### Section 14: Summary stats ####
# *************************************************************

AllCountries_Client_Xlsx %>%
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
  pivot_wider(names_from = Response, values_from = Label)


AllCountries_Client_Xlsx %>%
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


AllCountries_Client_Xlsx %>%
  pivot_longer(cols = H3:H4,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  dplyr::select(-n, -Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)


# *************************************************************
#### Section 15: Experimental ####
# *************************************************************

AllCountries_Client_Xlsx %>%
  pivot_longer(cols = starts_with("A1_"), names_to = "Question", values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response, levels = c("Strongly disagree", "Somewhat disagree",  "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>%
  dplyr::select(-n, -Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>%
  write.csv(quote = FALSE)


Test_A <- AllCountries_Client_Xlsx %>%
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
        axis.text.x = TextFormat,
        axis.text.y = TextFormat,
        text = TextFormat,
        legend.text = TextFormat)


ggsave(
  Test_A,
  device = "png",
  filename = paste0(here(),
                    "/Output/Figures/",
                    "Hypertension_Diagnostic_ResponseCheckA.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


Test_B <- AllCountries_Client_Xlsx %>%
  pivot_longer(cols = c(A3, A4), names_to = "Question", values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  mutate(Response = factor(Response, levels = c(  "Excellent" ,
                                                  "Good",
                                                  "Fair",
                                                  "Poor",
                                                  "Very poor"))) %>%
  ggplot(aes(x = Question, y = n, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Count", title = "Responses by Country and Question", fill = "Response") +
  scale_x_discrete(labels = c("Before", "After"))+
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_bw() +
  facet_wrap(~ Country) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        axis.text.x = TextFormat,
        axis.text.y = TextFormat,
        text = TextFormat,
        legend.text = TextFormat)


ggsave(
  Test_B,
  device = "png",
  filename = paste0(here(),
                    "/Output/Figures/",
                    "Hypertension_Diagnostic_ResponseCheckB.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)
