#### Hypertension: Bid Amounts ####
# Purpose: Produce Table A2: opening and follow-up bids in local currency before screening.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Tables/23_Hypertension_TableA2_BidAmounts.R", encoding = "UTF-8")
# Inputs:
#   Data/Intermediate/Data_AllCountries_WithJapan_Step3.csv.
# Outputs:
#   Output/Tables/Hypertension_TableA2_BidAmounts.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Read the complete merged survey and assign currency labels ####
# *************************************************************


library(tidyverse)


Bid_Data <- read_csv(here::here("Data", "Intermediate", "Data_AllCountries_WithJapan_Step3.csv"),
                     col_select = c(Country, Bid1_Numeric, FollowUp_Bid), show_col_types = FALSE)


Currency_Labels <- c(Austria = "EUR", Belgium = "EUR", Canada = "CAD", Finland = "EUR",
                     Greece = "EUR", Japan = "JPY", Portugal = "EUR", Slovenia = "EUR",
                     "United Kingdom" = "GBP", "United States" = "USD")


stopifnot(setequal(unique(Bid_Data$Country), names(Currency_Labels)),
          !anyNA(Bid_Data), all(Bid_Data$Bid1_Numeric > 0), all(Bid_Data$FollowUp_Bid > 0))


# *************************************************************
#### Section 1: Collect the distinct amounts and export Table A2 ####
# *************************************************************


Bid_Ladder <- Bid_Data %>%
  group_by(Country) %>%
  summarise(First_Bid = paste(sort(unique(Bid1_Numeric)), collapse = ", "),
            Second_Bid = paste(sort(unique(FollowUp_Bid)), collapse = ", "), .groups = "drop") %>%
  mutate(Currency = unname(Currency_Labels[Country])) %>%
  select(Country, Currency, First_Bid, Second_Bid) %>%
  arrange(Country)


write_delim(Bid_Ladder, here::here("Output", "Tables", "Hypertension_TableA2_BidAmounts.txt"),
             delim = "#")
