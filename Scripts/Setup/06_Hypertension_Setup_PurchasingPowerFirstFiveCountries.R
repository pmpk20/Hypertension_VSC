#### Hypertension: Purchasing Power First Five Countries ####
# Purpose: Convert the first five countries' bids and income to the common purchasing-power basis.
# Author: Anonymised for peer review.
# Prerequisites: Run script 05.
# Run from the project root: source("Scripts/Setup/06_Hypertension_Setup_PurchasingPowerFirstFiveCountries.R", encoding = "UTF-8")
# Inputs:
#   Data/Intermediate/AllCountries_CleanedData.csv; purchasing-power factors specified here.
# Outputs:
#   Data/Intermediate/AllCountries_CleanedData_PPP.csv.
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

## Where pipeline intermediates are written.  Default: Data/Intermediate/ in
## this repo (git-ignored).  Nothing here is committed.
if (!dir.exists(here::here("Data", "Intermediate"))) {
  dir.create(here::here("Data", "Intermediate"), recursive = TRUE, showWarnings = FALSE)
}


#' Path to a pipeline intermediate (read or write).
WORK <- function(...) file.path(here::here("Data", "Intermediate"), ...)


# *************************************************************
#### Section 2: Import data ####
# *************************************************************


## Start by importing cleaned data
## here() for filepaths
## data.table::fread() is the fastest way to import CSV files
AllCountries_Client_Xlsx <- WORK("AllCountries_CleanedData.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 3: Country PPP data ####
# *************************************************************
## Here: https://data.oecd.org/conversion/purchasing-power-parities-ppp.htm


## Use 2023 purchasing power parities for all ten countries in the paper.
## The master script requires this basis; the optional legacy setting below
## retains earlier conversion factors for comparison.


SWACHE_PPP_BASIS <- Sys.getenv("SWACHE_PPP_BASIS", unset = "2023")


## OLD (pre-2023) Factors from OECD -- the basis the PUBLISHED wave-1 columns
## are actually on.
Legacy_Conversion_Factors <- data.frame(
  "Austria" = 0.803598,
  "Belgium" = 0.796006,
  "Canada" = 1.281466,
  "Finland" = 0.872997,
  "Greece" = 0.576645,
  "Japan" = 105.200318,
  "Portugal" = 0.590032,
  "Slovenia" = 0.587038,
  "United Kingdom" = 0.747639,
  "United States" =	1
)


## 2023 Factors from OECD
Conversion_Factors_2023 <- data.frame(
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


Conversion_Factors <- switch(
  SWACHE_PPP_BASIS,
  "2023"   = Conversion_Factors_2023,
  "legacy" = Legacy_Conversion_Factors,
  stop("SWACHE_PPP_BASIS must be \"2023\" (default; internally consistent across ",
       "all ten countries) or \"legacy\" (reproduces the published mixed-basis ",
       "wave-1 columns exactly). Got: ", SWACHE_PPP_BASIS, call. = FALSE)
)


message("[02_] wave-1 PPP basis: ", SWACHE_PPP_BASIS,
        if (identical(SWACHE_PPP_BASIS, "2023"))
          "  (default; internally consistent across all ten countries -- does NOT reproduce the published file, moves LogBid1/LogBid2 on all wave-1 rows)"
        else "  (reproduces the published file exactly, but wave 1 is then on a DIFFERENT PPP vintage from waves 2-4)")


# *************************************************************
#### Section 4: Bid levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
AllCountries_Client_Xlsx$Bid1_Numeric_PPP <-
  ifelse(
    AllCountries_Client_Xlsx$Country == "United Kingdom",
    AllCountries_Client_Xlsx$Bid1_Numeric %>% divide_by(Conversion_Factors$United.Kingdom),
    ifelse(
      AllCountries_Client_Xlsx$Country == "Canada",
      AllCountries_Client_Xlsx$Bid1_Numeric %>% divide_by(Conversion_Factors$Canada),
      ifelse(
        AllCountries_Client_Xlsx$Country == "Slovenia",
        AllCountries_Client_Xlsx$Bid1_Numeric %>% divide_by(Conversion_Factors$Slovenia),
        ifelse(
          AllCountries_Client_Xlsx$Country == "Austria",
          AllCountries_Client_Xlsx$Bid1_Numeric %>% divide_by(Conversion_Factors$Austria),
          ifelse(
            AllCountries_Client_Xlsx$Country == "Belgium",
            AllCountries_Client_Xlsx$Bid1_Numeric %>% divide_by(Conversion_Factors$Belgium),
            AllCountries_Client_Xlsx$Bid1_Numeric
          )
        )
      )
    )
  )


## Bid levels for the follow up question
AllCountries_Client_Xlsx$Bid2_Numeric_PPP <-
  ifelse(
    AllCountries_Client_Xlsx$Country == "United Kingdom",
    AllCountries_Client_Xlsx$FollowUp_Bid %>% divide_by(Conversion_Factors$United.Kingdom),
    ifelse(
      AllCountries_Client_Xlsx$Country == "Canada",
      AllCountries_Client_Xlsx$FollowUp_Bid  %>% divide_by(Conversion_Factors$Canada),
      ifelse(
        AllCountries_Client_Xlsx$Country == "Slovenia",
        AllCountries_Client_Xlsx$FollowUp_Bid  %>% divide_by(Conversion_Factors$Slovenia),
        ifelse(
          AllCountries_Client_Xlsx$Country == "Austria",
          AllCountries_Client_Xlsx$FollowUp_Bid  %>% divide_by(Conversion_Factors$Austria),
          ifelse(
            AllCountries_Client_Xlsx$Country == "Belgium",
            AllCountries_Client_Xlsx$FollowUp_Bid  %>% divide_by(Conversion_Factors$Belgium),
            AllCountries_Client_Xlsx$FollowUp_Bid
          )
        )
      )
    )
  )


# *************************************************************
#### Section 5: Income levels ####
# *************************************************************


## divide bid level by country-specific PPP factor
AllCountries_Client_Xlsx$INCAT1_Numeric_PlusMissing_PPP <- AllCountries_Client_Xlsx$INCAT1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Austria)


AllCountries_Client_Xlsx$INCUK1_Numeric_PlusMissing_PPP <- AllCountries_Client_Xlsx$INCUK1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$United.Kingdom)


AllCountries_Client_Xlsx$INCCA1_Numeric_PlusMissing_PPP <- AllCountries_Client_Xlsx$INCCA1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Canada)


AllCountries_Client_Xlsx$INCSI1_Numeric_PlusMissing_PPP <- AllCountries_Client_Xlsx$INCSI1_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Slovenia)


AllCountries_Client_Xlsx$INCBE1_Always_Numeric_PlusMissing_PPP <- AllCountries_Client_Xlsx$INCBE1_Always_Numeric_PlusMissing %>% divide_by(Conversion_Factors$Belgium)


## Coalesce stitches things together when there are NAs
AllCountries_Client_Xlsx$INC1_All_Adjusted <- coalesce(
  AllCountries_Client_Xlsx$INCAT1_Numeric_PlusMissing_PPP,
  AllCountries_Client_Xlsx$INCUK1_Numeric_PlusMissing_PPP,
  AllCountries_Client_Xlsx$INCCA1_Numeric_PlusMissing_PPP,
  AllCountries_Client_Xlsx$INCSI1_Numeric_PlusMissing_PPP,
  AllCountries_Client_Xlsx$INCBE1_Always_Numeric_PlusMissing_PPP
)


## Log monetary variables
AllCountries_Client_Xlsx$LogIncome <- AllCountries_Client_Xlsx$INC1_All_Adjusted %>% log()


AllCountries_Client_Xlsx$LogBid1 <- AllCountries_Client_Xlsx$Bid1_Numeric_PPP %>% log()


AllCountries_Client_Xlsx$LogBid2 <- AllCountries_Client_Xlsx$Bid2_Numeric_PPP %>% log()


## Missing income indicators here
AllCountries_Client_Xlsx$INC1_MissingDummy <- coalesce(
  AllCountries_Client_Xlsx$INCAT1_MissingDummy,
  AllCountries_Client_Xlsx$INCUK1_MissingDummy,
  AllCountries_Client_Xlsx$INCCA1_MissingDummy,
  AllCountries_Client_Xlsx$INCSI1_MissingDummy,
  AllCountries_Client_Xlsx$INCBE1_Always_MissingDummy
)


## Missing income indicators here
AllCountries_Client_Xlsx$INC2_MissingDummy <- coalesce(
  AllCountries_Client_Xlsx$INCAT2_MissingDummy,
  AllCountries_Client_Xlsx$INCUK2_MissingDummy,
  AllCountries_Client_Xlsx$INCCA2_MissingDummy,
  AllCountries_Client_Xlsx$INCSI2_MissingDummy,
  AllCountries_Client_Xlsx$INCBE2_Always_MissingDummy
)


# *************************************************************
#### Section 6: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
AllCountries_Client_Xlsx %>%
  fwrite(sep = ",",
         WORK("AllCountries_CleanedData_PPP.csv"))


# END OF SCRIPT **************************************************************
