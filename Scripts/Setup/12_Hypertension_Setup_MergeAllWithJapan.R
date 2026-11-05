#### Hypertension: Merge All With Japan ####
# Purpose: Merge Japan with the other nine countries and construct analysis variables.
# Author: Anonymised for peer review.
# Prerequisites: Run scripts 10 and 11.
# Run from the project root: source("Scripts/Setup/12_Hypertension_Setup_MergeAllWithJapan.R", encoding = "UTF-8")
# Inputs:
#   Data/Intermediate/Data_AllCountries_Step3.csv and Data_Japan_New_Cleaned_Step2PPP.csv.
# Outputs:
#   Data/Intermediate/Data_AllCountries_WithJapan_Step3.csv (all ten countries before screening).
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
#### Section 2: Import datasets ####
# *************************************************************


## Start by importing SAV data
## here() is the best practice for filepaths
## and I use %>% everywhere as it's easier to read
Data_Japan <-
  WORK("Data_Japan_New_Cleaned_Step2PPP.csv") %>%
  fread() %>%
  data.frame()


Data_Rest <- WORK("Data_AllCountries_Step3.csv") %>%
  fread() %>%
  data.frame()


# *************************************************************
#### Section 3: S1A: Add missing columns to Japan ####
# *************************************************************

## Detect differences with:
# setdiff(
#   Data_Rest %>% colnames(),
#   Data_Japan %>% colnames()
# )


Data_Rest$JP01EDU <- 0 %>% as.character()


Data_Rest$Region_JP <- 0 %>% as.character()


Data_Rest$INCJP1 <- 0 %>% as.character()


Data_Rest$INCJP2 <- 0 %>% as.character()


Data_Rest$Dummy_HighEducation_JP <- 0


Data_Rest$INCJP1_MissingDummy <- 0


Data_Rest$INCJP2_MissingDummy <- 0


Data_Rest$INCJP1_Numeric <- 0


Data_Rest$INCJP2_Numeric <- 0


Data_Rest$INC2_Missing <- 0


Data_Rest$INCJP1_Numeric_PlusMissing <- 0


Data_Rest$INCJP1_Numeric_PlusMissing_PPP <- 0


Data_Rest$Japan <- 0


# *************************************************************
#### Section 4: S1B: Add missing columns to Rest ####
# *************************************************************

#
# Test <- Data_Japan %>%
#   group_by(Country) %>%
#   mutate(
#     "Speeders_Survey_CountryThreshold" =
#       median(Speeders_Survey_CompletionTime) %>% multiply_by(0.48),
#     "Slowers_Survey_CountryThreshold" =
#       median(Speeders_Survey_CompletionTime) %>% multiply_by(1.48)
#   )
#
#
#
# Data_Japan$Speeders_Survey_CountryThreshold <- Test$Speeders_Survey_CountryThreshold
# Data_Japan$Slowers_Survey_CountryThreshold <- Test$Slowers_Survey_CountryThreshold
#

Data_Japan$Austria <- 0


Data_Japan$Belgium <- 0


Data_Japan$Canada <- 0


Data_Japan$Slovenia <- 0


Data_Japan$UnitedKingdom <- 0


Data_Japan$Finland <- 0


Data_Japan$Greece <- 0


Data_Japan$Portugal <- 0


Data_Japan$INCBE1_Always <- 0 %>% as.character()


Data_Japan$INCBE2_Always <- 0 %>% as.character()


Data_Japan$INCAT1_MissingDummy <- 0


Data_Japan$INCUK1_MissingDummy <- 0


Data_Japan$INCCA1_MissingDummy <- 0


Data_Japan$INCSI1_MissingDummy <- 0


Data_Japan$INCBE1_Always_MissingDummy <- 0


Data_Japan$INCGR1_MissingDummy <- 0


Data_Japan$INCGR2_MissingDummy <- 0


Data_Japan$INCAT2_MissingDummy <- 0


Data_Japan$INCUK2_MissingDummy <- 0


Data_Japan$INCCA2_MissingDummy <- 0


Data_Japan$INCSI2_MissingDummy <- 0


Data_Japan$INCBE2_Always_MissingDummy <- 0


Data_Japan$Dummy_HighEducation_UK <- 0


Data_Japan$Dummy_HighEducation_AUT <- 0


Data_Japan$Dummy_HighEducation_Bel <- 0


Data_Japan$Dummy_HighEducation_Slo <- 0


Data_Japan$Dummy_HighEducation_CAD <- 0


Data_Japan$Weights_Austria <- 0


Data_Japan$Weights_Belgium <- 0


Data_Japan$Weights_Canada <- 0


Data_Japan$Weights_Slovenia <- 0


Data_Japan$Weights_UnitedKingdom <- 0


Data_Japan$Weights_Finland <- 0


Data_Japan$Weights_Greece <- 0


Data_Japan$Weights_Portugal <- 0


Data_Japan$INCFI1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCGR1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCPT1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCAT1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCUK1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCCA1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCSI1_Numeric_PlusMissing_PPP <- 0


Data_Japan$INCBE1_Always_Numeric_PlusMissing_PPP <- 0


Data_Japan$UnitedStates <- 0


Data_Japan$RR_Scaled <- 0


Data_Japan$BASE_Numeric_Scaled <- 0


Data_Japan$Canada_Weighted <- 0


Data_Japan$Austria_Weighted <- 0


Data_Japan$Belgium_Weighted <- 0


Data_Japan$Slovenia_Weighted <- 0


Data_Japan$UnitedKingdom_Weighted <- 0


Data_Japan$Finland_Weighted <- 0


Data_Japan$Greece_Weighted <- 0


Data_Japan$Portugal_Weighted <- 0


Data_Japan$UnitedStates_Weighted <- 0


Data_Japan$INCGR1 <- 0 %>% as.character()


Data_Japan$INCGR1_Numeric <- 0


Data_Japan$INCGR1_Numeric_PlusMissing <- 0


Data_Japan$INCGR2 <- 0 %>% as.character()


Data_Japan$INCGR2_Numeric <- 0


Data_Japan$INCGR2_Numeric_PlusMissing <- 0


Data_Japan$INCGR2_Numeric_PlusMissing_PPP <- 0


Data_Japan <- Data_Japan %>%
  mutate(Q5a = as.numeric(na_if(Q5a, "I don't know"))) %>%
  mutate(Q5a = ifelse(is.na(Q5a), 0, Q5a)) %>%
  mutate(Q6a = as.numeric(na_if(Q6a, "I don't know"))) %>%
  mutate(Q6a = ifelse(is.na(Q6a), 0, Q6a)) %>%
  mutate(Q6b = as.numeric(na_if(Q6b, "I don't know"))) %>%
  mutate(Q6b = ifelse(is.na(Q6b), 0, Q6b)) %>%
  mutate(Q6c = as.numeric(na_if(Q6c, "I don't know"))) %>%
  mutate(Q6c = ifelse(is.na(Q6c), 0, Q6c))


# *************************************************************
#### Section 5: Merge data by row binding ####
# *************************************************************


Data <- dplyr::bind_rows(
  Data_Rest,
  Data_Japan
)


# *************************************************************
#### Section 6: Update ELPTFI ####
# *************************************************************

# Education dummies -------------------------------------------------------------
Data$Dummy_HighEducation_Fi <- ifelse(Data$ISCED == "High" & Data$Country == "Finland", 1, 0)


Data$Dummy_HighEducation_EL <- ifelse(Data$ISCED == "High" & Data$Country == "Greece", 1, 0)


Data$Dummy_HighEducation_PT <- ifelse(Data$ISCED == "High" & Data$Country == "Portugal", 1, 0)


Data$Dummy_HighEducation_US <- ifelse(Data$ISCED == "High" &
                                        Data$Country == "United States",
                                      1,
                                      0)


## Simple interactions with existing weights
Data$Weights_US <- ((Data$weight %>% as.numeric()) * Data$UnitedStates %>% as.numeric())


# *************************************************************
#### Section 7: Import missing data ####
# *************************************************************
#
#
# ## Importing Finland, Greece, Portugal income here
# Data_ELPTFI <- here("Data",
#                     "Data_ELPTFI_Cleaned_Step2PPP.csv") %>%
#   fread() %>%
#   data.frame()
#
# Data_US <- here("Data",
#                 "Data_US_New_Cleaned_Step2PPP.csv") %>%
#   fread() %>%
#   data.frame()
#
#
#
# # **********************************************************************************
# #### Section 7B: Grab country-specific income ####
# # **********************************************************************************
# Data$INC_NUMERIC <- 0
#
#
# ## Select just the country-specific values
# Income_Austria <- Data %>% dplyr::filter(Country == "Austria") %>% dplyr::select(INCAT1_Numeric_PlusMissing)
# Income_Belgium <- Data %>% dplyr::filter(Country == "Belgium") %>% dplyr::select(INCBE1_Always_Numeric_PlusMissing)
# Income_Canada <- Data %>% dplyr::filter(Country == "Canada") %>% dplyr::select(INCCA1_Numeric_PlusMissing)
# Income_Slovenia <- Data %>% dplyr::filter(Country == "Slovenia") %>% dplyr::select(INCSI1_Numeric_PlusMissing)
# Income_UnitedKingdom <- Data %>%  dplyr::filter(Country == "United Kingdom") %>% dplyr::select(INCUK1_Numeric_PlusMissing)
#
# Income_Finland <- Data_ELPTFI %>% dplyr::filter(Country == "Finland") %>% dplyr::select(INCFI1_Numeric_PlusMissing)
# Income_Greece <- Data_ELPTFI %>% dplyr::filter(Country == "Greece") %>% dplyr::select(INCGR1_Numeric_PlusMissing)
# Income_Portugal <- Data_ELPTFI %>% dplyr::filter(Country == "Portugal") %>% dplyr::select(INCPT1_Numeric_PlusMissing)
# Income_Japan <- Data_Japan %>% dplyr::filter(Country == "Japan") %>% dplyr::select(INCJP1_Numeric_PlusMissing)
# Income_UnitedStates <- Data_US %>% dplyr::filter(Country == "United States") %>% dplyr::select(INCUS1_Numeric_PlusMissing)
#
#
# ## Give the same header for ease of merge
# colnames(Income_Austria) <- "Income"
# colnames(Income_Belgium) <- "Income"
# colnames(Income_Canada) <- "Income"
# colnames(Income_Finland) <- "Income"
# colnames(Income_Greece) <- "Income"
# colnames(Income_Japan) <- "Income"
# colnames(Income_Portugal) <- "Income"
# colnames(Income_Slovenia) <- "Income"
# colnames(Income_UnitedKingdom) <- "Income"
# colnames(Income_UnitedStates) <- "Income"
#
#
# ## Merge here in country-order
# Income_Vector <- rbind(
#   Income_Austria,
#   Income_Belgium,
#   Income_Canada,
#   Income_Finland,
#   Income_Greece,
#   Income_Japan,
#   Income_Portugal,
#   Income_Slovenia,
#   Income_UnitedKingdom,
#   Income_UnitedStates
# )
#
#
# # **********************************************************************************
# #### Section 7C: Get country-specific income dummies ####
# # **********************************************************************************
#
#
# ## Select just the country-specific values
# Income_MissingDummy_Austria <- Data %>% dplyr::filter(Country == "Austria") %>% dplyr::select(INCAT1_MissingDummy)
# Income_MissingDummy_Belgium <- Data %>% dplyr::filter(Country == "Belgium") %>% dplyr::select(INCBE1_Always_MissingDummy)
# Income_MissingDummy_Canada <- Data %>% dplyr::filter(Country == "Canada") %>% dplyr::select(INCCA1_MissingDummy)
# Income_MissingDummy_Finland <- Data_ELPTFI %>% dplyr::filter(Country == "Finland") %>% dplyr::select(INCFI1_MissingDummy) ##
# Income_MissingDummy_Greece <- Data_ELPTFI %>% dplyr::filter(Country == "Greece") %>% dplyr::select(INCGR1_MissingDummy) ##
# Income_MissingDummy_Japan <- Data %>% dplyr::filter(Country == "Japan") %>% dplyr::select(INCJP1_MissingDummy)
# Income_MissingDummy_Portugal <- Data_ELPTFI %>% dplyr::filter(Country == "Portugal") %>% dplyr::select(INCPT1_MissingDummy) ##
# Income_MissingDummy_Slovenia <- Data %>% dplyr::filter(Country == "Slovenia") %>% dplyr::select(INCSI1_MissingDummy)
# Income_MissingDummy_UnitedKingdom <- Data %>% dplyr::filter(Country == "United Kingdom") %>% dplyr::select(INCUK1_MissingDummy)
# Income_MissingDummy_UnitedStates <- Data_US %>% dplyr::filter(Country == "United States") %>% dplyr::select(INCUS1_MissingDummy) ##
#
#
# ## Give the same header for ease of merge
# colnames(Income_MissingDummy_Austria) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Belgium) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Canada) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Finland) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Greece) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Japan) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Portugal) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_Slovenia) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_UnitedKingdom) <- "Income_MissingDummy"
# colnames(Income_MissingDummy_UnitedStates) <- "Income_MissingDummy"
#
#
# ## Row bind in country-order
# Income_MissingDummy_Vector <- rbind(
#   Income_MissingDummy_Austria,
#   Income_MissingDummy_Belgium,
#   Income_MissingDummy_Canada,
#   Income_MissingDummy_Finland,
#   Income_MissingDummy_Greece,
#   Income_MissingDummy_Japan,
#   Income_MissingDummy_Portugal,
#   Income_MissingDummy_Slovenia,
#   Income_MissingDummy_UnitedKingdom,
#   Income_MissingDummy_UnitedStates
# )
#
#
#
# ## Now rearrange our existing data to be country-order
# Data_Arranged <- Data %>% dplyr::arrange(factor(Country, levels = c(
#   "Austria",
#   "Belgium",
#   "Canada",
#   "Finland",
#   "Greece",
#   "Japan",
#   "Portugal",
#   "Slovenia",
#   "United Kingdom",
#   "United States"
# )))
#
#
# Data_Arranged$INC_All_Numeric2 <- Income_Vector$Income
# Data_Arranged$INC1_MissingDummy <- Income_MissingDummy_Vector$Income_MissingDummy
#
#
#
# # **********************************************************************************
# #### Section 7B: Convert all  ####
# # **********************************************************************************
#
#
#
# ## Factors from OECD
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
#
#
#
# # First, reshape ConversionFactors to long format
# ConversionFactors_long <- ConversionFactors %>%
#   pivot_longer(cols = everything(),
#                names_to = "Country",
#                values_to = "PPP_factor") %>%
#   mutate(Country = str_replace_all(Country, "\\.", " "))
#
#
#
# # Now join with your main data and apply adjustments
# Data_PPPlus <- Data_Arranged %>%
#   left_join(ConversionFactors_long, by = "Country") %>%
#   mutate(
#     BIDTOT1_PPP = BIDTOT1_Numeric / PPP_factor,
#     BIDTOT2_PPP = BIDTOT2_Numeric / PPP_factor,
#     BIDTOT3_PPP = BIDTOT3_Numeric / PPP_factor,
#     Bid1_Numeric_PPP = Bid1_Numeric / PPP_factor,
#     Bid2_Numeric_PPP = FollowUp_Bid / PPP_factor,
#     INC1_All_Adjusted = INC1_All / PPP_factor,
#     INC_All_Numeric2_PPP = INC_All_Numeric2 / PPP_factor,
#   ) %>%
#   dplyr::select(-PPP_factor) # Remove the factor column if no longer needed
#
#
# #
# #
# #
# # Data_PPPlus$INCJP1_Numeric_PlusMissing_PPP <- Data_PPPlus$INCJP1_Numeric_PlusMissing %>% divide_by(ConversionFactors$Japan)
# #
# #
# # ## Coalesce stitches things together when there are NAs
# # Data_PPPlus$INC1_All_Adjusted <- coalesce(
# #   Data_PPPlus$INCJP1_Numeric_PlusMissing_PPP
# # )
# ## Log monetary variables
# Data_PPPlus$LogIncome <- Data_PPPlus$INC_All_Numeric2_PPP %>% log()
# Data_PPPlus$LogIncomeBackup <- Data_PPPlus$INC1_All_Adjusted %>% log()
# Data_PPPlus$LogBid1 <- Data_PPPlus$Bid1_Numeric_PPP %>% log()
# Data_PPPlus$LogBid2 <- Data_PPPlus$Bid2_Numeric_PPP %>% log()


# *************************************************************
#### Section 8: Misc fixes ####
# *************************************************************

## Rescale risk reduction to be 0 - 1 i.e. 1 in 1000 or 2 in 1000
Data$RR_Scaled <- Data$RR  %>% divide_by(1000)


Data$BASE_Numeric_Scaled <- Data$BASE_Numeric  %>% divide_by(1000)


## Create new country weighted variables
Data$Canada_Weighted <- (Data$Canada * Data$weight)


Data$Austria_Weighted <- (Data$Austria * Data$weight)


Data$Belgium_Weighted <- (Data$Belgium * Data$weight)


Data$Slovenia_Weighted <- (Data$Slovenia * Data$weight)


Data$UnitedKingdom_Weighted <- (Data$UnitedKingdom * Data$weight)


Data$Finland_Weighted <- (Data$Finland * Data$weight)


Data$Greece_Weighted <- (Data$Greece * Data$weight)


Data$Portugal_Weighted <- (Data$Portugal * Data$weight)


Data$UnitedStates_Weighted <- (Data$UnitedStates  * Data$weight)


Data$Japan_Weighted <- (Data$Japan  * Data$weight)


#
# Data_PPPlus <- Data_PPPlus %>%
#   mutate(Q5a = as.numeric(na_if(Q5a, "I don't know"))) %>%
#   mutate(Q5a = ifelse(is.na(Q5a), 0, Q5a)) %>%
#   mutate(Q6a = as.numeric(na_if(Q6a, "I don't know"))) %>%
#   mutate(Q6a = ifelse(is.na(Q6a), 0, Q6a)) %>%
#   mutate(Q6b = as.numeric(na_if(Q6b, "I don't know"))) %>%
#   mutate(Q6b = ifelse(is.na(Q6b), 0, Q6b)) %>%
#   mutate(Q6c = as.numeric(na_if(Q6c, "I don't know"))) %>%
#   mutate(Q6c = ifelse(is.na(Q6c), 0, Q6c))


# *************************************************************
#### Section 9: Export ####
# *************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data %>%
  fwrite(sep = ",",
         WORK("Data_AllCountries_WithJapan_Step3.csv"))


# END OF SCRIPT ****************************
