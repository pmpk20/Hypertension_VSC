#### Hypertension: Check Replication ####
# Purpose: Check script syntax, analysis keys, model samples, expected outputs and completion of the master run.
# Author: Anonymised for peer review.
# Prerequisites: Complete scripts 01–32 using the master script.
# Run from the project root: source("Scripts/Setup/33_Hypertension_Setup_CheckReplication.R", encoding = "UTF-8")
# Inputs:
#   Current R scripts; Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds and Hypertension_Model_WillingnessToPayDeterminants.rds.
#   Scripts/Replication_Expected_Outputs.txt and its listed files.
#   Output/Replication/Step_Timings.csv.
# Outputs:
#   Validation messages; no files modified.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Check every current R script ####
# *************************************************************


if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United Kingdom.utf8")


Script_Files <- c("00_Hypertension_Setup_ReplicatePaper.R", list.files("Scripts", pattern = "[.]R$", recursive = TRUE, full.names = TRUE))


Script_Files <- Script_Files[!grepl("/Archive/", Script_Files)]


invisible(lapply(Script_Files, parse, encoding = "UTF-8"))


# *************************************************************
#### Section 1: Check the rebuilt sample and composite respondent key ####
# *************************************************************


Analysis_Data <- read.csv("Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv", check.names = FALSE)


stopifnot(nrow(Analysis_Data) == 9496L,
          length(unique(Analysis_Data$Country)) == 10L,
          !anyNA(Analysis_Data[c("Country", "Respondent_Serial")]),
          !anyDuplicated(Analysis_Data[c("Country", "Respondent_Serial")]),
          all(Analysis_Data$Question1 %in% 0:1),
          all(Analysis_Data$Question2 %in% 0:1),
          all(Analysis_Data$RR %in% c(10, 20)),
          all(is.finite(Analysis_Data$LogBid1)),
          all(is.finite(Analysis_Data$LogBid2)))


# *************************************************************
#### Section 2: Check model sample sizes and finite coefficients ####
# *************************************************************


Country_Cache <- readRDS("Output/Models/Hypertension_Model_PooledAndCountryModels.rds")


Determinants_Models <- readRDS("Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds")


stopifnot(length(Country_Cache$country_models) == 10L,
          identical(names(Determinants_Models), c("Baseline", "With controls", "Health controls")))


for (Fitted_Model in c(list(Country_Cache$pooled_model), Determinants_Models)) {
  stopifnot(nrow(Fitted_Model$data) == nrow(Analysis_Data),
            all(is.finite(Fitted_Model$coefficients)),
            isTRUE(Fitted_Model$convergence))
}


for (Country_Name in names(Country_Cache$country_models)) {
  Country_Model <- Country_Cache$country_models[[Country_Name]]
  stopifnot(nrow(Country_Model$data) == sum(Analysis_Data$Country == Country_Name),
            all(is.finite(Country_Model$coefficients)),
            isTRUE(Country_Model$convergence))
}


# *************************************************************
#### Section 3: Check the expected output files ####
# *************************************************************


Expected_Outputs <- readLines("Scripts/Replication_Expected_Outputs.txt", warn = FALSE)


stopifnot(all(file.exists(Expected_Outputs)), all(file.info(Expected_Outputs)$size > 0))


# *************************************************************
#### Section 4: Check that every direct step finished ####
# *************************************************************


Step_Records <- read.csv("Output/Replication/Step_Timings.csv")


Runner_Lines <- readLines("00_Hypertension_Setup_ReplicatePaper.R", warn = FALSE)


Expected_Steps <- sub('^Run_Script\\("([^"]+)"\\).*$', '\\1',
                      grep('^Run_Script\\("', Runner_Lines, value = TRUE))


stopifnot(identical(Step_Records$Script, Expected_Steps), all(Step_Records$Status == "Passed"))


message("Replication checks passed: syntax, 9,496 unique respondents, model samples and completed steps.")
