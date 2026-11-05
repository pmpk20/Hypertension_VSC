#### Hypertension: Replicate Paper ####
# Purpose: Rebuild the analysis sample, estimate the models and produce every generated table and figure in the paper and supplementary information.
# Author: Anonymised for peer review.
# Prerequisites: None; start a fresh R session in the project root.
# Run from the project root: source("00_Hypertension_Setup_ReplicatePaper.R", encoding = "UTF-8")
# Inputs:
#   Data/SurveyData/*_Responses.xlsx and *_Timers.csv (ten countries).
#   Data/Other/ (purchasing power, gross domestic product and health spending).
#   renv.lock and Replication_Packages.txt.
# Outputs:
#   Data/Intermediate/ and Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Tables/, Output/Figures/ and Output/Models/.
#   Output/Replication/Input_Checksums.csv, Session_Info.txt and Step_Timings.csv.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Establish a reproducible session ####
# *************************************************************


if (!file.exists("HypertensionPaper.Rproj")) {
  stop("Run this script from the HypertensionPaper project root.", call. = FALSE)
}


if (.Platform$OS.type == "windows") {
  Locale_Result <- Sys.setlocale("LC_CTYPE", "English_United Kingdom.utf8")
  if (!nzchar(Locale_Result)) stop("A UTF-8 locale is required for survey labels.")
}


options(encoding = "UTF-8", stringsAsFactors = FALSE, width = 120)


RNGkind("Mersenne-Twister", "Inversion", "Rejection")


set.seed(20261022)


if (nzchar(Sys.getenv("SWACHE_PPP_BASIS")) && Sys.getenv("SWACHE_PPP_BASIS") != "2023") {
  stop("Paper replication requires SWACHE_PPP_BASIS=2023; unset the legacy override.")
}


# *************************************************************
#### Section 1: Check input files before estimation ####
# *************************************************************


Survey_Countries <- c("Austria", "Belgium", "Canada", "Finland", "Greece", "Japan", "Portugal",
                      "Slovenia", "United_Kingdom", "United_States")


Required_Inputs <- c(
  file.path("Data", "SurveyData", paste0("SWACHE_Hypertension_", Survey_Countries, "_Responses.xlsx")),
  file.path("Data", "SurveyData", paste0("SWACHE_Hypertension_", Survey_Countries, "_Timers.csv")),
  file.path("Data", "Other", c("OECD_GDP_Per_Capita.csv", "OECD_OutOfPocket_Health_Share.csv",
                               "OECD_PPP_ActualIndividualConsumption.csv"))
)


Missing_Inputs <- Required_Inputs[!file.exists(Required_Inputs)]


if (length(Missing_Inputs)) stop("Missing inputs: ", paste(Missing_Inputs, collapse = ", "))


# *************************************************************
#### Section 2: Record inputs, software and step timings ####
# *************************************************************


dir.create("Output/Replication", recursive = TRUE, showWarnings = FALSE)


for (Output_Folder in c("Tables", "Figures", "Models")) {
  dir.create(file.path("Output", Output_Folder), recursive = TRUE, showWarnings = FALSE)
}


write.csv(data.frame(File = Required_Inputs, MD5 = unname(tools::md5sum(Required_Inputs))),
          "Output/Replication/Input_Checksums.csv", row.names = FALSE)


writeLines(capture.output(sessionInfo()), "Output/Replication/Session_Info.txt")


Step_Records <- data.frame(Script = character(), Status = character(), Seconds = numeric())


Run_Script <- function(Script_Path) {
  message("Running ", Script_Path)
  Started_At <- proc.time()[["elapsed"]]
  Step_Status <- "Failed"
  Initial_Sinks <- sink.number()
  on.exit({
    while (sink.number() > Initial_Sinks) sink()
    Step_Records <<- rbind(Step_Records, data.frame(Script = Script_Path, Status = Step_Status,
                                                 Seconds = proc.time()[["elapsed"]] - Started_At))
    write.csv(Step_Records, "Output/Replication/Step_Timings.csv", row.names = FALSE)
    writeLines(capture.output(sessionInfo()), "Output/Replication/Session_Info.txt")
  })
  source(Script_Path, local = .GlobalEnv, encoding = "UTF-8")
  Step_Status <- "Passed"
}


# *************************************************************
#### Section 3: Prepare packages and shared calculations ####
# *************************************************************


Run_Script("Scripts/Setup/01_Hypertension_Setup_RestoreEnvironment.R")


Run_Script("Scripts/Setup/02_Hypertension_Setup_CheckEnvironment.R")


Run_Script("Scripts/Models/03_Hypertension_Model_DefineEstimators.R")


Run_Script("Scripts/Setup/04_Hypertension_Setup_SharedCalculations.R")


# *************************************************************
#### Section 4: Rebuild the analysis sample ####
# *************************************************************


library(here)


library(tidyverse)


library(data.table)


library(janitor)


WORK <- function(...) file.path(here::here("Data", "Intermediate"), ...)


SWACHE_KEY <- c("Country", "Respondent_Serial")


Run_Script("Scripts/Setup/05_Hypertension_Setup_CleanFirstFiveCountries.R")


Run_Script("Scripts/Setup/06_Hypertension_Setup_PurchasingPowerFirstFiveCountries.R")


Run_Script("Scripts/Setup/07_Hypertension_Setup_CleanNextThreeCountries.R")


Run_Script("Scripts/Setup/08_Hypertension_Setup_MergeFirstFiveNextThree.R")


Run_Script("Scripts/Setup/09_Hypertension_Setup_CleanUnitedStates.R")


Run_Script("Scripts/Setup/10_Hypertension_Setup_MergeFirstEightPlusUnitedStates.R")


Run_Script("Scripts/Setup/11_Hypertension_Setup_CleanJapan.R")


Run_Script("Scripts/Setup/12_Hypertension_Setup_MergeAllWithJapan.R")


Run_Script("Scripts/Setup/13_Hypertension_Setup_BuildAnalysisSample.R")


# *************************************************************
#### Section 5: Fit the models and produce the main-text results ####
# *************************************************************


Run_Script("Scripts/Models/14_Hypertension_Model_PooledAndCountryModels.R")


Run_Script("Scripts/Tables/15_Hypertension_TableA5_CountryModels.R")


Run_Script("Scripts/Models/16_Hypertension_Model_WillingnessToPayDeterminants.R")


Run_Script("Scripts/Tables/17_Hypertension_Table_DeterminantEstimates.R")


Run_Script("Scripts/Tables/18_Hypertension_Table_KrinskyRobbIntervals.R")


Run_Script("Scripts/Tables/19_Hypertension_Table1_CountryWillingnessToPayAndCaseValues.R")


Run_Script("Scripts/Tables/20_Hypertension_Table2_WillingnessToPayDeterminants.R")


Run_Script("Scripts/Figures/21_Hypertension_Figure3_CountryWillingnessToPayDistributions.R")


# *************************************************************
#### Section 6: Produce the remaining tables, figures and supplementary checks ####
# *************************************************************


Run_Script("Scripts/Tables/22_Hypertension_TableA1_VariableCoding.R")


Run_Script("Scripts/Tables/23_Hypertension_TableA2_BidAmounts.R")


Run_Script("Scripts/Figures/24_Hypertension_FigureA2_BidAcceptance.R")


Run_Script("Scripts/Tables/25_Hypertension_Table3_CrossCountryRegressions.R")


Run_Script("Scripts/Tables/26_Hypertension_TableA3_AlternativeDistributions.R")


Run_Script("Scripts/Tables/27_Hypertension_TableA4_ScreeningRobustness.R")


Run_Script("Scripts/Tables/28_Hypertension_TableA6_PairwiseCountryComparisons.R")


Run_Script("Scripts/Figures/29_Hypertension_FigureA1_RiskGroupWillingnessToPay.R")


Run_Script("Scripts/Models/30_Hypertension_Model_RobustnessChecks.R")


Run_Script("Scripts/Tables/31_Hypertension_TableA7_CountryInfluence.R")


Run_Script("Scripts/Tables/32_Hypertension_TableA8_CountryRobustness.R")


# *************************************************************
#### Section 7: Validate the completed outputs ####
# *************************************************************


source("Scripts/Setup/33_Hypertension_Setup_CheckReplication.R", encoding = "UTF-8")


message("Replication completed. See Output/Replication for inputs, software and timings.")
