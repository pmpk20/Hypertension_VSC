#### Hypertension: Determinant Estimates ####
# Purpose: format the three cached Weibull specifications as a coefficient table.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 16.
# Run from the project root: source("Scripts/Tables/17_Hypertension_Table_DeterminantEstimates.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Tables/Hypertension_Table_DeterminantEstimates.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Analysis_Data <- load_analysis_data()


Models <- readRDS(file.path(Report_Models, "Hypertension_Model_WillingnessToPayDeterminants.rds"))


ppp_vec <- unname(ppp_rescale()[Analysis_Data$Country])


Determinants_Table <- stitch_models(
  lapply(Models, TableOutputter_DBCHOICE, ppp_factor = ppp_vec)
) %>%
  dplyr::filter(!grepl("_Weighted$", Variable))


print(Determinants_Table)


# *************************************************************
#### Section 1: Export ####
# *************************************************************


Determinants_Table %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_Table_DeterminantEstimates.txt"))
