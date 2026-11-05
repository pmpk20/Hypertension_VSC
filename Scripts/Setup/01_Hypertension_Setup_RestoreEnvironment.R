#### Hypertension: Restore Environment ####
# Purpose: install the lockfile's package versions into a private project library.
# Author: Anonymised for peer review.
# Prerequisites: R 4.5.0; internet access; Rtools45 on Windows.
# Run from the project root: source("Scripts/Setup/01_Hypertension_Setup_RestoreEnvironment.R", encoding = "UTF-8")
# Inputs:
#   renv.lock.
# Outputs:
#   renv/library/ (the recorded package versions).
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Bootstrap renv and restore the project library ####
# *************************************************************


stopifnot(file.exists("HypertensionPaper.Rproj"), file.exists("renv.lock"))


options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 300)


if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")


renv::restore(project = getwd(), lockfile = "renv.lock", prompt = FALSE)


# *************************************************************
#### Section 1: Load the restored environment ####
# *************************************************************


renv::load(project = getwd())
