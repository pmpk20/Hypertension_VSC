#### Hypertension: Check Environment ####
# Purpose: check every recorded package version and load the analysis dependencies.
# Author: Anonymised for peer review.
# Prerequisites: Run script 01 to restore packages.
# Run from the project root: source("Scripts/Setup/02_Hypertension_Setup_CheckEnvironment.R", encoding = "UTF-8")
# Inputs:
#   renv.lock; Replication_Packages.txt; installed project library.
# Outputs:
#   Output/Replication/Environment_Check.csv.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Load the project library and read the recorded versions ####
# *************************************************************


if (dir.exists("renv/library")) renv::load(project = getwd())


Recorded_Environment <- jsonlite::read_json("renv.lock")


Recorded_Packages <- Recorded_Environment$Packages


if (getRversion() != package_version(Recorded_Environment$R$Version)) {
  stop("Use the recorded R version: ", Recorded_Environment$R$Version, call. = FALSE)
}


# *************************************************************
#### Section 1: Verify versions, including the recommended packages bundled with R ####
# *************************************************************


Package_Status <- data.frame(
  Package = names(Recorded_Packages),
  Recorded_Version = vapply(Recorded_Packages, function(Package) Package$Version, character(1)),
  Installed_Version = vapply(names(Recorded_Packages), function(Package) {
    if (!length(find.package(Package, quiet = TRUE))) return(NA_character_)
    as.character(packageVersion(Package))
  }, character(1))
)


Package_Status$Matches <- vapply(seq_len(nrow(Package_Status)), function(Package_Index) {
  if (is.na(Package_Status$Installed_Version[Package_Index])) return(FALSE)
  package_version(Package_Status$Installed_Version[Package_Index]) ==
    package_version(Package_Status$Recorded_Version[Package_Index])
}, logical(1))


dir.create("Output/Replication", recursive = TRUE, showWarnings = FALSE)


write.csv(Package_Status, "Output/Replication/Environment_Check.csv", row.names = FALSE)


if (!all(Package_Status$Matches)) {
  stop("Package versions differ from renv.lock; see Output/Replication/Environment_Check.csv.", call. = FALSE)
}


# *************************************************************
#### Section 2: Check that each direct analysis dependency loads ####
# *************************************************************


Required_Packages <- readLines("Replication_Packages.txt", warn = FALSE)


stopifnot(all(vapply(Required_Packages, requireNamespace, logical(1), quietly = TRUE)))


message("Environment checks passed: ", nrow(Package_Status), " recorded versions and ",
        length(Required_Packages), " direct packages.")
