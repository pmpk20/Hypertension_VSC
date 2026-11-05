#### Hypertension: Country Robustness ####
# Purpose: Produce Table A8: country-level VSC under the weighting and never-diagnosed robustness checks.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 30.
# Run from the project root: source("Scripts/Tables/32_Hypertension_TableA8_CountryRobustness.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Tables/Hypertension_Table_RobustnessChecks.txt.
# Outputs:
#   Output/Tables/Hypertension_TableA8_CountryRobustness.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


Data <- load_analysis_data()


Robustness_Lines <- readLines(
  here::here("Output", "Tables", "Hypertension_Table_RobustnessChecks.txt"),
  warn = FALSE)


# *************************************************************
#### Section 1: Weighting check (script 30, part a) ####
# *************************************************************


## Rows read: Country, Published (Specification 1), FE_weightedML,
## FE_unweighted, Pct_diff_weightedML. No model is re-estimated here.
Row_Pattern <- "^\\s*(.+?)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(-?[0-9.]+)\\s*$"


Weighting_Matches <- regmatches(Robustness_Lines, regexec(Row_Pattern, Robustness_Lines))


Weighting_Matches <- do.call(rbind, Weighting_Matches[lengths(Weighting_Matches) == 6])


Weighting <- data.frame(
  Country      = Weighting_Matches[, 2],
  VSC_Spec1    = as.numeric(Weighting_Matches[, 3]),
  VSC_Weighted = as.numeric(Weighting_Matches[, 4]),
  stringsAsFactors = FALSE)


Weighting <- Weighting[Weighting$Country %in% c(Report_Countries, "Pooled"), ]


stopifnot("Weighting check must have ten countries and a pooled row" = nrow(Weighting) == 11)


# *************************************************************
#### Section 2: Never-diagnosed check (script 30, part c) ####
# *************************************************************


Undiagnosed_Header <- grep("Spec 1, undiagnosed only", Robustness_Lines)


Undiagnosed_Pooled <- as.numeric(sub(".*mean VSC = (\\d+).*", "\\1", Robustness_Lines[Undiagnosed_Header]))


## The country means are printed as a named vector over four lines
## (names, values, names, values) after the coefficient block.
Country_Block <- Robustness_Lines[(Undiagnosed_Header + 14):(Undiagnosed_Header + 17)]


Undiagnosed_Names <- unlist(lapply(Country_Block[c(1, 3)], function(s) strsplit(trimws(s), "\\s{2,}")[[1]]))


Undiagnosed_Values <- as.numeric(unlist(lapply(Country_Block[c(2, 4)], function(s) strsplit(trimws(s), "\\s+")[[1]])))


Undiagnosed <- data.frame(
  Country            = c(Undiagnosed_Names, "Pooled"),
  VSC_NeverDiagnosed = c(Undiagnosed_Values, Undiagnosed_Pooled),
  stringsAsFactors = FALSE)


stopifnot("Never-diagnosed check must cover the ten countries" =
            setequal(Undiagnosed_Names, Report_Countries))


# *************************************************************
#### Section 3: Assemble and export ####
# *************************************************************


Sample_N <- c(table(factor(Data$Country, levels = Report_Countries)), Pooled = nrow(Data))


Undiagnosed_N <- c(table(factor(Data$Country[Data$Health_HadHBP == 0], levels = Report_Countries)),
                   Pooled = sum(Data$Health_HadHBP == 0))


TableA8 <- merge(Weighting, Undiagnosed, by = "Country", sort = FALSE)


TableA8 <- TableA8[match(c(Report_Countries, "Pooled"), TableA8$Country), ]


TableA8$N_Spec1            <- unname(Sample_N[TableA8$Country])
TableA8$Pct_Weighted       <- round(100 * (TableA8$VSC_Weighted / TableA8$VSC_Spec1 - 1), 1)
TableA8$N_NeverDiagnosed   <- unname(Undiagnosed_N[TableA8$Country])
TableA8$Pct_NeverDiagnosed <- round(100 * (TableA8$VSC_NeverDiagnosed / TableA8$VSC_Spec1 - 1), 1)


TableA8 <- TableA8[, c("Country", "N_Spec1", "VSC_Spec1", "VSC_Weighted", "Pct_Weighted",
                       "N_NeverDiagnosed", "VSC_NeverDiagnosed", "Pct_NeverDiagnosed")]


stopifnot(
  "Analysis sample is 9,496" = TableA8$N_Spec1[TableA8$Country == "Pooled"] == 9496,
  "Never-diagnosed sample is 6,309" = TableA8$N_NeverDiagnosed[TableA8$Country == "Pooled"] == 6309)


print(TableA8, row.names = FALSE)


fwrite(TableA8, sep = "#", file.path(Report_Tables, "Hypertension_TableA8_CountryRobustness.txt"))


cat(paste(
  "\nNote: mean VSC in USD 2023 PPP. Specification 1 values match Table 1.",
  "Weighted: country fixed effects estimated by weighted pseudo-maximum likelihood.",
  "Never diagnosed: Specification 1 re-estimated on respondents never told by a doctor",
  "that they had high blood pressure. Percentage changes are relative to Specification 1.\n"),
  file = file.path(Report_Tables, "Hypertension_TableA8_CountryRobustness.txt"), append = TRUE)


cat("\nAppendix Table A8 (country robustness) written.\n")
