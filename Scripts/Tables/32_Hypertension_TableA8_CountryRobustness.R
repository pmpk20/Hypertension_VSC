#### Hypertension: Country Robustness ####
# Purpose: Produce Table A8: country-level VSC under the weighting and never-diagnosed robustness checks.
# Author: Anonymised for peer review.
# Prerequisites: None with the supplied outputs. Scripts 14 and 30 produce the inputs in a full replication.
# Run from the project root: source("Scripts/Tables/32_Hypertension_TableA8_CountryRobustness.R", encoding = "UTF-8")
# Inputs:
#   Output/Models/Hypertension_Model_PooledAndCountryModels.rds (sample counts).
#   Output/Tables/Hypertension_Table_RobustnessChecks.txt.
# Outputs:
#   Output/Tables/Hypertension_TableA8_CountryRobustness.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


Report_Tables <- file.path("Output", "Tables")


Report_Countries <- c("Austria", "Belgium", "Canada", "Finland", "Greece",
                      "Japan", "Portugal", "Slovenia", "United Kingdom", "United States")


Data <- readRDS(file.path("Output", "Models", "Hypertension_Model_PooledAndCountryModels.rds"))$pooled_model$data


Robustness_Lines <- readLines(
  file.path(Report_Tables, "Hypertension_Table_RobustnessChecks.txt"),
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


## Read each country-name line and its values within section (c). This handles
## varying print widths and does not depend on the number of coefficient rows.
Read_Country_Means <- function(Lines, Countries) {
  Section_Start <- grep("^\\(c\\)", trimws(Lines))
  Section_End <- grep("^\\(d\\)", trimws(Lines))
  stopifnot(length(Section_Start) == 1L, length(Section_End) == 1L,
            Section_End > Section_Start)
  Block <- trimws(Lines[seq.int(Section_Start + 1L, Section_End - 1L)])
  Block <- Block[nzchar(Block)]
  Result <- setNames(rep(NA_real_, length(Countries)), Countries)
  for (i in seq_len(length(Block) - 1L)) {
    Name_Pattern <- paste(Countries, collapse = "|")
    Names <- regmatches(Block[i], gregexpr(Name_Pattern, Block[i]))[[1]]
    if (!length(Names) || nzchar(trimws(gsub(Name_Pattern, "", Block[i])))) next
    Values <- strsplit(Block[i + 1L], "\\s+")[[1]]
    stopifnot(length(Names) == length(Values), !anyDuplicated(Names),
              all(grepl("^[0-9]+$", Values)), all(is.na(Result[Names])))
    Result[Names] <- as.numeric(Values)
  }
  stopifnot("Never-diagnosed estimates must cover each country once" = !anyNA(Result))
  Result
}


Undiagnosed_Means <- Read_Country_Means(Robustness_Lines, Report_Countries)
Undiagnosed_Names <- names(Undiagnosed_Means)
Undiagnosed_Values <- unname(Undiagnosed_Means)


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


write.table(TableA8, file.path(Report_Tables, "Hypertension_TableA8_CountryRobustness.txt"),
            sep = "#", row.names = FALSE, quote = FALSE)


cat(paste(
  "\nNote: mean VSC in USD 2023 PPP. Specification 1 values match Table 1.",
  "Weighted: country fixed effects estimated by weighted pseudo-maximum likelihood.",
  "Never diagnosed: Specification 1 re-estimated on respondents never told by a doctor",
  "that they had high blood pressure. Percentage changes are relative to Specification 1.\n"),
  file = file.path(Report_Tables, "Hypertension_TableA8_CountryRobustness.txt"), append = TRUE)


cat("\nAppendix Table A8 (country robustness) written.\n")
