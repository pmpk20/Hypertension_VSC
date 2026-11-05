#### Hypertension: Pairwise Country Comparisons ####
# Purpose: Produce Table A6: compare mean willingness to pay for all 45 country pairs using joint simulation draws.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then scripts 14 and 18.
# Run from the project root: source("Scripts/Tables/28_Hypertension_TableA6_PairwiseCountryComparisons.R", encoding = "UTF-8")
# Inputs:
#   Output/Models/Hypertension_Model_KrinskyRobbDraws.rds.
# Outputs:
#   Output/Tables/Hypertension_TableA6_PairwiseCountryComparisons.txt.
#   Output/Tables/Hypertension_TableA6_PairwiseCountryComparisonsLongFormat.txt.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


KRPath <- file.path(Report_Models, "Hypertension_Model_KrinskyRobbDraws.rds")


if (!file.exists(KRPath)) {
  stop("Output/Models/Hypertension_Model_KrinskyRobbDraws.rds not found -- run ",
       "Scripts/Tables/18_Hypertension_Table_KrinskyRobbIntervals.R first.", call. = FALSE)
}


KR <- readRDS(KRPath)


stopifnot(
  "Cached KR draws must cover WTP_mean for every report country" =
    all(Report_Countries %in% dimnames(KR$draw_stats)[[3]]),
  "Cached KR draws must include the WTP_mean statistic" =
    "WTP_mean" %in% dimnames(KR$draw_stats)[[2]])


cat(sprintf("\nReusing cached Krinsky-Robb draws: R = %d, PPP basis = AIC2023, %d invalid draws already dropped.\n",
            KR$draws, KR$n_invalid))


## Three-letter country codes used in the table.
COUNTRY_CODE <- c(
  "Austria" = "AUT", "Belgium" = "BEL", "Canada" = "CAN", "Finland" = "FIN",
  "Greece" = "GRC", "Japan" = "JPN", "Portugal" = "PRT", "Slovenia" = "SLN",
  "United Kingdom" = "UK", "United States" = "US"
)


# *************************************************************
#### Section 1: One shared valid-row mask across all ten countries ####
# *************************************************************


WTPmat <- KR$draw_stats[, "WTP_mean", Report_Countries]


colnames(WTPmat) <- unname(COUNTRY_CODE[Report_Countries])


valid <- stats::complete.cases(WTPmat)


WTPmat <- WTPmat[valid, , drop = FALSE]


R <- nrow(WTPmat)


cat(sprintf("Draws usable for every one of the ten countries simultaneously: %d of %d.\n",
            R, KR$draws))


stopifnot("Need enough valid draws for a stable percentile-style test" = R >= 1000)


# *************************************************************
#### Section 2: Poe-Giraud-Loomis pairwise test, all 45 pairs ####
# *************************************************************


CODES <- unname(COUNTRY_CODE[Report_Countries])


pairs <- utils::combn(CODES, 2)


pgl_test <- function(a, b) {
  delta  <- WTPmat[, a] - WTPmat[, b]
  p_one  <- mean(delta <= 0)
  p_two  <- min(2 * min(p_one, 1 - p_one), 1)
  data.frame(A = a, B = b, p_one_sided = p_one, p_two_sided = p_two,
             stringsAsFactors = FALSE)
}


Pairwise <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(i) pgl_test(pairs[1, i], pairs[2, i])))


n_sig10 <- sum(Pairwise$p_two_sided < 0.10)


n_sig5  <- sum(Pairwise$p_two_sided < 0.05)


n_sig1  <- sum(Pairwise$p_two_sided < 0.01)


cat(sprintf(
  "\nOf the %d country pairs, %d (%.1f%%) showed statistically significant differences in mean WTP at the 5%% level (%d, %.1f%%, at the 1%% level; %d, %.1f%%, at the 10%% level).\n",
  nrow(Pairwise), n_sig5, 100 * n_sig5 / nrow(Pairwise), n_sig1, 100 * n_sig1 / nrow(Pairwise),
  n_sig10, 100 * n_sig10 / nrow(Pairwise)))


# *************************************************************
#### Section 3: Assemble the 10x10 lower-triangular matrix ####
# *************************************************************


## Significance convention for this table (different from
## 04_Hypertension_Setup_SharedCalculations.R's TableOutputter_DBCHOICE, which uses a
## four-tier ***/**/*/+ scheme at 0.001/0.01/0.05/0.1 -- this table is
## three-tier): * p<0.10, ** p<0.05, *** p<0.01.
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))


cell <- setNames(sprintf("%.2f%s", Pairwise$p_two_sided, stars(Pairwise$p_two_sided)),
                 paste(Pairwise$A, Pairwise$B))


Matrix <- data.frame(Country = CODES, stringsAsFactors = FALSE)


for (j in seq_along(CODES)) {
  colname <- CODES[j]
  Matrix[[colname]] <- vapply(seq_along(CODES), function(i) {
    if (i == j) return("-")
    if (i < j) return("")   # upper triangle blank -- lower-triangular only, as requested
    key <- paste(CODES[j], CODES[i])   # pairs were built with the earlier code first
    if (!key %in% names(cell)) key <- paste(CODES[i], CODES[j])
    unname(cell[key])
  }, character(1))
}


cat("\n#### Appendix Table A6: pairwise country WTP comparisons (Poe-Giraud-Loomis) ####\n")


print(Matrix, row.names = FALSE)


# *************************************************************
#### Section 4: Export ####
# *************************************************************


OUT <- file.path(Report_Tables, "Hypertension_TableA6_PairwiseCountryComparisons.txt")


## quote = FALSE so the blank upper-triangle cells write out as genuinely
## empty fields, not "" (an empty-but-quoted string reads oddly when the
## file is opened in a spreadsheet/text editor).
Matrix %>% fwrite(sep = "#", OUT, quote = FALSE)


Pairwise %>%
  dplyr::mutate(Significant_10pct = p_two_sided < 0.10,
                Significant_5pct  = p_two_sided < 0.05,
                Significant_1pct  = p_two_sided < 0.01) %>%
  fwrite(sep = "#", file.path(Report_Tables, "Hypertension_TableA6_PairwiseCountryComparisonsLongFormat.txt"), quote = FALSE)


Note <- sprintf(paste(
  "\nNote: P-values for equal mean WTP between country pairs, computed",
  "from paired differences within each joint parameter draw",
  "(doi: 10.2307/3700938). The test uses %d valid Krinsky-Robb draws",
  "(of %d simulated) from the joint parameter distribution of",
  "Specification 1 (baseline), reused from",
  "Output/Models/Hypertension_Model_KrinskyRobbDraws.rds rather than re-simulated.",
  "Overlapping confidence intervals do not constitute a valid test of",
  "equality (Schenker and Gentleman 2001); this test provides the",
  "appropriate formal comparison. * p < 0.10, ** p < 0.05, *** p < 0.01,",
  "two-sided. Lower-triangular: row/column order matches REPORT_COUNTRIES (Austria",
  "through United States). Of the %d country pairs, %d (%.1f%%) are",
  "significant at 10%%, %d (%.1f%%) at 5%%, %d (%.1f%%) at 1%%.\n"
), R, KR$draws, nrow(Pairwise), n_sig10, 100 * n_sig10 / nrow(Pairwise),
   n_sig5, 100 * n_sig5 / nrow(Pairwise), n_sig1, 100 * n_sig1 / nrow(Pairwise))


cat(Note, file = OUT, append = TRUE)


cat(Note)


cat("\nAppendix Table A6 (pairwise Poe-Giraud-Loomis test) written.\n")
