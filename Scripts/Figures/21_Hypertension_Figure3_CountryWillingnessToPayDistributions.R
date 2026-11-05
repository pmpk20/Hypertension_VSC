#### Hypertension: Country Willingness To Pay Distributions ####
# Purpose: Produce Figure 3: country distributions of individual willingness to pay under the specification with controls.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 16.
# Run from the project root: source("Scripts/Figures/21_Hypertension_Figure3_CountryWillingnessToPayDistributions.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Figures/Hypertension_Figure3_CountryWillingnessToPayDistributions.png.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


options(scipen = 999)


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


library(ggplot2)


library(ggdist)


ModelsPath <- file.path(Report_Models, "Hypertension_Model_WillingnessToPayDeterminants.rds")


if (!file.exists(ModelsPath)) {
  stop("Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds not found -- run ",
       "Scripts/Models/16_Hypertension_Model_WillingnessToPayDeterminants.R first.", call. = FALSE)
}


Models <- readRDS(ModelsPath)


Model  <- Models[["With controls"]]


Data <- load_analysis_data()


stopifnot(nrow(Model$covariates) == nrow(Data))


# *************************************************************
#### Section 1: Closed-form individual WTP (Specification 2, With controls) ####
# *************************************************************

weibull_adjtrunc <- function(beta, X, max_bid) {
  k    <- length(beta)
  cpar <- -beta[k]
  if (!is.finite(cpar) || cpar <= 0) return(NULL)
  XB  <- as.vector(X %*% beta[-k])
  inv <- 1 / cpar
  s <- exp(XB * inv)
  t <- exp(cpar * max_bid - XB)
  adjtrunc <- s * gamma(inv + 1) * pgamma(t, shape = inv) / (1 - exp(-t))
  if (!all(is.finite(adjtrunc))) return(NULL)
  adjtrunc
}


Coefficient_Estimates <- Model$coefficients


Maximum_Bid  <- max(Model$bid)


Xmat    <- Model$covariates


PPP_i   <- unname(ppp_rescale()[Data$Country])


WTP_i <- weibull_adjtrunc(Coefficient_Estimates, Xmat, Maximum_Bid) * PPP_i


stopifnot(!is.null(WTP_i), all(is.finite(WTP_i)))


Data$WTP_i <- WTP_i


# *************************************************************
#### Section 2: Summary stats ####
# *************************************************************

CountrySummary <- Data %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    N      = dplyr::n(),
    Mean   = mean(WTP_i),
    Median = median(WTP_i),
    SD     = sd(WTP_i),
    IQR    = IQR(WTP_i),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Country)


cat("\n#### Per-country individual WTP summary (Specification 2, With controls) ####\n")


print(as.data.frame(CountrySummary), row.names = FALSE, digits = 4)


# *************************************************************
#### Section 3: Plot data & styling matching Hypertension_Figure_HypertensionSubgroupWillingnessToPay ####
# *************************************************************

## Combine country-specific data with pooled sample
DensityData <- dplyr::bind_rows(
  data.frame(WTP = Data$WTP_i, Country = Data$Country, stringsAsFactors = FALSE),
  data.frame(WTP = Data$WTP_i, Country = "Pooled", stringsAsFactors = FALSE)
)


CountryLevels <- c("Pooled", rev(sort(unique(Data$Country))))


DensityData$Country <- factor(DensityData$Country, levels = CountryLevels)


X_MAX <- 2500


## Consistent text sizing/family across this repo's figures (serif, black);
## size 14.
TextSetup <- element_text(size = 14, colour = "black", family = "serif")


SlabHeight <- 0.65


p_dens <- ggplot(DensityData, aes(x = WTP, y = Country)) +
  ggdist::stat_histinterval(
    fill = "steelblue",
    alpha = 0.55,
    linewidth = 0.5,
    colour = "black",
    point_interval = "mean_qi",
    outline_bars = TRUE,
    slab_colour = "black",
    slab_linewidth = 0.35,
    height = SlabHeight,
    normalize = "groups",
    breaks = seq(0, max(DensityData$WTP) * 1.001, length.out = 61)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.08, 0.6))) +
  scale_x_continuous(
    breaks = seq(0, X_MAX, by = 500),
    labels = scales::label_number(big.mark = ","),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(xlim = c(0, X_MAX), clip = "off") +
  labs(
    x = "Individual adjusted-truncated mean WTP (USD2023 PPP)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid    = element_blank(),
    axis.line.x   = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.x  = element_line(colour = "black", linewidth = 0.3),
    text          = TextSetup,
    axis.text     = TextSetup,
    axis.text.y   = element_text(size = 14, colour = "black", family = "serif", face = "bold"),
    axis.title    = TextSetup,
    plot.margin   = margin(t = 20, r = 10, b = 5, l = 5)
  )


# *************************************************************
#### Section 4: Export PNG ####
# *************************************************************

ggsave(file.path(Report_Figures, "Hypertension_Figure3_CountryWillingnessToPayDistributions.png"), p_dens,
       width = 9, height = 10.5, units = "in", dpi = 300, bg = "white")


# ggsave(file.path(REPORT_FIGURES, "Hypertension_Figure3_CountryWillingnessToPayDistributions.pdf"), p_dens,
#        width = 9, height = 10.5, units = "in", device = cairo_pdf)
# ggsave(file.path(REPORT_FIGURES, "Hypertension_Figure3_CountryWillingnessToPayDistributions.tiff"), p_dens,
#        width = 9, height = 10.5, units = "in", dpi = 300, compression = "lzw", bg = "white")

cat("\nManuscript Figure 3 (WTP density by country + Pooled) written to Output/Figures/Hypertension_Figure3_CountryWillingnessToPayDistributions.png\n")
