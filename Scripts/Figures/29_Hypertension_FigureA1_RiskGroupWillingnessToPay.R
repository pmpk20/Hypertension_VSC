#### Hypertension: Risk Group Willingness To Pay ####
# Purpose: Produce Figure A1: willingness-to-pay distributions for six risk-perception groups.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13; then script 16.
# Run from the project root: source("Scripts/Figures/29_Hypertension_FigureA1_RiskGroupWillingnessToPay.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Figures/Hypertension_FigureA1_RiskGroupWillingnessToPay.png.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


library(ggplot2)


library(car)


ModelsPath <- file.path(Report_Models, "Hypertension_Model_WillingnessToPayDeterminants.rds")


if (!file.exists(ModelsPath)) {
  stop("Output/Models/Hypertension_Model_WillingnessToPayDeterminants.rds not found -- run ",
       "Scripts/Models/16_Hypertension_Model_WillingnessToPayDeterminants.R first.", call. = FALSE)
}


Models <- readRDS(ModelsPath)


Model  <- Models[["Health controls"]]


Data <- load_analysis_data()


stopifnot(nrow(Model$covariates) == nrow(Data))


WithWelfare <- attach_welfare(Model, Data)


stopifnot(all(is.finite(WithWelfare$WTP_mean)))


# *************************************************************
#### Section 1: Build the 6 group variables (mapped names, Health_Lifestyle recoded) ####
# *************************************************************


HealthLifestyleBin <- dplyr::case_when(
  WithWelfare$Health_Lifestyle %in% c(3, 4) ~ 1,
  WithWelfare$Health_Lifestyle %in% c(0, 1, 2) ~ 0,
  TRUE ~ NA_real_
)


GROUP_SPECS <- list(
  list(Label = "HBP_HigherRisk",   Note = "perceived higher risk of developing HBP",
       Group = WithWelfare$HBP_RelativeRiskHigh),
  list(Label = "HBP_Genetic",      Note = "believes HBP is a genetic condition in their family",
       Group = WithWelfare$HBP_Genetic),
  list(Label = "HBP_KnowPeople",   Note = "knows friends/family with HBP",
       Group = WithWelfare$HBP_Family),
  list(Label = "HBP_Diagnosed",    Note = "prior HBP diagnosis",
       Group = WithWelfare$Health_HadHBP),
  list(Label = "HBP_Healthier",    Note = "considered adopting healthier lifestyle instead of paying",
       Group = WithWelfare$HBP_Healthier),
  list(Label = "Health_Lifestyle", Note = "currently reports healthy lifestyle (Yes/In general yes=1 vs No/In general no/PNTS=0)",
       Group = HealthLifestyleBin)
)


for (g in GROUP_SPECS) {
  bad <- !(g$Group %in% c(0, 1)) & !is.na(g$Group)
  if (any(bad)) stop(sprintf("%s must be coded 0/1 (NA allowed)", g$Label), call. = FALSE)
}


# *************************************************************
#### Section 2: Setup plotting ####
# *************************************************************


## Stack the two groups within each health-perception panel. Use histogram
## intervals, as in Figure 3, and question wording for the panel labels.
library(ggdist)


FACET_LABELS <- c(
  HBP_HigherRisk   = "Perceives higher risk of developing HBP",
  HBP_Genetic      = "Genetic HBP condition in family",
  HBP_KnowPeople   = "Knows friends/family with HBP",
  HBP_Diagnosed    = "Prior HBP diagnosis",
  HBP_Healthier    = "Considered healthier lifestyle instead of paying",
  Health_Lifestyle = "Currently reports healthy lifestyle"
)


PlotData <- dplyr::bind_rows(lapply(GROUP_SPECS, function(g) {
  ok <- !is.na(g$Group)
  data.frame(WTP = WithWelfare$WTP_mean[ok], Group = factor(g$Group[ok]),
             Variable = g$Label, stringsAsFactors = FALSE)
}))


PlotData$Variable <- factor(PlotData$Variable, levels = vapply(GROUP_SPECS, `[[`, character(1), "Label"))


X_MAX <- unname(quantile(WithWelfare$WTP_mean, 0.99))


SlabHeight <- 0.75


## Consistent text sizing/family across this repo's figures (serif, black);
## size 14.
TextSetup <- element_text(size = 14, colour = "black", family = "serif")


## 0/1 in the data == did the respondent answer No/Yes to that panel's item.
GROUP_LABELS <- c("0" = "No", "1" = "Yes")


# *************************************************************
#### Section 3: Build plot ####
# *************************************************************

p_dens <- ggplot(PlotData, aes(x = WTP, y = Group, fill = Group)) +
  ggdist::stat_histinterval(
    alpha = 0.7,
    linewidth = 0.5,
    colour = "black",
    point_interval = "mean_qi",
    outline_bars = TRUE,
    slab_colour = "black",
    slab_linewidth = 0.35,
    height = SlabHeight,
    normalize = "groups",
    ## breaks must cover the FULL data range (ggdist::density_histogram()
    ## errors otherwise), not just the X_MAX display window below --
    ## coord_cartesian() below does the visual cropping to the 99th
    ## percentile, same division of labour as Figure 3.
    breaks = seq(0, max(PlotData$WTP) * 1.001, length.out = 41)
  ) +
  facet_wrap(~ Variable, ncol = 2, nrow = 3, labeller = as_labeller(FACET_LABELS)) +
  coord_cartesian(xlim = c(0, X_MAX)) +
  scale_fill_manual(values = c("0" = "grey60", "1" = "steelblue"),
                    labels = GROUP_LABELS, name = "Response") +
  scale_y_discrete(labels = GROUP_LABELS) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(x = "Individual adjusted-truncated mean WTP (USD2023 PPP)", y = "Response") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    legend.position  = "bottom",
    text             = TextSetup,
    axis.text        = TextSetup,
    axis.title       = TextSetup,
    strip.text       = TextSetup,
    legend.text      = TextSetup,
    legend.title     = TextSetup
  )


# *************************************************************
#### Section 4: Export ####
# *************************************************************


## Save the paper figure as PNG.
ggsave(file.path(Report_Figures, "Hypertension_FigureA1_RiskGroupWillingnessToPay.png"), p_dens,
       width = 10, height = 6.5, units = "in", dpi = 300, bg = "white")


cat("\nWTP-by-risk-group histogram figure written to Output/Figures/Hypertension_FigureA1_RiskGroupWillingnessToPay.png\n")
