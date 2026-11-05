#### Hypertension: Bid Acceptance ####
# Purpose: Produce Figure A2: nonparametric bid-acceptance curves by country.
# Author: Anonymised for peer review.
# Prerequisites: Run preparation steps 01–13.
# Run from the project root: source("Scripts/Figures/24_Hypertension_FigureA2_BidAcceptance.R", encoding = "UTF-8")
# Inputs:
#   Data/Data_AllCountries_SubsampledForAnalysis_Rebuilt.csv.
#   Data/Other/OECD_PPP_ActualIndividualConsumption.csv.
# Outputs:
#   Output/Figures/Hypertension_FigureA2_BidAcceptance.pdf.
#   Output/Figures/Hypertension_FigureA2_BidAcceptance.png.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


source(here::here("Scripts", "Setup", "04_Hypertension_Setup_SharedCalculations.R"))


library(ggplot2)


library(survival)


Data <- load_analysis_data()


stopifnot(
  "Question1/Question2 must be 0/1" =
    all(Data$Question1 %in% c(0, 1), Data$Question2 %in% c(0, 1)))


# *************************************************************
#### Section 1: Interval-censored WTP bracket per respondent (as Appendix A3) ####
# *************************************************************


PPP_i <- unname(ppp_rescale()[Data$Country])


b1 <- exp(Data$LogBid1) * PPP_i


b2 <- exp(Data$LogBid2) * PPP_i


accept1 <- ifelse(Data$Question1 == 1, b1, -Inf)


accept2 <- ifelse(Data$Question2 == 1, b2, -Inf)


reject1 <- ifelse(Data$Question1 == 0, b1,  Inf)


reject2 <- ifelse(Data$Question2 == 0, b2,  Inf)


L <- pmax(accept1, accept2)


U <- pmin(reject1, reject2)


L[!is.finite(L)] <- 0          # NN: nothing accepted -> lower bound 0


U_finite <- is.finite(U)       # YY: nothing rejected -> right-censored


stopifnot("Accepted bid must not exceed rejected bid (DBDC monotonicity)" =
            all(L[U_finite] <= U[U_finite] + 1e-9))


U_surv <- U


U_surv[!U_finite] <- NA_real_   # survival::Surv(type="interval2") convention for right-censored


# *************************************************************
#### Section 2: Turnbull NPMLE survival curve, per country + pooled ####
# *************************************************************


survival_curve <- function(mask, label) {
  fit <- survival::survfit(survival::Surv(L[mask], U_surv[mask], type = "interval2") ~ 1)
  data.frame(
    Country = label,
    Time    = c(0, fit$time),
    Surv    = c(1, fit$surv)
  )
}


CountryCurves <- dplyr::bind_rows(lapply(Report_Countries, function(ctry) {
  survival_curve(Data$Country == ctry, ctry)
}))


PooledCurve <- survival_curve(rep(TRUE, nrow(Data)), "Pooled")


cat("\n#### Turnbull survival curve step points, by country (S(t) = P(WTP > t)) ####\n")


for (ctry in Report_Countries) {
  cat(sprintf("\n%s:\n", ctry))
  Sub <- CountryCurves %>% dplyr::filter(Country == ctry, Time > 0) %>%
    dplyr::transmute(`Bid (USD2023 PPP)` = sprintf("$%.2f", Time),
                      `S(t) = P(WTP > t)` = sprintf("%.1f%%", 100 * Surv))
  print(as.data.frame(Sub), row.names = FALSE)
}


cat("\n#### Pooled Turnbull survival curve step points ####\n")


print(as.data.frame(PooledCurve %>% dplyr::filter(Time > 0) %>%
  dplyr::transmute(`Bid (USD2023 PPP)` = sprintf("$%.2f", Time),
                    `S(t) = P(WTP > t)` = sprintf("%.1f%%", 100 * Surv))), row.names = FALSE)


## Median WTP (where the curve crosses 50%) per country, a useful single
## number to sanity-check the curve against.
MedianWTP <- CountryCurves %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(MedianWTP = Time[which(Surv <= 0.5)[1]], .groups = "drop")


cat("\n#### Turnbull median WTP (first bid where S(t) <= 50%), by country ####\n")


print(as.data.frame(MedianWTP %>% dplyr::mutate(MedianWTP = sprintf("$%.2f", MedianWTP))), row.names = FALSE)


# *************************************************************
#### Section 3: Plot -- 5x2 facets, one step function per country ####
# *************************************************************


## Show one country per panel. The pooled curve is summarised in Section 2.
PlotData <- CountryCurves


PlotData$Country <- factor(PlotData$Country, levels = Report_Countries)


## Cap the display range at the pooled curve's tau (largest finite bracket
## endpoint) -- beyond that, remaining probability mass is right-censored
## (YY respondents) and the NPMLE has no further step to draw.
X_MAX <- max(L[U_finite])


## Consistent text sizing/family across this repo's figures (serif, black);
## size 14.
TextSetup <- element_text(size = 14, colour = "black", family = "serif")


p_bid <- ggplot(PlotData, aes(x = Time, y = Surv)) +
  geom_step(colour = "steelblue", linewidth = 0.7, direction = "hv") +
  facet_wrap(~ Country, nrow = 5, ncol = 2) +
  coord_cartesian(xlim = c(0, X_MAX)) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = "DBDC bid (USD2023 PPP)",
    y = "S(t) = P(WTP > t) -- Turnbull non-parametric survival estimate"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    text             = TextSetup,
    axis.text        = TextSetup,
    axis.title       = TextSetup,
    strip.text       = TextSetup
  )


# *************************************************************
#### Section 4: Export ####
# *************************************************************


## 5x2 panels -- taller canvas so the panels aren't squashed
##.
ggsave(file.path(Report_Figures, "Hypertension_FigureA2_BidAcceptance.png"), p_bid,
       width = 9, height = 13, units = "in", dpi = 300, bg = "white")


ggsave(file.path(Report_Figures, "Hypertension_FigureA2_BidAcceptance.pdf"), p_bid,
       width = 9, height = 13, units = "in", device = cairo_pdf)


cat("\nBid-acceptance survival curve (by country) written to Output/Figures/Hypertension_FigureA2_BidAcceptance.png (+ .pdf)\n")
