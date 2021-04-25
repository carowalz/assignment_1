library(dplyr)
library(ggplot2)
library(ExPanDaR)
library(stringi)
library(stringr)

load("data/generated/sample.rda")

fig_scatter <- ggplot(
  smp, aes(x = ln_gdp_capita, y = life_expectancy, color = region)
) +
  geom_point(alpha = 0.3) +
  labs(
    color = "World Bank Region",
    x = "Ln(Income per capita in thsd. 2010 US-$)",
    y = "Life expectancy in years"
  ) +
  geom_smooth(method = "auto") +
  theme_minimal()


by_region <- smp %>% group_by(region)
regional_descripives <- by_region %>% summarise(
  gdp_mean = round(mean(gdp_capita), 1),
  le_mean = round(mean(life_expectancy), 1),
  unemp_mean = round(mean(unemployment), 1)
)
colnames(regional_descripives) <- c("Worldbank Region", "Mean GDP", "Mean Life Expectancy", "Mean Unemployment Rate")


regional_correlations <- by_region %>% summarise(
  cor_gdp_le = round(cor(ln_gdp_capita, life_expectancy), 3),
  cor_le_unemp = round(cor(life_expectancy, unemployment), 3),
  cor_gdp_unemp = round(cor(ln_gdp_capita, unemployment), 3)
)
colnames(regional_correlations) <- c("Worldbank Region", "Correlation: ln(GDP), Life Expectancy", 
                                     "Correlation: Life Expectancy, Unemployment", "Correlation: ln(GDP), Unemployment")

tab_desc_stat <- prepare_descriptive_table(
  smp %>% select(-year, -ln_gdp_capita)
)


tab_corr <- prepare_correlation_table(
  smp %>% select(-year, -gdp_capita),
  format = "latex", booktabs = TRUE, linesep = ""
)

smp$dummy_region <- rep(NA, nrow(smp))
for (i in 1:nrow(smp)) {
  regions <- unique(smp$region)
  for (j in 1:length(regions)) {
    if(smp$region[i] == regions[j]){smp$dummy_region[i] <- j - 1}
    if(smp$region[i] == regions[j]){smp$region[i] <- str_replace(regions[j], "&", "")}
  }
}

tab_regression_region <-  prepare_regression_table(
  smp,
  dvs = rep("life_expectancy", 4),
  idvs = list(
    c("ln_gdp_capita", "region"),
    c("ln_gdp_capita", "unemployment", "region", "region:ln_gdp_capita"),
    c("ln_gdp_capita", "unemployment", "region", "region:ln_gdp_capita"),
    c("ln_gdp_capita", "unemployment")
  ),
  feffects = list("", "", "year", c("country", "year")),
  cluster = list("", "",  "year", c("country", "year")),
  format = "latex"
)

tab_regression <-  prepare_regression_table(
  smp,
  dvs = rep("life_expectancy", 4),
  idvs = list(
    c("ln_gdp_capita"),
    c("ln_gdp_capita", "unemployment"),
    c("ln_gdp_capita", "unemployment"),
    c("ln_gdp_capita", "unemployment")
  ),
  feffects = list("", "", "year", c("country", "year")),
  cluster = list("", "",  "year", c("country", "year")),
  format = "latex"
)


# smp_subset <- smp %>% filter(region == "Sub-Saharan Africa")
# tab_regression_subset <-  prepare_regression_table(
#   smp_subset,
#   dvs = rep("life_expectancy", 4),
#   idvs = list(
#     c("ln_gdp_capita"),
#     c("ln_gdp_capita", "unemployment"),
#     c("ln_gdp_capita", "unemployment"),
#     c("ln_gdp_capita", "unemployment")
#   ),
#   feffects = list("", "", "year", c("country", "year")),
#   cluster = list("", "",  "year", c("country", "year")),
#   format = "latex"
# )


save(
  list = c(ls(pattern = "fig_*"), ls(pattern = "tab_*"), ls(pattern = "regional_")),
  file = "output/results.rda"
)

