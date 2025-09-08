
plot_annual_rate <- function() {

  dat_all <- readRDS("data/dat_all.rds")
  
  # Remove some countries from the analysis that have low exposure (<1% of sample)
  dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))
  
  # Remove observations with 0 weight
  dat_all <- dat_all %>% filter(v005_denorm!=0)
  
  # Country name
  dat_all$country <- countrycode(dat_all$dhs_cde, "dhs", "country.name")
  
  plot_df_w <- dat_all %>%
    group_by(country, year) %>%
    summarise(
      prop_married = wtd.mean(x = as.numeric(married), weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(married)),
      .groups = "drop"
    )
  
  ggplot(plot_df_w, aes(x = year, y = prop_married)) +
    geom_line() +
    ylim(0, 0.25) +
    xlim(1980, 2015) +
    facet_wrap(~ country) +
    labs(x = "Year", y = "Annual probability of marriage")
  ggsave("figures/annual_rates.jpeg")
}
