
# Plot the annual rate of child marriage in each country

plot_annual_rate <- function(dat_all) {
  
  # Country name
  dat_all$country <- countrycode(dat_all$dhs_cde, "dhs", "country.name")
  
  # Annual rate
  plot_df_w <- dat_all %>%
    group_by(country, year) %>%
    summarise(
      prop_married = Hmisc::wtd.mean(x = as.numeric(married), weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(married)),
      .groups = "drop"
    )
  
  # Make the plot
  ggplot(plot_df_w, aes(x = year, y = prop_married)) +
    geom_line() +
    ylim(0, 0.25) +
    xlim(1980, 2015) +
    facet_wrap(~ country) +
    labs(x = "Year", y = "Annual probability of marriage")
  
  # Save the plot
  ggsave("figures/annual_rates.jpeg")
}
