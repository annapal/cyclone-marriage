
# Plot the percentage of py exposed in each country each year

plot_cyclones_temporal <- function(dat_all){
  
  # Country name
  dat_all$country <- countrycode(dat_all$dhs_cde, "dhs", "country.name")

  # Calculate proportions
  plot_df_w <- dat_all %>%
    group_by(country, year) %>%
    summarise(
      prop_exp34 = Hmisc::wtd.mean(exp34 == 1, weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(exp34)),
      .groups = "drop"
    )
  
  # Make the plot
  ggplot(plot_df_w, aes(x = year, y = fct_rev(country), fill = prop_exp34)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "navy",
                        labels = scales::percent_format(accuracy = 1),
                        name = "Person-years exposed") +
    labs(x = "Year", y = "Country")
  ggsave("figures/country_exposed.jpeg")
}
