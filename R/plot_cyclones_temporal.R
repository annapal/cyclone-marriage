
plot_cyclones_temporal <- function(){

  dat_all <- readRDS("data/dat_all.rds")
  dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))
  dat_all <- dat_all %>% filter(v005_denorm!=0)
  
  plot_df_w <- dat_all %>%
    group_by(country, year) %>%
    summarise(
      prop_exp34 = wtd.mean(exp34 == 1, weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(exp34)),
      .groups = "drop"
    )
  
  ggplot(plot_df, aes(x = year, y = fct_rev(country), fill = prop_exp34)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "navy",
                        labels = scales::percent_format(accuracy = 1),
                        name = "Person-years exposed") +
    labs(x = "Year", y = "Country")
  ggsave("figures/country_exposed.jpeg")
}
