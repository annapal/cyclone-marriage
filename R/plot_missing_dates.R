
# Plot the proportion of individuals with missing birth and marriage dates

plot_missing_dates <- function(dat_all) {
  
  # Take first observation for each person
  dat_all <- dat_all %>%
    group_by(caseid) %>%
    slice(1) %>%
    ungroup()
  
  # Get missing birth data
  birth <- dat_all %>%
    count(dhs_cde, v014, name = "n") %>%
    transmute(
      dhs_cde,
      category = fct_recode(v014,
                            "Month & Year Specified"                   = "month and year",
                            "Month & Age Specified, Year Imputed"      = "month and age -y imp",
                            "Year & Age Specified, Month Imputed"      = "year and age - m imp",
                            "Age Specified, Year & Month Imputed"      = "age - y, m imp",
                            "Year & Age Specified, Year Ignored"       = "y & age - y ignored",
                            "Year Specified, Age & Month Imputed"      = "year - a, m imp",
                            "None Specified – All Imputed"             = "none - all imp"
      ),
      n,
      date_type = "Birth dates"
    )
  
  # Get missing marriage data
  marriage <- dat_all %>%
    filter(!is.na(v510)) %>%
    count(dhs_cde, v510, name = "n") %>%
    transmute(
      dhs_cde,
      category = fct_recode(v510,
                            "Month & Year Specified"                   = "month and year",
                            "Month & Age Specified, Year Imputed"      = "month and age -y imp",
                            "Year & Age Specified, Month Imputed"      = "year and age - m imp",
                            "Age Specified, Year & Month Imputed"      = "age - y, m imp",
                            "Year & Age Specified, Year Ignored"       = "y & age - y ignored",
                            "Year Specified, Age & Month Imputed"      = "year - a, m imp",
                            "Month Specified, Age & Year Imputed"      = "month - a, y imp",
                            "None Specified – All Imputed"             = "none - all imp"
      ),
      n,
      date_type = "Marriage dates"
    )
  
  # Combine data
  plot_data_bm <- bind_rows(birth, marriage) %>%
    group_by(dhs_cde, date_type) %>%
    mutate(
      prop = n / sum(n),
      Country = countrycode(dhs_cde, "dhs", "country.name")
    ) %>%
    ungroup()
  
  # Plot the proportions missing
  plot <- ggplot(plot_data_bm, aes(y = fct_rev(Country), x = prop, fill = category)) +
    geom_col(position = "fill") +
    labs(
      x = "Proportion of observations",
      y = "Country",
      fill = " "
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8)
    ) +
    facet_wrap(~date_type)
  
  # Save the figure
  ggsave("figures/missing.jpeg", plot)
}
