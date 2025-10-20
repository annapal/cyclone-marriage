
# Make table of countries and surveys for SI

make_table <- function(dat_all) {
  
  # Make summary table
  summary_tbl <- dat_all %>%
    mutate(`Country` = countrycode(dhs_cde, "dhs", "country.name")) %>%
    group_by(Country) %>%
    summarise(
      `Surveys` = paste(sort(unique(v000)), collapse = ", "),
      `No. Person-Years` = n(),
      `No. Marriages`    = sum(married, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_xlsx(summary_tbl, "results/summary_table.xlsx")
  
}