
make_table <- function() {
  
  dat_all <- readRDS("data/dat_all.rds")
  
  # Remove some countries from the analysis that have low exposure (<1% of sample)
  dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))
  
  # Remove observations with 0 weight
  dat_all <- dat_all %>% filter(v005_denorm!=0)
  
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
  
  write_xlsx(summary_tbl, "figures/summary_table.xlsx")
  
}