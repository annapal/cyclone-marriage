
# Get descriptive statistics from the data

get_descriptives <- function(dat_all) {

  # Create an output list to store summary tables
  desc_out <- list()
  
  # Person-years of data
  desc_out$person_years <- data.frame(
    metric = "Person-years of data",
    value = nrow(dat_all)
  )
  
  # Number of surveys
  desc_out$num_surveys <- data.frame(
    metric = "Number of surveys",
    value = length(unique(dat_all$v000))
  )
  
  # Dataset of unique individuals (first record per person)
  dat_first <- dat_all %>%
    group_by(caseid) %>%
    slice(1) %>%
    ungroup()
  
  # Proportion of sample married before age 18
  desc_out$prop_married <- dat_first %>%
    group_by(dhs_cde) %>%
    summarise(
      prop_married = Hmisc::wtd.mean(v511 < 18, weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(v511)),
      .groups = "drop"
    )
  
  # Average annual marriage rate
  desc_out$annual_rate <- dat_all %>%
    group_by(dhs_cde) %>%
    summarise(
      annual_rate = Hmisc::wtd.mean(as.numeric(married), weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(married)),
      .groups = "drop"
    )
  
  # Overall proportion of person-years exposed
  desc_out$overall_exp <- data.frame(
    metric = "Proportion of person-years exposed",
    value = Hmisc::wtd.mean(dat_all$exp34 == 1, weights = dat_all$v005_denorm, na.rm = TRUE)
  )
  
  # Proportion of person-years exposed by country
  desc_out$exp_by_country <- dat_all %>%
    group_by(dhs_cde) %>%
    summarise(
      prop_exp = Hmisc::wtd.mean(exp34 == 1, weights = v005_denorm, na.rm = TRUE),
      n = sum(!is.na(exp34)),
      .groups = "drop"
    )
  
  # Ensure output directory exists
  dir.create("results", recursive = TRUE, showWarnings = FALSE)
  
  # Save all summaries to an Excel workbook
  writexl::write_xlsx(
    desc_out,
    path = "results/descriptives.xlsx"
  )
}
