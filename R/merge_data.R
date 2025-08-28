
merge_data <- function() {

  # Get DHS file names
  m_files <- list.files("data/merged_dhs/", full.names = FALSE)
  
  # Get population data for denormalising survey weights
  pop_data <- read.csv("data/pop_year.csv") %>%
    mutate(dhs = countrycode(ISO3_code, "iso3c", "dhs", warn = FALSE))
  
  # Get cyclone raster
  r_stack <- rast("data/cy_stack.tif")
  
  # Countries
  countries <- unique(substr(m_files, 1, 2))
  
  for (country in countries) {
  
    # Get all files associated with that country
    country_files <- al_files <- grep(paste0("^", country), m_files, value = TRUE)
  
    # Empty list to store the datasets for that country
    dat_list <- list()
  
    for (i in 1:length(country_files)) {
      surv <- country_files[i]
      dhs_dat <- readRDS(paste0("data/merged_dhs/", surv))
  
      # Denormalise survey weights
      pop_data_country <- pop_data %>%
        filter(dhs == country, Time %in% unique(dhs_dat$v007)) %>%
        summarise(avg_PopFemale = mean(PopFemale)) %>%
        mutate(denorm_factor = avg_PopFemale/nrow(dhs_dat))
      dhs_dat$v005_denorm <- (dhs_dat$v005/1000000)*pop_data_country$denorm_factor
  
      # Put data in long format
      dhs_long <- dhs_dat %>%
        filter(!is.na(v010), !is.na(v012)) %>%
        mutate(
          start_age = 10L,
          # stop at min(17, current age, marriage age if known)
          end_age = pmin(17L, v012, ifelse(!is.na(v511), v511, Inf)),
          n_rows  = pmax(0L, end_age - start_age + 1L)
        ) %>%
        filter(n_rows > 0) %>%
        mutate(age_seq = map2(start_age, end_age, ~seq(.x, .y))) %>%
        unnest_longer(age_seq, values_to = "age") %>%
        mutate(
          year    = v010 + age,
          married = if_else(!is.na(v511) & age >= v511, 1L, 0L)
        ) %>%
        select(caseid, year, age, married, everything(), -start_age, -end_age, -n_rows)
  
      # Create unique case id and cluster id
      dhs_long$caseid <- paste(dhs_long$v000, dhs_long$caseid, sep = "-")
      dhs_long$clustid <- paste(dhs_long$v000, dhs_long$v001, sep = "-")
  
      # Add survey data to the list
      dat_list[[surv]] <- dhs_long
    }
  
    # Merge datasets together keeping labels
    comb_dat <- bind_rows(dat_list)
    comb_dat <- copy_labels(comb_dat, dat_list[[1]])
    
    # Add country variable
    comb_dat$dhs_cde <- country
    
    # Add cyclone data
    comb_dat <- link_windspeed(r_stack, comb_dat, lags = 1:3)
    
    # Save this data
    saveRDS(comb_dat, paste0("data/merged_cyclone/", country, ".rds"))
  }
  
}
