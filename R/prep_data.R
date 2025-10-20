
# Prepare the data for analysis
# Merges all country datasets into one ("data/dat_all.rds")

prep_data <- function() {

  # List to store the data
  dat_list <- list()
  
  # Merged country files
  files <- list.files("data/merged_cyclone", full.names = TRUE)
  
  for (file in files) {
  
    dat <- readRDS(file) # Read in the data
    
    # If there are no positive windspeeds skip it
    if (all(dat$windspeed==0)) next
    
    # Filter to relevant years
    dat <- dat %>% filter(year %in% 1983:2015)
    
    # Create binary exposure variables (current & lags)
    dat <- dat %>%
      mutate(
        exp34      = as.integer(windspeed        > 34),
        exp64      = as.integer(windspeed        > 64),
        exp96      = as.integer(windspeed        > 96),
        
        exp34_lag1 = as.integer(windspeed_lag1   > 34),
        exp34_lag2 = as.integer(windspeed_lag2   > 34),
        exp34_lag3 = as.integer(windspeed_lag3   > 34),
        
        exp64_lag1 = as.integer(windspeed_lag1   > 64),
        exp64_lag2 = as.integer(windspeed_lag2   > 64),
        exp64_lag3 = as.integer(windspeed_lag3   > 64),
        
        exp96_lag1 = as.integer(windspeed_lag1   > 96),
        exp96_lag2 = as.integer(windspeed_lag2   > 96),
        exp96_lag3 = as.integer(windspeed_lag3   > 96)
      )
    
    # Sum lags to get number of cyclones experienced in past 3 years
    dat <- dat %>%
      mutate(
        exp34_num = exp34_lag1 + exp34_lag2 + exp34_lag3,
        exp64_num = exp64_lag1 + exp64_lag2 + exp64_lag3,
        exp96_num = exp96_lag1 + exp96_lag2 + exp96_lag3,
      )
    
    # Make sure every cluster has the same rural/urban classification
    dat <- dat %>%
      group_by(clustid) %>%
      mutate(
        # Count Urban and Rural in the cluster
        n_rural = sum(v102 == "Rural"),
        n_urban = sum(v102 == "Urban"),
        
        # Set to the majority value in the cluster
        v102_majority = if_else(n_rural >= n_urban, "Rural", "Urban")
      ) %>%
      # Replace the original hv025 with the majority version
      mutate(rural = v102_majority) %>%
      select(-n_rural, -n_urban, -v102_majority) %>%
      ungroup()
    
    # Add dataset to list
    dat_list[[file]] <- dat
  }
  
  # Bind all datasets
  dat_all <- bind_rows(dat_list)
  
  # Remove some countries from the analysis that have low exposure (<1% exposed)
  dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))
  
  # Remove observations with 0 weight
  dat_all <- dat_all %>% filter(v005_denorm!=0)
  
  # Create country-year FE
  dat_all$country_year <- paste(dat_all$dhs_cde,  dat_all$year, sep = "_")
  
  # Save the data
  saveRDS(dat_all, "data/dat_all.rds")
  
  return(dat_all)
}

