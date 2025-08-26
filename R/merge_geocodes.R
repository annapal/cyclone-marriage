
# Merge DHS geocodes to survey data
# Also trim down the survey data to save memory
# Datasets are saved in data/merged_dhs/

merge_geocodes <- function(files) {

  for (i in 165:nrow(files)) {
    
    # Get file names
    dta_name <- files$dta_file[i]
    geo_name <- files$geo_file[i]
    
    # If geo data is available
    if (!is.na(geo_name)) {
      
      # Read in dhs dataset
      dat <- read_dta(dta_name)
      
      # Read in the geocodes
      geo_dat <- st_read(geo_name, quiet=TRUE)
      
      # Desired variables
      vars <- c("caseid",
                "v000", "v001", "v002", "v003", "v005", "v006", "v007", "v008", "v102", 
                "v009", "v010", "v011", "v012", "v014",
                "v104", 
                "v502", "v507", "v508", "v509", "v510", "v511")
      
      # Select relevant variables
      dat <- dat %>% select(all_of(vars))

      # Re-factor and label variables for consistency across datasets
      dat$caseid <- as.character(dat$caseid)
      attr(dat$caseid, "label") <- "Case ID"
      
      dat$v000 <- as.character(dat$v000)
      attr(dat$v000, "label") <- "Survey"
      
      dat$v001 <- as.integer(dat$v001)
      attr(dat$v001, "label") <- "Cluster Number"
      
      dat$v002 <- as.integer(dat$v002)
      attr(dat$v002, "label") <- "Household Number"
      
      dat$v003 <- as.integer(dat$v003)
      attr(dat$v003, "label") <- "Line Number"
      
      dat$v005 <- as.integer(dat$v005)
      attr(dat$v005, "label") <- "Survey Weight"
      
      dat$v006 <- as.integer(dat$v006)
      attr(dat$v006, "label") <- "Interview Month"
      
      dat$v007 <- as.integer(dat$v007)
      attr(dat$v007, "label") <- "Interview Year"
      
      dat$v008 <- as.integer(dat$v008)
      attr(dat$v008, "label") <- "Interview CMC"
      
      dat$v102 <- factor(dat$v102, levels=c(1,2), labels=c("Urban", "Rural"))
      attr(dat$v102, "label") <- "Urban/rural residence"
      
      dat$v009 <- as.integer(dat$v009)
      attr(dat$v009, "label") <- "Month of birth"
      
      dat$v010 <- as.integer(dat$v010)
      attr(dat$v010, "label") <- "Year of birth"
      
      dat$v011 <- as.integer(dat$v011)
      attr(dat$v011, "label") <- "Birth CMC"
      
      dat$v012 <- as.integer(dat$v012)
      attr(dat$v012, "label") <- "Current age"
      
      dat$v014 <- factor(dat$v014, levels=1:8, 
                         labels=c("month and year", 
                                  "month and age -y imp",
                                  "year and age - m imp",
                                  "y & age - y ignored",
                                  "year - a, m imp", 
                                  "age - y, m imp",
                                  "month - a, y imp",
                                  "none - all imp"))
      attr(dat$v014, "label") <- "Completeness of birth information"
      
      dat$al_lived <- ifelse(
        is.na(dat$v104) | dat$v104 %in% c(96:99), NA,
        as.integer(dat$v104 == 95)
      )
      attr(dat$al_lived, "label") <- "Always lived in same location"
      
      dat$v104 <- ifelse(dat$v104>=95, NA, as.numeric(dat$v104))
      attr(dat$v104, "label") <- "Years lived in current place of residence"
      
      dat$v502 <- factor(dat$v502,
                          levels = c(0, 2, 1),
                          labels = c("never", "formerly", "currently"))
      attr(dat$v502, "label") <- "Current marrital status"
      
      dat$samp <- ifelse(all(dat$v502!=0), "Ever Married Women",
                                "All Women")
      attr(dat$samp, "label") <- "Ever-married sample indicator"
      
      dat$v507 <- as.integer(dat$v507)
      attr(dat$v507, "label") <- "Month of marriage"
      
      dat$v508 <- as.integer(dat$v508)
      attr(dat$v508, "label") <- "Year of marriage"
      
      dat$v509 <- as.integer(dat$v509)
      attr(dat$v509, "label") <- "Marriage CMC"
      
      dat$v510 <- factor(dat$v510, levels=1:8, 
                         labels=c("month and year", 
                                  "month and age -y imp",
                                  "year and age - m imp",
                                  "y & age - y ignored",
                                  "year - a, m imp", 
                                  "age - y, m imp",
                                  "month - a, y imp",
                                  "none - all imp"))
      attr(dat$v510, "label") <- "Completeness of marriage information"
      
      dat$v511 <- as.integer(dat$v511)
      attr(dat$v511, "label") <- "Marriage age"
      
      # Convert dates to gregorian calendar where necessary
      if (substr(dat$v000[1], 1, 2)=="ET") {
        
        # Interview date variables
        dat$v008 <- dat$v008+92
        dat$v007 <- trunc((dat$v008-1)/12) + 1900
        dat$v006 <- dat$v008 - ((dat$v007-1900)*12)
        
        # Birth variables
        dat$v011 <- dat$v011+92
        dat$v010 <- trunc((dat$v011-1)/12) + 1900
        dat$v009 <- dat$v011 - ((dat$v010-1900)*12)
        
        # Marriage variables
        dat$v511 <- dat$v511+92
        dat$v508 <- trunc((dat$v511-1)/12) + 1900
        dat$v507 <- dat$v511 - ((dat$v508-1900)*12)
        
      }
      if (substr(dat$v000[1], 1, 2)=="NP") {
        
        # Interview date variables
        dat$v008 <- dat$v008-681
        dat$v007 <- trunc((dat$v008-1)/12) + 1900
        dat$v006 <- dat$v008 - ((dat$v007-1900)*12)
        
        # Birth variables
        dat$v011 <- dat$v011-681
        dat$v010 <- trunc((dat$v011-1)/12) + 1900
        dat$v009 <- dat$v011 - ((dat$v010-1900)*12)
        
        # Marriage variables
        dat$v511 <- dat$v511-681
        dat$v508 <- trunc((dat$v511-1)/12) + 1900
        dat$v507 <- dat$v511 - ((dat$v508-1900)*12)
      }
      
      # Select relevant variables of geodata
      geo_dat <- geo_dat %>%
        select(DHSCLUST, LATNUM, LONGNUM, ALT_GPS) %>%
        rename(v001 = DHSCLUST) # Make cluster var name same
      
      # Merge with dat
      dat <- left_join(dat, geo_dat, by="v001")
      
      # Save the data
      saveRDS(dat, paste0("data/merged_dhs/", substr(dta_name, 10, 15), ".rds"))
    }
  }
}
