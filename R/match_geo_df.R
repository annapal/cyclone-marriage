
# Create dataframe that has data file name and corresponding geo file name

match_geo_df <- function() {
  # Find all .DTA files recursively in subfolders
  dta_files <- list.files("data/dhs", pattern = "\\.dta$", 
                          recursive = TRUE, 
                          full.names = TRUE, 
                          ignore.case = TRUE)
  
  # Get file names for geocoded data
  geo_files <- list.files("data/geocodes", pattern = "\\.shp$", 
                          recursive = TRUE, 
                          full.names = TRUE, 
                          ignore.case = TRUE)
  
  # Create a dataframe that for each dta file matches the geo file using match_geo_file
  output <- list()
  for (i in seq_along(dta_files)) {
    dta_file <- dta_files[i]
    
    # Call your matching function
    geo_file <- match_geo_file(dta_file, geo_files)
    
    # If no match found, assign NA
    if (length(geo_file) == 0) {
      geo_file <- NA_character_
    }
    
    # Store results
    output[[i]] <- list(dta_file = dta_file, geo_file = geo_file)
  }
  files <- do.call(rbind, lapply(output, as.data.frame))
  
  # Add excluded surveys here
  excluded_surveys <- c()
  
  # Remove excluded surveys
  files <- files %>%
    filter(!(dta_file %in% excluded_surveys))
  
  files
}
