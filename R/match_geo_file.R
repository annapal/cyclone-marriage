# Function to get matching geo files for one DHS file

match_geo_file <- function(file_name, geo_files) {
  # Extract survey name
  file_component <- basename(dirname(file_name))
  
  # Extract country code and phase
  cc <- substr(file_component, 1, 2)
  phase <- substr(file_component, 5, 6)
  
  # Extract phase number and version
  phase_num <- substr(phase, 1, 1)
  version <- substr(phase, 2, 2)
  
  # Determine the range
  if (version %in% LETTERS[1:8]) {
    range <- LETTERS[1:8]
  } else if (version %in% LETTERS[9:17]) {
    range <- LETTERS[9:17]
  } else if (version %in% LETTERS[18:26]) {
    range <- LETTERS[18:26]
  } else if (version %in% as.character(0:9)) {
    range <- as.character(0:9)
  } else {
    return(character(0))
  }
  
  # Build all possible geo file stems
  possible_stems <- paste0(cc, "GE", phase_num, range, "FL")
  
  # Match against geo_files
  matches <- geo_files[grepl(paste0("geocodes/(", paste(possible_stems, collapse = "|"), ")/"), geo_files)]
  
  return(matches)
}
