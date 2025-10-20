
# Unzip files

unzip_files <- function(zip_folder) {
  
  # List all .zip files in the folder
  zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)
  
  # Extract each zip file
  for (zip_file in zip_files) {
    
    # Create a subfolder for each ZIP file
    folder_name <- tools::file_path_sans_ext(basename(zip_file))
    out_dir <- file.path(zip_folder, folder_name)
    dir.create(out_dir, showWarnings = FALSE)
    
    # Unzip the files
    unzip(zip_file, exdir = out_dir)
  }
}

## Unzip DHS data files and geocodes
unzip_files("data/dhs")
unzip_files("data/geocodes")

