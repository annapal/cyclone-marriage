
# Create a stacked raster file using the TC data
# Tif file saved in "data/cy_stack.tif"
# Maps for each year 1980-2015 are saved in "figures/cyclone_plots/"

merge_tc_data <- function() {

  # Get list of DHS countries
  m_files <- list.files("data/merged_dhs/", full.names = FALSE)
  countries <- countrycode(unique(substr(m_files, 1, 2)), "dhs", "iso3c")
  
  # Get TC data (1980–2015)
  files <- list.files("data/TC_data")
  filtered_files <- files[grepl("^(198[0-9]|199[0-9]|200[0-9]|201[0-5])", files)] 
  
  # Merge all cyclone data into one dataframe
  df <- data.frame()
  for (i in seq_along(filtered_files)) {
    
    # Only get data from DHS countries
    cy_dat <- read.csv(paste0("data/TC_data/", filtered_files[i])) %>%
      filter(ISO %in% countries)
    if (nrow(cy_dat) > 0) {
      cy_dat$year <- as.numeric(substr(filtered_files[i], 1, 4))
      df <- rbind(df, cy_dat)
    }
  }
  
  # Variables to include in stacked raster file
  vars <- c("exposed_assets", "exposed_pop", "windspeed")
  
  # Derive grid template raster (WGS84)
  r_template <- terra::rast(
    xmin = min(df$LON) - 0.1/2, xmax = max(df$LON) + 0.1/2,
    ymin = min(df$LAT) - 0.1/2, ymax = max(df$LAT) + 0.1/2,
    resolution = c(0.1, 0.1), crs = "EPSG:4326"
  )
  
  # Make a SpatVector of points
  pts <- terra::vect(df, geom = c("LON","LAT"), crs = "EPSG:4326")
  
  # Rasterize each variable for every unique year, then combine into one stack
  yrs <- sort(unique(df$year))  # get sorted list of available years
  
  layers <- lapply(yrs, function(yy) {
    # Subset points to the current year
    pyy <- pts[pts$year == yy, ]
    
    # Rasterize each variable for this year
    rs <- lapply(vars, function(v) {
      # Assign cell value as the max of points falling within that cell
      terra::rasterize(pyy, r_template, field = v, fun = "max")
    })
    
    # Combine yearly variable rasters into a multi-layer SpatRaster
    s <- terra::rast(rs)
    names(s) <- paste0(vars, "_", yy)
    s
  })
  
  # Merge all yearly stacks into one multi-layer SpatRaster
  r_stack <- do.call(c, layers)
  
  # Save raster file
  terra::writeRaster(r_stack, "data/cy_stack.tif", overwrite = TRUE)
  
  # Create a plot for each year to check
  
  # Get country boundaries
  world0 <- geodata::world(path = tempdir())
  
  # Ensure output dir exists
  dir.create(file.path("figures", "cyclone_plots"), recursive = TRUE, showWarnings = FALSE)
  
  # If world0 is sf/sp object, convert once to SpatVector
  world <- if (inherits(world0, "SpatVector")) world0 else terra::vect(world0)
  
  for (yr in yrs) {
    # Pick the layer
    lyr <- r_stack[[paste0("windspeed_", yr)]]
    
    # Project & crop world to match layer
    w <- terra::project(world, terra::crs(lyr))
    w <- terra::crop(w, terra::ext(lyr))
    
    # Filename
    outfile <- file.path("figures", "cyclone_plots", paste0("windspeed_", yr, ".jpg"))
    
    # Draw & save
    jpeg(outfile, width = 1600, height = 1200, res = 200)
    terra::plot(lyr, main = paste0("Windspeed (", yr, ")"))
    terra::lines(w, lwd = 0.7)
    dev.off()
  }
}
