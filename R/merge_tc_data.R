
merge_tc_data <- function() {

  # Get DHS countries
  m_files <- list.files("data/merged_dhs/", full.names = FALSE)
  countries <- countrycode(unique(substr(m_files, 1, 2)), "dhs", "iso3c")
  
  # Get TC data (1980–2015)
  files <- list.files("data/TC_data")
  filtered_files <- files[grepl("^(198[0-9]|199[0-9]|200[0-9]|201[0-5])", files)] 
  
  # Read in the cyclone data
  cy_dat_all <- data.frame()
  
  # Merge into one dataframe
  for (i in seq_along(filtered_files)) {
    cy_dat <- read.csv(paste0("data/TC_data/", filtered_files[i])) %>%
      filter(ISO %in% countries)
    if (nrow(cy_dat) > 0) {
      cy_dat$year <- as.numeric(substr(filtered_files[i], 1, 4))
      cy_dat_all <- rbind(cy_dat_all, cy_dat)
      message(paste0(i, "/", length(filtered_files)))
    }
  }
  
  # Create a stacked raster file
  df <- cy_dat_all
  vars <- c("exposed_assets", "exposed_pop", "windspeed")
  
  # 1) derive grid/resolution & template raster (WGS84)
  r_template <- rast(
    xmin = min(df$LON) - 0.1/2, xmax = max(df$LON) + 0.1/2,
    ymin = min(df$LAT) - 0.1/2, ymax = max(df$LAT) + 0.1/2,
    resolution = c(0.1, 0.1), crs = "EPSG:4326"
  )
  
  # 2) make a SpatVector of points
  pts <- vect(df, geom = c("LON","LAT"), crs = "EPSG:4326")
  
  # 3) rasterize each variable per year, stack all layers
  yrs <- sort(unique(df$year))
  layers <- lapply(yrs, function(yy) {
    pyy <- pts[pts$year == yy, ]
    rs <- lapply(vars, function(v) {
      # if same cell affected multiple times, take max windspeed
      rasterize(pyy, r_template, field = v, fun = "max") 
    })
    s <- rast(rs)
    names(s) <- paste0(vars, "_", yy)
    s
  })
  r_stack <- do.call(c, layers)   # SpatRaster with layers var_year
  
  # 4) write to disk
  writeRaster(r_stack, "data/cy_stack.tif", overwrite = TRUE)
  
  # get country boundaries once
  world0 <- geodata::world(path = tempdir())  # SpatVector (EPSG:4326)
  
  years <- 1980:2015
  for (yr in years) {
    nm <- paste0("windspeed_", yr)
    if (!nm %in% names(r_stack)) {
      message("Skipping ", yr, ": layer ", nm, " not found in r_stack.")
      next
    }
    
    lyr <- r_stack[[nm]]
    
    # project/crop boundaries to match this layer
    world <- project(world0, crs(lyr))
    world <- crop(world, ext(lyr))
    
    # file name
    outfile <- file.path("figures", "cyclone_plots", paste0("windspeed_", yr, ".jpg"))
    
    # save plot
    jpeg(outfile, width = 1600, height = 1200, res = 200)
    plot(lyr, main = paste0("Windspeed (", yr, ")"))
    lines(world, lwd = 0.7)
    dev.off()
  }  
}
