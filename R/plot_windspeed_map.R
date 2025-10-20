
# Make a map of TC windspeeds globally (max in each grid cell)

plot_windspeed_map <- function() {

  # Get TC data (1980–2015)
  files <- list.files("data/TC_data")
  filtered_files <- files[grepl("^(198[0-9]|199[0-9]|200[0-9]|201[0-5])", files)] 
  
  # Read in the cyclone data
  cy_dat_all <- data.frame()
  
  # Merge into one dataframe
  for (i in seq_along(filtered_files)) {
    cy_dat <- read.csv(paste0("data/TC_data/", filtered_files[i]))
    cy_dat$year <- as.numeric(substr(filtered_files[i], 1, 4))
    cy_dat_all <- rbind(cy_dat_all, cy_dat)
  }
  
  # Create a stacked raster file
  df <- cy_dat_all
  vars <- c("exposed_assets", "exposed_pop", "windspeed")
  
  # Derive template raster (WGS84)
  r_template <- terra::rast(
    xmin = min(df$LON) - 0.1/2, xmax = max(df$LON) + 0.1/2,
    ymin = min(df$LAT) - 0.1/2, ymax = max(df$LAT) + 0.1/2,
    resolution = c(0.1, 0.1), crs = "EPSG:4326"
  )
  
  # Make a SpatVector of points
  pts <- terra::vect(df, geom = c("LON","LAT"), crs = "EPSG:4326")
  
  # Rasterize each variable per year, stack all layers
  yrs <- sort(unique(df$year))
  layers <- lapply(yrs, function(yy) {
    pyy <- pts[pts$year == yy, ]
    rs <- lapply(vars, function(v) {
      # if same cell affected multiple times, take max windspeed
      terra::rasterize(pyy, r_template, field = v, fun = "max") 
    })
    s <- terra::rast(rs)
    names(s) <- paste0(vars, "_", yy)
    s
  })
  cy_stack <- do.call(c, layers)
  
  # Subset windspeed layers for 1980–2015
  ws_names  <- grep("^windspeed_", names(cy_stack), value = TRUE)
  ws_stack  <- cy_stack[[ws_names]]
  
  # Get max windspeed
  ws_max <- terra::app(ws_stack, fun = max, na.rm = TRUE)
  names(ws_max) <- "windspeed_max_1980_2015"
  
  # Set as dataframe
  ws_df <- as.data.frame(ws_max, xy = TRUE, na.rm = TRUE)
  
  # Get world map
  world_map <- maps::map("world", fill = TRUE, plot = FALSE)
  world_sf <- st_as_sf(world_map)
  
  # Make the map
  map_plot <- ggplot() +
    geom_sf(data = world_sf, fill = "gray80", color = "white") +
    geom_tile(data = ws_df, aes(x = x, y = y, fill = windspeed_max_1980_2015)) +
    theme_minimal() +
    scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      name = "Max Windspeed"
    ) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.15, 0.30),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.height = unit(0.6, "cm"),
      legend.key.width = unit(0.3, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  
  # Save the map
  ggsave("figures/cyclone_map.jpeg", plot = map_plot, height = 5, width = 11)
}
