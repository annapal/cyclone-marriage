
plot_windspeed_map <- function() {

  cy_stack <- rast("data/cy_stack.tif")
  
  # Get max windspeed
  
  # 1) subset windspeed layers for 1980–2015
  ws_names  <- grep("^windspeed_", names(cy_stack), value = TRUE)
  ws_stack  <- cy_stack[[ws_names]]
  
  # 2) per-cell max across years
  ws_max <- app(ws_stack, fun = max, na.rm = TRUE)
  names(ws_max) <- "windspeed_max_1980_2015"
  
  ws_df <- as.data.frame(ws_max, xy = TRUE, na.rm = TRUE)
  
  # Plot on world map
  world_map <- map("world", fill = TRUE, plot = FALSE)
  world_sf <- st_as_sf(world_map)
  
  map_plot <- ggplot() +
    geom_sf(data = world_sf, fill = "gray80", color = "white") +
    geom_tile(data = ws_df, aes(x = x, y = y, fill = windspeed_max_1980_2015)) +
    theme_minimal() +
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
  ggsave("figures/cyclone_map.jpeg", plot = map_plot, height = 5, width = 11)
}
