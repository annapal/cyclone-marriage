
# Map of child marriage prevalence

plot_child_marriage_map <- function(dat_all) {
  
  # Take the first observation for each person
  dat_first <- dat_all %>%
    group_by(caseid) %>%
    slice(1) %>%
    ungroup()
  
  # Average proportion married before 18 in each cluster
  prop_mar <- dat_first %>%
    group_by(clustid, geometry) %>%
    summarise(
      mar_prop = mean(v511<18, na.rm = TRUE)
    )
  
  # Make into sf object
  merged_prop <- st_as_sf(prop_mar)
  merged_prop <- merged_prop %>%
    arrange(prop_mar)
  
  # Get world map
  world_map <- maps::map("world", fill = TRUE, plot = FALSE)
  world_sf <- st_as_sf(world_map)
  
  # Make the map
  map_plot <- ggplot(data = world_sf) +
    geom_sf(fill = "gray80", color = "white") +
    geom_sf(data = merged_prop, aes(color = mar_prop), 
            size = 0.01, shape = 20) +
    scale_color_viridis_c(
      name = "Proportion of cluster\nmarried before 18\n",
      option = "magma",
      direction = 1,
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 0.5,
        barheight = 5
      )
    ) +
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
  
  # Save the map
  ggsave("figures/cluster_map.jpeg", plot = map_plot, height = 5, width = 11)
}
