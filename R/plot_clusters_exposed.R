
plot_clusters_exposed <- function() {
  
  dat_all <- readRDS("data/dat_all.rds")
  
  # Remove some countries from the analysis that have low exposure (<1% of sample)
  dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))
  
  # Calculate the proportion of the cluster exposed to floods
  clust_dat <- dat_all %>%
    group_by(clustid, dhs_cde) %>%
    summarise(
      prop_cyc_exp = mean(exp34)) %>%
    mutate(country = countrycode(dhs_cde, "dhs", "country.name.en"))
  
  # Plot all the clusters
  p <- ggplot(clust_dat, aes(x = prop_cyc_exp, y = fct_rev(country))) +
    geom_jitter(height = 0.3, width = 0, alpha = 0.6)  +
    labs(x = "Proportion of cluster observations exposed to winds >34 knots", y = "Country") +
    theme_minimal()
  ggsave("figures/cluster_proportion.jpeg", height=10)
  
}