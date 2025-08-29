source("packages.R")
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

# files <- match_geo_df()
# merge_geocodes(files)
# merge_tc_data()
# merge_data()
# prep_data()

# Plots
plot_clusters_exposed()
plot_windspeed_map()
plot_child_marriage_map()
make_table()
plot_missing_dates()
