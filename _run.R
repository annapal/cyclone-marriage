source("packages.R")
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

files <- match_geo_df()
merge_geocodes(files)
