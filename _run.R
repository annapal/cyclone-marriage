source("packages.R")
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

# Data prep ---------------------------------------------------------------

# Read in table of DHS file names and associated geocode files names
files <- read.csv("data/files.csv")

# Merge DHS datasets with geocodes (saved in data/merged_dhs/)
merge_geocodes(files)

# Create TC raster file (saved in data/cy_stack.tif)
merge_tc_data()

# Merge TC data and DHS data (saved in data/merged_cyclone/)
merge_data()

# Merge all country data into one dataset
dat_all <- prep_data()


# Descriptives and plots --------------------------------------------------

# Get descriptive statistics for the paper (saved in results/descriptives.xslx)
get_descriptives(dat_all)

# Make table of countries and surveys for SI (saved in results/summary_table.xlsx)
make_table(dat_all)

# Map TC & child marriage map (saved in figures/)
plot_windspeed_map()
plot_child_marriage_map(dat_all)

# Plot missing data (saved in figures/)
plot_missing_dates(dat_all)

# Plot the annual rate of child marriage (saved in figures/)
plot_annual_rate(dat_all)

# Plot proportion py exposed in each country (saved in figures/)
plot_cyclones_temporal(dat_all)


# Run the analysis --------------------------------------------------------

# Run the main analysis (outputs saved in results/ and figures/)
run_analysis(dat_all)

# Run the migration analysis (outputs saved in figures/)
run_migration_analysis(dat_all)

