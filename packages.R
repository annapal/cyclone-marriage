# Core packages (loaded)
library(haven)
library(tidyverse)
library(writexl)
library(sf)
library(countrycode)
library(lubridate)
library(readxl)
library(fixest)
library(broom)

# Secondary packages (ensure installed but not loaded)
extra_pkgs <- c("terra", "sjlabelled", "Hmisc", "maps")

# Check which are missing, then install if needed
missing_pkgs <- extra_pkgs[!extra_pkgs %in% installed.packages()[, "Package"]]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
