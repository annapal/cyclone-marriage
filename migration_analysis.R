
dat_all <- readRDS("data/dat_all.rds")

# Remove some countries from the analysis that have low exposure (<1% exposed)
dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))

# Remove observations with 0 weight
dat_all <- dat_all %>% filter(v005_denorm!=0)

# Create country-year FE
dat_all$country_year <- paste(dat_all$dhs_cde,  dat_all$year, sep = "_")


# Plot the migration variable ---------------------------------------------

# Take first observation for each person
dat_first <- dat_all %>%
  group_by(caseid) %>%
  slice(1) %>%
  ungroup()

# Calculate variable for misclassified
dat_first <- dat_first %>%
  mutate(
    moved = case_when(
      # 1) Always lived in same place
      al_lived == 1 ~ 0L,
      
      # 2) Move occurred before childhood
      !is.na(v012) & !is.na(v104) & (v012 - v104) <= 17 ~ 0L,
      
      # 3) Unknown region moved to
      al_lived == 0 & is.na(move_same_region) ~ NA_integer_,
      
      # 4) Moved to the same region
      al_lived == 0 & move_same_region == 1 ~ 0L,
      
      # 5) Moved to a different region
      al_lived == 0 & move_same_region == 0 ~ 1L,
      
      # Unknown
      is.na(al_lived) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )
table(dat_first$moved, useNA="always")

# Create bar plot
plot_data <- dat_first %>%
  mutate(
    moved_cat = case_when(
      moved == 0 ~ "No",
      moved == 1 ~ "Yes",
      is.na(moved) ~ "Missing"
    ),
    country = countrycode(dhs_cde, "dhs", "country.name")
  ) %>%
  group_by(country, moved_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

ggplot(plot_data, aes(x = prop, y = fct_rev(country), fill = moved_cat)) +
  geom_col(position = "stack") +
  labs(
    x = "Proportion",
    y = "Country",
    fill = "Moved"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Yes" = "#d73027",     # red shade
      "No" = "#4575b4",      # green shade
      "Missing" = "grey70"   # grey
    )
  )
ggsave("figures/migration_plot.jpeg")
