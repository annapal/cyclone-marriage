
dat_all <- readRDS("data/dat_all.rds")

# Remove some countries from the analysis that have low exposure (<1% exposed)
dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))

# Remove observations with 0 weight
dat_all <- dat_all %>% filter(v005_denorm!=0)

# Create country-year FE
dat_all$country_year <- paste(dat_all$dhs_cde,  dat_all$year, sep = "_")


# Create migration variables ---------------------------------------------

# Take first observation for each person
# dat_first <- dat_all %>%
#   group_by(caseid) %>%
#   slice(1) %>%
#   ungroup()

# Year & age migrated
dat_all$migration_year <- dat_all$v007-dat_all$v104
dat_all$migration_age <- pmax(dat_all$v012-dat_all$v104, 0)

# Variable for timing of migration
dat_all <- dat_all %>%
  mutate(
    migration_timing = case_when(
      # 1) Always lived in same place
      al_lived == 1 ~ "Never migrated",
      
      # 2) Move occurred before age 10
      !is.na(migration_age) & migration_age<10 ~ "Migrated before age 10",

      # 3) Moved after
      al_lived == 0 ~ "Migrated after age 10",
      
      # Unknown
      TRUE ~ NA
    )
  )

# TODO: How many migrated within the same region


# Determine if people exposed to TCs migrate more -------------------------

prop.table(table(dat_first$exp34, dat_first$migration_timing, useNA = "always"), margin=1)

# TODO: Model migration as outcome
dat_all <- dat_all %>%
  mutate(
    migration = case_when(
      al_lived==1 ~ 0,
      migration_year==year ~ 1,
      migration_year!=year ~ 0,
      TRUE ~ NA_real_
    )
  )

# Run the model
mod <- feols(
  migration ~ 
    i(dhs_cde, exp34) +
    age |
    clustid + country_year,
  data   = dat_all,
  vcov   = ~ clustid,
  weights = ~ v005_denorm
)
summary(mod)

# Analysis repeated among those who haven't migrated ----------------------

# Subset the data
dat_all_subs <- dat_all %>%
  filter(migration_timing %in% c("Never migrated", "Migrated before age 10"))

# Run the model
mod <- feols(
  married ~ 
    i(dhs_cde, exp34) +
    i(dhs_cde, exp34_lag1) +
    i(dhs_cde, exp34_lag2) +
    i(dhs_cde, exp34_lag3) +
    age |
    clustid + country_year,
  data   = dat_all_subs,
  vcov   = ~ clustid,
  weights = ~ v005_denorm
)

# Tidy the results
res_clean <- broom::tidy(mod, conf.int = TRUE) %>%
  mutate(
    dhs_cde   = str_extract(term, "(?<=::)[A-Z]{2}"),
    variable  = str_remove(term, ".*:"),
    country = countrycode(dhs_cde, "dhs", "country.name"),
    year = case_when(
      variable == "exp34"       ~ 0L,
      variable == "exp34_lag1"  ~ 1L,
      variable == "exp34_lag2"  ~ 2L,
      variable == "exp34_lag3"  ~ 3L,
      TRUE ~ NA_integer_)
  ) %>%
  select(term, estimate, conf.low, conf.high, std.error, statistic, p.value, country, year) %>%
  filter(!is.na(country))

# Plot the results
plot <- ggplot(res_clean, aes(x = year, y = estimate, group = country)) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  labs(
    x = "Years since tropical cyclone",
    y = "Change in the annual rate of child marriage"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(),
    axis.ticks.length = grid::unit(3, "pt"),
    axis.ticks = element_line(linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(fill = NA, linewidth = 0.5),
    strip.text = element_text(hjust = 0)
  ) +
  facet_wrap(~ country, ncol = 4) +
  coord_cartesian(ylim = c(-0.05, 0.05), xlim = c(-0.5, 3.5))
ggsave("figures/main_migration.jpeg", plot, height = 6, width = 6)





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
