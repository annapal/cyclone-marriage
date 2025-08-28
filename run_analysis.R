
dat_all <- readRDS("data/dat_all.rds")

# Remove some countries from the analysis that have low exposure (<1% of sample)
dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))

# Remove observations with 0 weight
dat_all <- dat_all %>% filter(v005_denorm!=0)

# Create country-year FE
dat_all$country_year <- paste(dat_all$dhs_cde,  dat_all$year,     sep = "_")

# Run the model
mod <- feols(
  married ~ 
    i(dhs_cde, exp34) +
    i(dhs_cde, exp34_lag1) +
    i(dhs_cde, exp34_lag2) +
    i(dhs_cde, exp34_lag3) +
    age |
    clustid + country_year,
  data   = dat_all,
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
ggsave("figures/main.jpeg", plot, height = 6, width = 6)
