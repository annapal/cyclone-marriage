
dat_all <- readRDS("data/dat_all.rds")

# Remove some countries from the analysis that have low exposure (<1% of sample)
dat_all <- dat_all %>% filter(!(dhs_cde %in% c("CO", "ID", "MW", "PK", "TZ")))

# Remove observations with 0 weight
dat_all <- dat_all %>% filter(v005_denorm!=0)

# Create country-year FE
dat_all$country_year <- paste(dat_all$dhs_cde,  dat_all$year, sep = "_")


# Main model --------------------------------------------------------------

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

# Calculate cumulative effect
V <- vcov(mod)  # clustered vcov you used in estimation
cumulative <- res_clean %>%
  group_by(country) %>%
  summarise(
    terms = list(term),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    cum_estimate = sum(coef(mod)[terms]),
    cum_se = sqrt(sum(V[terms, terms])),
    cum_low = cum_estimate - 1.96 * cum_se,
    cum_high = cum_estimate + 1.96 * cum_se
  )

# Rural model --------------------------------------------------------------

dat_all$dhs_cde_rural <- interaction(dat_all$dhs_cde, dat_all$rural, drop = TRUE)

# Run the model
mod <- feols(
  married ~ 
    i(dhs_cde_rural, exp34) +
    i(dhs_cde_rural, exp34_lag1) +
    i(dhs_cde_rural, exp34_lag2) +
    i(dhs_cde_rural, exp34_lag3) +
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
    residence = str_extract(term, "(?<=\\.)Rural|Urban"),
    variable  = str_remove(term, ".*:"),
    country = countrycode(dhs_cde, "dhs", "country.name"),
    year = case_when(
      variable == "exp34"       ~ 0L,
      variable == "exp34_lag1"  ~ 1L,
      variable == "exp34_lag2"  ~ 2L,
      variable == "exp34_lag3"  ~ 3L,
      TRUE ~ NA_integer_)
  ) %>%
  select(term, estimate, conf.low, conf.high, std.error, statistic, p.value,
         country, residence, year) %>%
  filter(!is.na(country))

# Plot the results
plot <- ggplot(
  res_clean,
  aes(x = year, y = estimate,
      group = interaction(country, residence),
      color = residence, fill = residence)
) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
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
  coord_cartesian(ylim = c(-0.05, 0.05), xlim = c(-0.5, 3.5)) +
  scale_color_viridis_d(end = 0.85) +
  scale_fill_viridis_d(end = 0.85)
ggsave("figures/rural_urban.jpeg", plot, height = 6, width = 7)

# Calculate cumulative effect
V <- vcov(mod)  # clustered vcov you used in estimation
cumulative_res <- res_clean %>%
  group_by(country, residence) %>%
  summarise(
    terms = list(term),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    cum_estimate = sum(coef(mod)[terms]),
    cum_se = sqrt(sum(V[terms, terms])),
    cum_low = cum_estimate - 1.96 * cum_se,
    cum_high = cum_estimate + 1.96 * cum_se
  )

# pre/post 2000 model --------------------------------------------------------------

dat_all$post_2000 <- ifelse(dat_all$year<2000, 0, 1)
dat_all$dhs_cde_2000 <- interaction(dat_all$dhs_cde, dat_all$post_2000, drop = TRUE)

# Run the model
mod <- feols(
  married ~ 
    i(dhs_cde_2000, exp34) +
    i(dhs_cde_2000, exp34_lag1) +
    i(dhs_cde_2000, exp34_lag2) +
    i(dhs_cde_2000, exp34_lag3) +
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
    period_code  = str_extract(term, "(?<=\\.)[01](?=:)"),
    period = recode(period_code, "0" = "Pre-2000", "1" = "Post-2000"),
    variable  = str_remove(term, ".*:"),
    country   = countrycode(dhs_cde, "dhs", "country.name"),
    year      = case_when(
      variable == "exp34"       ~ 0L,
      variable == "exp34_lag1"  ~ 1L,
      variable == "exp34_lag2"  ~ 2L,
      variable == "exp34_lag3"  ~ 3L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(term, estimate, conf.low, conf.high, std.error, statistic, p.value,
         country, period, year) %>%
  filter(!is.na(country))

# Plot the results
plot <- ggplot(
  res_clean,
  aes(x = year, y = estimate,
      group = interaction(country, period),
      color = period, fill = period)
) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
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
  coord_cartesian(ylim = c(-0.05, 0.05), xlim = c(-0.5, 3.5)) +
  scale_color_viridis_d(end = 0.85) +
  scale_fill_viridis_d(end = 0.85)
ggsave("figures/period.jpeg", plot, height = 6, width = 7)

# Calculate cumulative effect
V <- vcov(mod)  # clustered vcov you used in estimation
cumulative_period <- res_clean %>%
  group_by(country, period) %>%
  summarise(
    terms = list(term),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    cum_estimate = sum(coef(mod)[terms]),
    cum_se = sqrt(sum(V[terms, terms])),
    cum_low = cum_estimate - 1.96 * cum_se,
    cum_high = cum_estimate + 1.96 * cum_se
  )

# 64< model --------------------------------------------------------------

# Remove some countries from the analysis that have low exposure (<1% of sample)
dat_all2 <- dat_all %>% filter(!(dhs_cde %in% c("GU", "HN", "IA", "MZ", "TL", "ZW")))

# Run the model
mod <- feols(
  married ~ 
    i(dhs_cde, exp64) +
    i(dhs_cde, exp64_lag1) +
    i(dhs_cde, exp64_lag2) +
    i(dhs_cde, exp64_lag3) +
    age |
    clustid + country_year,
  data   = dat_all2,
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
      variable == "exp64"       ~ 0L,
      variable == "exp64_lag1"  ~ 1L,
      variable == "exp64_lag2"  ~ 2L,
      variable == "exp64_lag3"  ~ 3L,
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
ggsave("figures/main_64.jpeg", plot, height = 4, width = 6)

# Calculate cumulative effect
V <- vcov(mod)  # clustered vcov you used in estimation
cumulative_64 <- res_clean %>%
  group_by(country) %>%
  summarise(
    terms = list(term),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    cum_estimate = sum(coef(mod)[terms]),
    cum_se = sqrt(sum(V[terms, terms])),
    cum_low = cum_estimate - 1.96 * cum_se,
    cum_high = cum_estimate + 1.96 * cum_se
  )


# Make cumulative plot ----------------------------------------------------

# Combined cumulative results
combined <- bind_rows(
  cumulative       %>% mutate(strata = "exposure >34 knots"),
  cumulative_64    %>% mutate(strata = "exposure >64 knots"),
  cumulative_period %>% rename(strata = period),
  cumulative_res    %>% rename(strata = residence)
)

plot <- ggplot(combined, aes(x = cum_estimate, y = fct_rev(strata), color = strata)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_errorbarh(aes(xmin = cum_low, xmax = cum_high), height = 0.2, alpha = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ country, ncol = 3, scales = "free_y") +
  labs(
    x = "Cumulative effect 3 years after landfall",
    y = "Strata"
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
    legend.position = "none",
    strip.text = element_text(hjust = 0)
  ) +
  scale_color_viridis_d(end = 0.85)
ggsave("figures/fig3.jpeg", plot, height = 7, width = 8)

