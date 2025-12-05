shape_location_data_1030 <- readRDS("~/Downloads/shape_location_data_1030.rds")
country_year_entropy <- readRDS("~/Downloads/country_year_entropy.rds")
country_portfolio_data_Nov3 <- readRDS("~/Downloads/country_portfolio_data_Nov3.rds")

portfolio <- country_portfolio_data_Nov3 %>%
  arrange(ioname, ccode, year) %>%
  group_by(ioname, ccode) %>%
  mutate(
    regime_change = v2x_polyarchy - v2x_polyarchy_lag
  )

endo_change <- portfolio %>%
  group_by(ioname, year) %>%
  summarise(
    mean_regime_change = mean(regime_change, na.rm = TRUE),
    sd_regime_change = sd(regime_change, na.rm = TRUE),
    n_members = n()
  )

portfolio <- portfolio %>%
  group_by(ioname, ccode) %>%
  mutate(
    new_member = ifelse(is.na(lag(year)) | year - lag(year) > 1, 1, 0)
  )

entry_effects <- portfolio %>%
  filter(new_member == 1) %>%
  group_by(ioname, year) %>%
  summarise(
    entrant_regime_mean = mean(v2x_polyarchy, na.rm = TRUE),
    entrant_autoc_mean = mean(autoc_pct_post_lag, na.rm = TRUE),
    entrant_num = n()
  )

full <- shape_location_data_1030 %>%
  left_join(endo_change, by = c("ioname", "year")) %>%
  left_join(entry_effects, by = c("ioname", "year"))


ggplot(full, aes(entrant_regime_mean, autoc_perc)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal()

