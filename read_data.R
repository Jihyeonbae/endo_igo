# this is at the IGO level
shape_location_data_1030 <- readRDS("~/Downloads/shape_location_data_1030.rds")
# this is at the country level 
country_year_entropy <- readRDS("~/Downloads/country_year_entropy.rds")

country_portfolio_data_Nov3 <- readRDS("~/Desktop/IGOvariance/data/new/country_portfolio_data_Nov3.rds")

# main output of interest:  IGO democracy beta mean (mu_mean)
analysis_df <- shape_location_data_1030 %>%
  arrange(ioname, year) %>%
  group_by(ioname)

analysis_df <- analysis_df %>%
  mutate(
    lag_mu = lag(mu_mean)
  )

analysis_df <- shape_location_data_1030 %>%
  arrange(ioname, year) %>%
  group_by(ioname) %>%
  mutate(
    lag_quadrant = lag(shape_quadrant),
    quadrant_cross = shape_quadrant != lag_quadrant & !is.na(lag_quadrant),
    delta_mu = mu_mean - lag(mu_mean)
  ) %>%
  ungroup()

analysis_df <- analysis_df %>%
  mutate(
    crossing_direction = case_when(
      quadrant_cross & delta_mu > 0 ~ "Toward democracy",
      quadrant_cross & delta_mu < 0 ~ "Toward autocracy",
      TRUE ~ "No crossing"
    )
  )


plot1 <- ggplot() +
  # No crossing first (background)
  geom_jitter(
    data = analysis_df %>% filter(crossing_direction == "No crossing"),
    aes(x = year, y = delta_mu),
    color = "#BDBDBD",
    alpha = 0.25,
    size = 0.8,
    width = 0.25
  ) +
  # Toward democracy
  geom_jitter(
    data = analysis_df %>% filter(crossing_direction == "Toward democracy"),
    aes(x = year, y = delta_mu),
    color = "#009E73",
    alpha = 0.8,
    size = 1.6,
    width = 0.25
  ) +
  # Toward autocracy
  geom_jitter(
    data = analysis_df %>% filter(crossing_direction == "Toward autocracy"),
    aes(x = year, y = delta_mu),
    color = "#CC79A7",
    alpha = 0.8,
    size = 1.6,
    width = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    x = "Year",
    y = "Year-to-year change in IGO democracy mean (Δμ)",
    title = "IGO regime orientation changes persist over time"
  )


ggsave(
  filename = "~/Desktop/endo_igo/overleaf/igo_regime_orientation_changes.png",
  plot = plot1,
  width = 8.5,      # inches (fits \textwidth nicely)
  height = 4.5,     # inches
  dpi = 300,
  units = "in",
  bg = "white"
)

baseline_autoc <- analysis_df %>%
  group_by(ioname) %>%
  slice_min(year, with_ties = FALSE) %>%
  filter(location == "Authoritarian location") %>%
  select(ioname)


end_labels <- analysis_df %>%
  semi_join(baseline_autoc, by = "ioname") %>%
  group_by(ioname) %>%
  filter(year == max(year)) %>%
  ungroup()


plot2 <- ggplot(
  analysis_df %>% semi_join(baseline_autoc, by = "ioname"),
  aes(x = year, y = mu_mean, group = ioname)
) +
  geom_line(
    color = "#009E73",
    alpha = 0.85,
    linewidth = 0.6
  ) +
  
  geom_text_repel(
    data = end_labels,
    aes(label = ioname, color = location),
    size = 3,
    family = "Times New Roman",
    nudge_x = 2,
    direction = "y",
    segment.size = 0.25,
    segment.alpha = 0.6,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(
    values = c(
      "Authoritarian location" = "red",  # color-blind safe red
      "Mixed location"         = "#7F7F7F",
      "Democratic location"    = "#0072B2"   # color-blind safe blue
    ),
    name = "Final regime orientation"
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    x = "Year",
    y = "IGO democracy beta mean (μ)",
    title = "Trajectories of Initially Authoritarian IGOs Over Time",
    subtitle = "Line paths show organizational drift; labels indicate final mean "
  ) +
  
  theme_classic(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10),
    
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    
    panel.border = element_blank()
  ) +
  
  coord_cartesian(clip = "off")


ggsave(
  filename = "~/Desktop/endo_igo/overleaf/aigo_trajectory.png",
  plot = plot2,
  width = 8.5,      # inches (fits \textwidth nicely)
  height = 4.5,     # inches
  dpi = 300,
  units = "in",
  bg = "white"
)




baseline_democ <- analysis_df %>%
  group_by(ioname) %>%
  slice_min(year, with_ties = FALSE) %>%
  filter(location == "Democratic location") %>%
  select(ioname)


end_labels <- analysis_df %>%
  semi_join(baseline_democ, by = "ioname") %>%
  group_by(ioname) %>%
  filter(year == max(year)) %>%
  ungroup()


plot3 <- ggplot(
  analysis_df %>% semi_join(baseline_democ, by = "ioname"),
  aes(x = year, y = mu_mean, group = ioname)
) +
  geom_line(
    color = "#009E73",
    alpha = 0.85,
    linewidth = 0.6
  ) +
  
  geom_text_repel(
    data = end_labels,
    aes(label = ioname, color = location),
    size = 3,
    family = "Times New Roman",
    nudge_x = 2,
    direction = "y",
    segment.size = 0.25,
    segment.alpha = 0.6,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(
    values = c(
      "Authoritarian location" = "red",  # color-blind safe red
      "Mixed location"         = "#7F7F7F",
      "Democratic location"    = "#0072B2"   # color-blind safe blue
    ),
    name = "Final regime orientation"
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    x = "Year",
    y = "IGO democracy beta mean (μ)",
    title = "Trajectories of Initially Democratic IGOs Over Time",
    subtitle = "Line paths show organizational drift; labels indicate final mean "
  ) +
  
  theme_classic(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10),
    
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    
    panel.border = element_blank()
  ) +
  
  coord_cartesian(clip = "off")


ggsave(
  filename = "~/Desktop/endo_igo/overleaf/digo_trajectory.png",
  plot = plot3,
  width = 8.5,      # inches (fits \textwidth nicely)
  height = 4.5,     # inches
  dpi = 300,
  units = "in",
  bg = "white"
)

#Given where the IGO was last year, which processes predict its updated level this year?

#1) incumbent pressure.  What regime orientation do existing members bring into year t?
incumbent_df <- country_portfolio_data_Nov3 %>%
  arrange(ioname, ccode, year) %>%
  group_by(ioname, ccode) %>%
  mutate(
    member_change = v2x_polyarchy - lag(v2x_polyarchy)
  )

endo_change <- incumbent_df %>%
  group_by(ioname, year) %>%
  summarise(
    incumbent_pressure = mean(member_change, na.rm = TRUE),
    incumbent_dispersion = sd(member_change, na.rm = TRUE),
    .groups = "drop"
  )

analysis_df <- analysis_df %>%
  left_join(endo_change, by = c("ioname", "year"))




#2) new members' pressure.

entry_df <- country_portfolio_data_Nov3 %>%
  arrange(ioname, ccode, year) %>%
  group_by(ioname, ccode) %>%
  mutate(
    new_member = if_else(
      is.na(lag(year)) | year - lag(year) > 1,
      1L, 0L
    )
  )


new_member_change <- entry_df %>%
  group_by(ioname, year) %>%
  summarise(
    n_new_members = sum(new_member, na.rm = TRUE),
    # proportion of membership that is new. this measures how big the entry event is, relative to the organization. 
    entry_rate    = n_new_members / n(),
    .groups = "drop"
  )

entry_comp <- entry_df %>%
  filter(new_member == 1) %>%
  group_by(ioname, year) %>%
  summarise(
    entrant_mu = mean(v2x_polyarchy, na.rm = TRUE),
    entrant_autoc = mean(autoc_pct_post, na.rm = TRUE),
    .groups = "drop"
  )


analysis_df <- analysis_df %>%
  ungroup() %>%
  left_join(new_member_change, by = c("ioname", "year")) %>%
  left_join(entry_comp,        by = c("ioname", "year"))

# are the new members more democratic/autocratic than members at t-1?
# entry_direction tells us which way entrants push; entry_pressure tells you how hard they push.
analysis_df <- analysis_df %>%
  mutate(
    entry_direction = entrant_mu - lag_mu, #ideological distance between new entrants and the organization’s prior composition
    entry_pressure = entry_rate * entry_direction #size-weighted external shock through entry event. 
    )



analysis_df <- analysis_df %>%
  mutate(
    incumbent_active = abs(incumbent_pressure) > 0.001,
    entry_event = entry_rate > 0
  )

analysis_df %>%
  summarise(
    pct_incumbent_change = mean(incumbent_active, na.rm = TRUE),
    pct_entry_event = mean(entry_event, na.rm = TRUE)
  )

analysis_df <- analysis_df %>%
  mutate(
    endogenous_cross = quadrant_cross & incumbent_active & !entry_event
  )

analysis_df <- analysis_df %>%
  mutate(
    transition_type = case_when(
      endogenous_cross &
        lag_quadrant %in% c("Autocracy-skewed", "Democracy-skewed") &
        shape_quadrant == "Unimodal"
      ~ "Dominance erosion (skew → unimodal)",
      TRUE ~ NA_character_
    )
  )


analysis_df %>%
  filter(!is.na(transition_type)) %>%
  count(transition_type) %>%
  mutate(share = n / sum(n))

# regression
bayes0 <- brm(
  mu_mean ~ incumbent_pressure + entry_pressure + year + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

bayes1 <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entry_pressure + year + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

bayes0 <- brm(
  mu_mean ~ incumbent_pressure + entry_pressure + year + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

bayes1 <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entry_pressure + year + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

bayes2 <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entry_pressure + phi_mean + n_founders + year + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

bayes2_fe <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entry_pressure + phi_mean + n_founders +
    factor(year) + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

library(modelsummary)

coef_plot <- modelplot(
  bayes2_fe,
  coef_omit = "Intercept|factoryear",
  conf_level = 0.95
) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  theme_classic(base_family = "Times New Roman") +
  labs(
    x = "Posterior estimate",
    y = NULL,
    title = "Drivers of IGO Regime Orientation",
    subtitle = "Points show posterior means; bars indicate 95% credible intervals"
  )

coef_plot

library(haven)
MIA <- read_dta("MIA-V/DP_March 2023.dta")

mia_attached <- analysis_df %>%
  left_join(MIA, by=c("ioname","year"))


bayes3 <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entry_pressure + poolaccess + delaccess + phi_mean +n_founders + year + (1 | ioname),
  data = mia_attached,
  family = gaussian()
)

bayes3_fe <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entry_pressure +
    poolaccess + delaccess + phi_mean + n_founders +
    factor(year) + (1 | ioname),
  data = mia_attached,
  family = gaussian()
)

bayes4 <- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entrant_mu + poolaccess + delaccess + phi_mean +n_founders + year + (1 | ioname),
  data = mia_attached,
  family = gaussian()
)

bayes4_fe<- brm(
  mu_mean ~ lag_mu + incumbent_pressure + entrant_mu + phi_mean +n_founders + factor(year) + (1 | ioname),
  data = analysis_df,
  family = gaussian()
)

library(modelsummary)

modelplot(
  bayes3,
  coef_omit = "Intercept",
  conf_level = 0.95
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic()
