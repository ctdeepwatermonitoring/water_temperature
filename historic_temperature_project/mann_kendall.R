# Loading libraries
library(tidyverse)
library(broom)
library(ggforce)
library(Kendall)
library(patchwork)
library(scales)
library(EnvStats)

# --------- Helpers and constants ----------
# thresholds and mapping
COLD_MAX = 18.29
WARM_MIN = 21.70

THRESHOLD_LINES_LEGEND = tibble(
  yintercept = c(COLD_MAX, WARM_MIN),
  line_type   = c(sprintf("Cold threshold (%.2f°C)", COLD_MAX),
                  sprintf("Warm threshold (%.2f°C)", WARM_MIN))
)

THRESHOLD_COLORS = setNames(
  c("mediumturquoise", "indianred1"),
  c(sprintf("Cold threshold (%.2f°C)", COLD_MAX), sprintf("Warm threshold (%.2f°C)", WARM_MIN))
)
THRESHOLD_LINETYPES = setNames(
  c("solid", "solid"),
  c(sprintf("Cold threshold (%.2f°C)", COLD_MAX), sprintf("Warm threshold (%.2f°C)", WARM_MIN))
)

#Clean sites
clean_sites = function(sites_df) {
  sites_df %>%
    mutate(STA_SEQ = as.character(STA_SEQ)) %>%
    select(STA_SEQ, WaterbodyName)
}

# reusable theil-sen
theil_sen = function(y, x) {
  y = as.numeric(y); x = as.numeric(x)
  if (length(na.omit(y)) < 2 || length(na.omit(x)) < 2) return(NA_real_)
  num = outer(y, y, "-")
  den = outer(x, x, "-")
  slopes = num[upper.tri(num)] / den[upper.tri(den)]
  median(slopes, na.rm = TRUE)
}

#label builder
build_mk_label = function(df) {
  tau = mean(df$tau, na.rm = TRUE)
  slope = mean(df$theil_sen, na.rm = TRUE)
  pval = mean(df$p_value, na.rm = TRUE)
  paste0("τ = ", sprintf("%.2f", tau),
         " | Slope = ", sprintf("%.3f", slope), " °C/yr",
         " | p = ", sprintf("%.3f", pval))
}

# --------- End of Helpers and constants ----------

#Read in the data
initial_data = read_csv("/home/deepuser/ContDataQC/historic_temperature_project/temperature.csv")
sites = read_csv("/home/deepuser/ContDataQC/historic_temperature_project/awx_stations_webservice(stations).csv")
landscape_cover = read_csv("/home/deepuser/ContDataQC/historic_temperature_project/chloride_mmi_lc_2003_2020.csv")

#Clean the data
sites_clean = clean_sites(sites)

#Prepare daily means and join site names
daily_means = initial_data %>%
  mutate(
    date = as_date(mDateTime),
    year = year(date),
    month = month(date),
    staSeq = as.character(staSeq)
  ) %>%
  group_by(staSeq, date, year, month) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop") %>%
  #filter(staSeq %in% landscape_cover$staSeq) %>%
  left_join(
    sites_clean %>%
      select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  ) %>%
  mutate(
    staSeq_waterbodyName = paste(staSeq, "-", WaterbodyName)
  )


#Mann-Kendall trend test
temp_stat = "median"

temp_label = ifelse(temp_stat == "mean", "Mean", "Median")

#June-August temperature
summer_avg = daily_means %>%
  filter(month(date) %in% 6:8) %>%
  group_by(staSeq, year) %>%
  summarise(
    median_temp = median(mean_temp, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE), 
    .groups = "drop"
    ) %>%
  mutate(temp_value = if (temp_stat == "mean") mean_temp else median_temp)

mk_results_base = summer_avg %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  summarise(
    n_years = n_distinct(year),
    kendall = list(cor.test(year, temp_value, alternative = "two.sided", method = "kendall", continuity = TRUE)),
    tau = kendall[[1]]$estimate,
    p_value = kendall[[1]]$p.value,
    theil_sen = theil_sen(temp_value, year)
  ) %>%
  ungroup()

# Adding waterbody names
mk_results = mk_results_base %>%
  left_join(
    sites_clean %>%
      select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  )

# Join averages
mk_results = summer_avg %>%
  filter(staSeq %in% mk_results$staSeq) %>%
  left_join(mk_results %>% select(staSeq, WaterbodyName, tau, p_value, theil_sen), by = "staSeq") %>%
  group_by(staSeq) %>%
  mutate(intercept = median(temp_value) - theil_sen * median(year)) %>%
  ungroup()

# Label data
label_data = mk_results %>%
  group_by(staSeq) %>%
  summarise(label = build_mk_label(cur_data_all()))

#Summarized data
mk_results_summary = mk_results %>%
  group_by(staSeq, WaterbodyName) %>%
  summarise(
    n_year = n_distinct(year),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_avg_temp = min(temp_value, na.rm = TRUE),
    max_avg_temp = max(temp_value, na.rm = TRUE),
    tau = first(na.omit(tau)),
    p_value = first(na.omit(p_value)),
    theil_sen = first(na.omit(theil_sen)),
    intercept = first(na.omit(intercept)),
    .groups = "drop"
  )

# Join labels
mk_results_labeled = mk_results %>%
  left_join(label_data, by = "staSeq") %>%
  mutate(
    facet_label = paste0(staSeq, " - ", WaterbodyName, "\n", label)
  )

# Temperature zones
COLD_MAX = 18.29
WARM_MIN = 21.70

# Split data by facet
mk_site_list = split(mk_results_labeled, mk_results_labeled$facet_label)
mk_plots = list()
for (facet in names(mk_site_list)) {
  df = mk_site_list[[facet]]
  min_temp = min(df$temp_value, na.rm = TRUE)
  max_temp = max(df$temp_value, na.rm = TRUE)
  y_min = min_temp - 1.5
  y_max = max_temp + 1.5
  
  # Determine which threshold lines to draw
  threshold_lines = tibble(
    yintercept = c(COLD_MAX, WARM_MIN),
    line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
  ) %>%
    filter(
      (line_type == "Cold threshold (18.29°C)" & y_min <= COLD_MAX & COLD_MAX <= y_max) |
        (line_type == "Warm threshold (21.70°C)" & y_min <= WARM_MIN & WARM_MIN <= y_max)
    )
  # If no threshold lines would be drawn, always include the cold threshold line
  if (nrow(threshold_lines) == 0) {
    threshold_lines = tibble(
      yintercept = COLD_MAX,
      line_type = "Cold threshold (18.29°C)"
    )
  }
  
  p = ggplot(df, aes(x = year, y = temp_value)) +
    geom_hline(
      data = threshold_lines,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      show.legend = TRUE
    ) +
    geom_hline(
      data = THRESHOLD_LINES_LEGEND,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      alpha = 0,
      show.legend = TRUE
    ) +
    geom_point(color = "gray20") +
    geom_line(color = "gray50") +
    geom_abline(aes(intercept = intercept, slope = theil_sen),
                size = 1.2, show.legend = FALSE) +
    labs(
      title = facet,
      x = "Year",
      y = paste0(temp_label, " Summer Temperature (°C)"),
      color = "Temperature thresholds",
      linetype = "Temperature thresholds"
    ) +
    scale_color_manual(
      values = c("Cold threshold (18.29°C)" = "mediumturquoise", "Warm threshold (21.70°C)" = "indianred1")
    ) +
    scale_linetype_manual(
      values = c("Cold threshold (18.29°C)" = "solid", "Warm threshold (21.70°C)" = "solid")
    ) +
    scale_y_continuous(limits = c(y_min, y_max), oob = rescale_none) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      panel.grid.minor = element_blank()
    )
  mk_plots[[facet]] = p
}

plots_per_page = 9
n_pages = ceiling(length(mk_plots) / plots_per_page)

page_plots = vector("list", n_pages)

for (i in seq_len(n_pages)) {
  idx = ((i - 1) * plots_per_page + 1):(min(i * plots_per_page, length(mk_plots)))
  
  page_plots[[i]] = wrap_plots(mk_plots[idx], ncol = 3, nrow = 3, guides = "collect") +
    plot_annotation(
      title = paste0("Summer Temperature Trends (", temp_label, ") – Theil-Sen Slopes (Page ", i, " of ", n_pages, ")")
    ) &
    theme(legend.position = "bottom")
}
page_plots[[1]]
page_plots[[2]]
page_plots[[3]]
page_plots[[4]]
page_plots[[5]]
page_plots[[6]]
page_plots[[7]]
page_plots[[8]]






#Seasonal Mann-Kendall trend test for summer months
temp_stat = "median"

temp_label = ifelse(temp_stat == "mean", "Mean", "Median")

summer_months = daily_means %>%
  filter(month %in% 6:8) %>%
  group_by(staSeq, year, month) %>%
  summarise(
    median_temp = median(mean_temp, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  mutate(temp_value = if (temp_stat == "mean") mean_temp else median_temp)

seasonal_mk_envstats = function(df) {
  if (n_distinct(df$year) < 10) {
    return(tibble(tau = NA, p_value = NA, theil_sen = NA))
  }
  
  res = EnvStats::kendallSeasonalTrendTest(
    y = df$temp_value,
    year = df$year,
    season = df$month
  )
  
  tibble(
    tau = res$estimate["tau"],
    p_value = res$p.value["z (Trend)"],
    theil_sen = res$estimate["slope"]
  )
}

smk_results = summer_months %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  group_modify(~ seasonal_mk_envstats(.x)) %>%
  ungroup() %>%
  left_join(
    sites_clean %>% select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  )

smk_results_sig = smk_results %>%
  filter(p_value < 0.1)

smk_results = summer_months %>%
  filter(staSeq %in% (smk_results %>% pull(staSeq))) %>%
  left_join(smk_results %>% select(staSeq, WaterbodyName, tau, p_value, theil_sen), by = "staSeq") %>%
  group_by(staSeq) %>%
  mutate(intercept = median(temp_value) - theil_sen * median(year)) %>%
  ungroup()

smk_results_summary = smk_results %>%
  group_by(staSeq, WaterbodyName) %>%
  summarise(
    n_year = n_distinct(year),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_avg_temp = min(temp_value, na.rm = TRUE),
    max_avg_temp = max(temp_value, na.rm = TRUE),
    tau = first(na.omit(tau)),
    p_value = first(na.omit(p_value)),
    theil_sen = first(na.omit(theil_sen)),
    intercept = first(na.omit(intercept)),
    .groups = "drop"
  )

label_data = smk_results %>%
  group_by(staSeq) %>%
  summarise(label = build_mk_label(cur_data_all()))

smk_results_labeled = smk_results %>%
  left_join(label_data, by = "staSeq") %>%
  mutate(
    facet_label = paste0(staSeq, " - ", WaterbodyName, "\n", label)
  )

THRESHOLD_LINES_LEGEND = tibble(
  yintercept = c(COLD_MAX, WARM_MIN),
  line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
)

smk_site_list = split(smk_results_labeled, smk_results_labeled$facet_label)
smk_plots = list()

for (facet in names(smk_site_list)) {
  df = smk_site_list[[facet]]
  min_temp = min(df$temp_value, na.rm = TRUE)
  max_temp = max(df$temp_value, na.rm = TRUE)
  y_min = min_temp - 1.5
  y_max = max_temp + 1.6
  
  threshold_lines = tibble(
    yintercept = c(COLD_MAX, WARM_MIN),
    line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
  ) %>%
    filter(
      (line_type == "Cold threshold (18.29°C)" & y_min <= COLD_MAX & COLD_MAX <= y_max) |
        (line_type == "Warm threshold (21.70°C)" & y_min <= WARM_MIN & WARM_MIN <= y_max)
    )
  
  if (nrow(threshold_lines) == 0) {
    threshold_lines = tibble(
      yintercept = COLD_MAX,
      line_type = "Cold threshold (18.29°C)"
    )
  }
  
  p = ggplot(df, aes(x = year, y = temp_value)) +
    geom_hline(
      data = threshold_lines,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      show.legend = TRUE
    ) +
    geom_hline(
      data = THRESHOLD_LINES_LEGEND,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      alpha = 0,
      show.legend = TRUE
    ) +
    geom_point(color = "gray20") +
    geom_abline(aes(intercept = intercept, slope = theil_sen),
                size = 1.2, show.legend = FALSE) +
    labs(
      title = facet,
      x = "Year",
      y = paste0(temp_label, " Summer Temperature (°C)"),
      color = "Temperature thresholds",
      linetype = "Temperature thresholds"
    ) +
    scale_color_manual(
      values = c("Cold threshold (18.29°C)" = "mediumturquoise", "Warm threshold (21.70°C)" = "indianred1")
    ) +
    scale_linetype_manual(
      values = c("Cold threshold (18.29°C)" = "solid", "Warm threshold (21.70°C)" = "solid")
    ) +
    scale_y_continuous(limits = c(y_min, y_max), oob = rescale_none) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      panel.grid.minor = element_blank()
    )
  
  smk_plots[[facet]] = p
}

plots_per_page = 9
n_pages = ceiling(length(smk_plots) / plots_per_page)

page_plots = vector("list", n_pages)

for (i in seq_len(n_pages)) {
  idx = ((i - 1) * plots_per_page + 1):(min(i * plots_per_page, length(smk_plots)))
  
  page_plots[[i]] = wrap_plots(smk_plots[idx], ncol = 3, nrow = 3, guides = "collect") +
    plot_annotation(
      title = paste0("Seasonal Summer Temperature Trends (", temp_label, ") – Theil-Sen Slopes (Page ", i, " of ", n_pages, ")")
    ) &
    theme(legend.position = "bottom")
}
page_plots[[1]]
page_plots[[2]]
page_plots[[3]]
page_plots[[4]]
page_plots[[5]]
page_plots[[6]]
page_plots[[7]]
page_plots[[8]]






#Seasonal Mann-Kendall trend test for all seasons
temp_stat = "median"

temp_label = ifelse(temp_stat == "mean", "Mean", "Median")

seasoned_month = function(month) {
  case_when(
    month %in% c(12, 1, 2)  ~ "DJF",
    month %in% c(3, 4, 5)   ~ "MAM",
    month %in% c(6, 7, 8)   ~ "JJA",
    month %in% c(9, 10, 11) ~ "SON",
    TRUE ~ NA_character_
  )
}

seasonal_monthly = daily_means %>%
  mutate(season = seasoned_month(month(date))) %>%
  filter(!is.na(season)) %>%
  group_by(staSeq, year, season) %>%
  summarise(
    median_temp = median(mean_temp, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  mutate(temp_value = if (temp_stat == "mean") mean_temp else median_temp)


#Exploring seasonal data gaps
season_years = seasonal_monthly %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup() %>%
  group_by(staSeq, season) %>%
  summarise(n_years = n_distinct(year), .groups = "drop")

season_years_wide = season_years %>%
  pivot_wider(names_from = season, values_from = n_years, values_fill = 0)

season_years %>%
  ggplot(aes(x = season, y = staSeq, fill = n_years)) +
  geom_tile() +
  geom_text(aes(label = n_years), color = "white") +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Distinct Years per Season per Site", fill = "Years")


seasonal_mk_envstats_seasons = function(df) {
  if (n_distinct(df$year) < 10) {
    return(tibble(tau = NA, p_value = NA, theil_sen = NA))
  }
  
  res = EnvStats::kendallSeasonalTrendTest(
    y = df$temp_value,
    year = df$year,
    season = df$season
  )
  
  tibble(
    tau = res$estimate["tau"],
    p_value = res$p.value["z (Trend)"],
    theil_sen = res$estimate["slope"]
  )
}

smk_seasonal_results = seasonal_monthly %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  group_modify(~ seasonal_mk_envstats_seasons(.x)) %>%
  ungroup() %>%
  left_join(
    sites_clean %>% select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  )

smk_seasonal_sig = smk_seasonal_results %>% filter(p_value < 0.1)

smk_seasonal_plot_df = seasonal_monthly %>%
  filter(staSeq %in% (smk_seasonal_results %>% pull(staSeq))) %>%
  left_join(smk_seasonal_results %>% select(staSeq, WaterbodyName, tau, p_value, theil_sen), by = "staSeq") %>%
  group_by(staSeq) %>%
  mutate(intercept = median(temp_value, na.rm = TRUE) - theil_sen * median(year, na.rm = TRUE)) %>%
  ungroup()

smk_seasonal_results_summary = smk_seasonal_plot_df %>%
  group_by(staSeq, WaterbodyName) %>%
  summarise(
    n_year = n_distinct(year),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_avg_temp = min(temp_value, na.rm = TRUE),
    max_avg_temp = max(temp_value, na.rm = TRUE),
    tau = first(na.omit(tau)),
    p_value = first(na.omit(p_value)),
    theil_sen = first(na.omit(theil_sen)),
    intercept = first(na.omit(intercept)),
    .groups = "drop"
  )

season_label_data = smk_seasonal_plot_df %>%
  group_by(staSeq) %>%
  summarise(label = build_mk_label(cur_data_all()))

smk_seasonal_plot_labeled = smk_seasonal_plot_df %>%
  left_join(season_label_data, by = "staSeq") %>%
  mutate(facet_label = paste0(staSeq, " - ", WaterbodyName, "\n", label))

legend_lines = tibble(
  yintercept = c(COLD_MAX, WARM_MIN),
  line_type = c(sprintf("Cold threshold (%.2f°C)", COLD_MAX), sprintf("Warm threshold (%.2f°C)", WARM_MIN))
)

smk_seasonal_site_list = split(smk_seasonal_plot_labeled, smk_seasonal_plot_labeled$facet_label)
smk_seasonal_plots = list()

for (facet in names(smk_seasonal_site_list)) {
  df = smk_seasonal_site_list[[facet]]
  min_temp = min(df$temp_value, na.rm = TRUE)
  max_temp = max(df$temp_value, na.rm = TRUE)
  y_min = min_temp - 1.5
  y_max = max_temp + 1.6
  
  threshold_lines = tibble(
    yintercept = c(COLD_MAX, WARM_MIN),
    line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
  ) %>%
    filter(
      (line_type == "Cold threshold (18.29°C)" & y_min <= COLD_MAX & COLD_MAX <= y_max) |
        (line_type == "Warm threshold (21.70°C)" & y_min <= WARM_MIN & WARM_MIN <= y_max)
    )
  
  if (nrow(threshold_lines) == 0) {
    threshold_lines = tibble(
      yintercept = COLD_MAX,
      line_type = "Cold threshold (18.29°C)"
    )
  }
  
  p = ggplot(df, aes(x = year, y = temp_value)) +
    geom_hline(
      data = threshold_lines,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      show.legend = TRUE
    ) +
    geom_hline(
      data = THRESHOLD_LINES_LEGEND,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      alpha = 0,
      show.legend = TRUE
    ) +
    geom_point(color = "gray20") +
    geom_abline(aes(intercept = intercept, slope = theil_sen),
                size = 1.2, show.legend = FALSE) +
    labs(
      title = facet,
      x = "Year",
      y = paste(temp_label, "Season Temperature (°C)"),
      color = "Temperature thresholds",
      linetype = "Temperature thresholds"
    ) +
    scale_color_manual(
      values = c("Cold threshold (18.29°C)" = "mediumturquoise", "Warm threshold (21.70°C)" = "indianred1")
    ) +
    scale_linetype_manual(
      values = c("Cold threshold (18.29°C)" = "solid", "Warm threshold (21.70°C)" = "solid")
    ) +
    scale_y_continuous(limits = c(y_min, y_max), oob = rescale_none) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      panel.grid.minor = element_blank()
    )
  
  smk_seasonal_plots[[facet]] = p
}

plots_per_page = 9
n_pages = ceiling(length(smk_seasonal_plots) / plots_per_page)

page_plots = vector("list", n_pages)

for (i in seq_len(n_pages)) {
  idx = ((i - 1) * plots_per_page + 1):(min(i * plots_per_page, length(smk_seasonal_plots)))
  
  page_plots[[i]] = wrap_plots(smk_seasonal_plots[idx], ncol = 3, nrow = 3, guides = "collect") +
    plot_annotation(
      title = paste0("Seasonal Temperature Trends (", temp_label, ") – Theil-Sen Slopes (Page ", i, " of ", n_pages, ")")
    ) &
    theme(legend.position = "bottom")
}
page_plots[[1]]
page_plots[[2]]
page_plots[[3]]
page_plots[[4]]
page_plots[[5]]
page_plots[[6]]
page_plots[[7]]
page_plots[[8]]
page_plots[[9]]







#Seasonal Mann-Kendall trend test for bioperiods
temp_stat = "median"

temp_label = ifelse(temp_stat == "mean", "Mean", "Median")

bioperiod_from_date = function(date) {
  md = month(date) * 100 + day(date)
  case_when(
    md >= 1201 | md <= 228 ~ "Overwinter",
    md >= 301 & md <= 430 ~ "Habitat Forming",
    md >= 501 & md <= 531 ~ "Clupeid Spawning",
    md >= 601 & md <= 630 ~ "Resident Spawning",
    md >= 701 & md <= 1031 ~ "Rearing & Growth",
    md >= 1101 & md <= 1130 ~ "Salmonid Spawning",
    TRUE ~ NA_character_
  )
}

seasonal_bioperiod = daily_means %>%
  mutate(bioperiod = bioperiod_from_date(date)) %>%
  filter(!is.na(bioperiod)) %>%
  group_by(staSeq, year, bioperiod) %>%
  summarise(
    median_temp = median(mean_temp, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(temp_value = if (temp_stat == "mean") mean_temp else median_temp)

#Exploring gaps in bioperiod
bioperiod_years = seasonal_bioperiod %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup() %>%
  group_by(staSeq, bioperiod) %>%
  summarise(n_years = n_distinct(year), .groups = "drop")

bioperiod_years_wide = bioperiod_years %>%
  pivot_wider(names_from = bioperiod, values_from = n_years, values_fill = 0)

bioperiod_years %>%
  ggplot(aes(x = bioperiod, y = staSeq, fill = n_years)) +
  geom_tile() +
  geom_text(aes(label = n_years), color = "white") +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Distinct Years per Season per Site", fill = "Years")

seasonal_mk_envstats_bioperiod = function(df) {
  if (n_distinct(df$year) < 10) {
    return(tibble(tau = NA, p_value = NA, theil_sen = NA))
  }
  
  res = EnvStats::kendallSeasonalTrendTest(
    y = df$temp_value,
    year = df$year,
    season = df$bioperiod
  )
  
  tibble(
    tau = res$estimate["tau"],
    p_value = res$p.value["z (Trend)"],
    theil_sen = res$estimate["slope"]
  )
}

smk_bioperiod_results = seasonal_bioperiod %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  group_modify(~ seasonal_mk_envstats_bioperiod(.x)) %>%
  ungroup() %>%
  left_join(
    sites_clean %>% select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  )

smk_bioperiod_sig = smk_bioperiod_results %>% filter(p_value < 0.1)

smk_bioperiod_plot_df = seasonal_bioperiod %>%
  filter(staSeq %in% (smk_bioperiod_results %>% pull(staSeq))) %>%
  left_join(smk_bioperiod_results %>% select(staSeq, WaterbodyName, tau, p_value, theil_sen), by = "staSeq") %>%
  group_by(staSeq) %>%
  mutate(intercept = median(temp_value, na.rm = TRUE) - theil_sen * median(year, na.rm = TRUE)) %>%
  ungroup()

smk_bioperiod_results_summary = smk_bioperiod_plot_df %>%
  group_by(staSeq, WaterbodyName) %>%
  summarise(
    n_year = n_distinct(year),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_avg_temp = min(temp_value, na.rm = TRUE),
    max_avg_temp = max(temp_value, na.rm = TRUE),
    tau = first(na.omit(tau)),
    p_value = first(na.omit(p_value)),
    theil_sen = first(na.omit(theil_sen)),
    intercept = first(na.omit(intercept)),
    .groups = "drop"
  )

bioperiod_label_data = smk_bioperiod_plot_df %>%
  group_by(staSeq) %>%
  summarise(label = build_mk_label(cur_data_all()))

smk_bioperiod_plot_labeled = smk_bioperiod_plot_df %>%
  left_join(bioperiod_label_data, by = "staSeq") %>%
  mutate(facet_label = paste0(staSeq, " - ", WaterbodyName, "\n", label))

legend_lines = tibble(
  yintercept = c(COLD_MAX, WARM_MIN),
  line_type = c(sprintf("Cold threshold (%.2f°C)", COLD_MAX), sprintf("Warm threshold (%.2f°C)", WARM_MIN))
)

smk_bioperiod_site_list = split(smk_bioperiod_plot_labeled, smk_bioperiod_plot_labeled$facet_label)
smk_bioperiod_plots = list()

for (facet in names(smk_bioperiod_site_list)) {
  df = smk_bioperiod_site_list[[facet]]
  min_temp = min(df$temp_value, na.rm = TRUE)
  max_temp = max(df$temp_value, na.rm = TRUE)
  y_min = min_temp - 1.5
  y_max = max_temp + 1.6
  
  threshold_lines = tibble(
    yintercept = c(COLD_MAX, WARM_MIN),
    line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
  ) %>%
    filter(
      (line_type == "Cold threshold (18.29°C)" & y_min <= COLD_MAX & COLD_MAX <= y_max) |
        (line_type == "Warm threshold (21.70°C)" & y_min <= WARM_MIN & WARM_MIN <= y_max)
    )
  
  if (nrow(threshold_lines) == 0) {
    threshold_lines = tibble(
      yintercept = COLD_MAX,
      line_type = "Cold threshold (18.29°C)"
    )
  }
  
  p = ggplot(df, aes(x = year, y = temp_value)) +
    geom_hline(
      data = threshold_lines,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      show.legend = TRUE
    ) +
    geom_hline(
      data = THRESHOLD_LINES_LEGEND,
      aes(yintercept = yintercept, color = line_type, linetype = line_type),
      size = 1,
      alpha = 0,
      show.legend = TRUE
    ) +
    geom_point(color = "gray20") +
    geom_abline(aes(intercept = intercept, slope = theil_sen),
                size = 1.2, show.legend = FALSE) +
    labs(
      title = facet,
      x = "Year",
      y = paste(temp_label, "Bioperiod Temperature (°C)"),
      color = "Temperature thresholds",
      linetype = "Temperature thresholds"
    ) +
    scale_color_manual(
      values = c("Cold threshold (18.29°C)" = "mediumturquoise", "Warm threshold (21.70°C)" = "indianred1")
    ) +
    scale_linetype_manual(
      values = c("Cold threshold (18.29°C)" = "solid", "Warm threshold (21.70°C)" = "solid")
    ) +
    scale_y_continuous(limits = c(y_min, y_max), oob = rescale_none) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      panel.grid.minor = element_blank()
    )
  
  smk_bioperiod_plots[[facet]] = p
}

plots_per_page = 9
n_pages = ceiling(length(smk_bioperiod_plots) / plots_per_page)

page_plots = vector("list", n_pages)

for (i in seq_len(n_pages)) {
  idx = ((i - 1) * plots_per_page + 1):(min(i * plots_per_page, length(smk_bioperiod_plots)))
  
  page_plots[[i]] = wrap_plots(smk_bioperiod_plots[idx], ncol = 3, nrow = 3, guides = "collect") +
    plot_annotation(
      title = paste0("Bioperiod Temperature Trends (", temp_label, ") – Theil-Sen Slopes (Page ", i, " of ", n_pages, ")")
    ) &
    theme(legend.position = "bottom")
}
page_plots[[1]]
page_plots[[2]]
page_plots[[3]]
page_plots[[4]]
page_plots[[5]]
page_plots[[6]]
page_plots[[7]]
page_plots[[8]]
page_plots[[9]]








# Comparisons of significant values
mk_summer = mk_results_summary %>% filter(p_value < 0.1) %>% mutate(method = "Summer Standard MK")
smk_summer_monthly = smk_results_sig %>% mutate(method = "Summer Monthly")
smk_season = smk_seasonal_sig %>% mutate(method = "Seasons")
smk_bioperiod = smk_bioperiod_sig %>% mutate(method = "Bioperiod")


smk_combined = bind_rows(
  mk_summer,
  smk_summer_monthly,
  smk_season,
  smk_bioperiod
) %>%
  select(staSeq, WaterbodyName, method, p_value)

smk_combined2 = smk_combined %>%
  filter(p_value < 0.05)

smk_compare = smk_combined %>%
  tidyr::pivot_wider(
    names_from = method,
    values_from = p_value,
    values_fill = NULL
  ) %>%
  mutate(station_label = paste(staSeq, WaterbodyName, sep = " - "))

smk_compare2 = smk_combined2 %>%
  tidyr::pivot_wider(
    names_from = method,
    values_from = p_value,
    values_fill = NULL
  ) %>%
  mutate(station_label = paste(staSeq, WaterbodyName, sep = " - "))

# p-values < 0.1
ggplot(
  smk_compare %>%
    tidyr::pivot_longer(
      cols = c("Summer Standard MK", "Summer Monthly", "Seasons", "Bioperiod")
    ),
  aes(x = name, y = station_label)
) +
  geom_tile(fill = "white", color = "black", linewidth = 0.7) +
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.4f", value))),
            size = 3.2, color = "black") +
  labs(
    title = "Comparison of Significant MK Results Using Median (p-values < 0.1)",
    x = "SMK Method",
    y = "Station"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# p-values < 0.05
ggplot(
  smk_compare2 %>%
    tidyr::pivot_longer(
      cols = c("Summer Standard MK", "Summer Monthly", "Seasons", "Bioperiod")
    ),
  aes(x = name, y = station_label)
) +
  geom_tile(fill = "white", color = "black", linewidth = 0.7) +
  geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.4f", value))),
            size = 3.2, color = "black") +
  labs(
    title = "Comparison of Significant MK Results Using Median (p-values < 0.05)",
    x = "SMK Method",
    y = "Station"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )




#Set differences
setdiff(smk_seasonal_results_summary$staSeq, smk_bioperiod_results_summary$staSeq)
setdiff(smk_bioperiod_results_summary$staSeq, smk_seasonal_results_summary$staSeq)

setdiff(smk_results_summary$staSeq, smk_bioperiod_results_summary$staSeq)
setdiff(smk_bioperiod_results_summary$staSeq, smk_results_summary$staSeq)
