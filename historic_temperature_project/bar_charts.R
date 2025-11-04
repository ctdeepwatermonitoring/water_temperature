# Loading libraries
library(tidyverse)
library(broom)
library(ggforce)
library(Kendall)
library(patchwork)

#Helper function for temperature category
temp_category = function(temp, cold_thresh = 18.29, warm_thresh = 21.70) {
  case_when(
    temp < cold_thresh ~ "Cold",
    temp < warm_thresh ~ "Cool",
    TRUE ~ "Warm"
  )
}

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

#Read in the data
initial_data = read_csv("/home/deepuser/ContDataQC/historic_temperature_project/temperature.csv")
sites = read_csv("/home/deepuser/ContDataQC/historic_temperature_project/awx_stations_webservice(stations).csv")
landscape_cover = read_csv("/home/deepuser/ContDataQC/historic_temperature_project/chloride_mmi_lc_2003_2020.csv")

#Clean sites data
clean_sites = function(sites_df) {
  sites_df %>%
    mutate(STA_SEQ = as.character(STA_SEQ)) %>%
    select(STA_SEQ, WaterbodyName)
}
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
  filter(staSeq %in% landscape_cover$staSeq) %>%
  left_join(
    sites_clean %>%
      select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  ) %>%
  mutate(
    staSeq_waterbodyName = paste(staSeq, "-", WaterbodyName)
  )

#Plotting June-August Temperatures over the years
summer_avg_all = daily_means %>%
  filter(month %in% 6:8) %>%
  group_by(staSeq, year, WaterbodyName) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") %>%
  mutate(category = temp_category(avg_temp))

#Bar chart
summer_avg_longterm = summer_avg_all %>%
  group_by(staSeq) %>%
  filter(n_distinct(year) >= 10) %>%
  ungroup()

summer_avg_longterm = summer_avg_longterm %>%
  mutate(
    year_group_start = floor(year / 5) * 5,
    year_group_end = year_group_start + 4,
    year_group = paste0(year_group_start, "-", year_group_end)
  )

summer_5yr_summary = summer_avg_longterm %>%
  group_by(staSeq, WaterbodyName, year_group) %>%
  summarise(
    mean_temp = mean(avg_temp, na.rm = TRUE),
    median_temp = median(avg_temp, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

summer_5yr_summary = summer_5yr_summary %>%
  mutate(facet_label = paste(staSeq, "-", WaterbodyName))

site_ranges = summer_5yr_summary %>%
  group_by(staSeq, WaterbodyName, facet_label) %>%
  summarise(
    min_temp = min(mean_temp, na.rm = TRUE),
    max_temp = max(mean_temp, na.rm = TRUE),
    .groups = "drop"
  )

cold_lines = site_ranges %>%
  mutate(
    yintercept = COLD_MAX,
    color = "mediumturquoise",
    line_type = "Cold threshold (18.29°C)"
  )

warm_lines = site_ranges %>%
  filter(max_temp > WARM_MIN) %>%
  mutate(
    yintercept = WARM_MIN,
    color = "indianred1",
    line_type = "Warm threshold (21.70°C)"
  )

site_lines = bind_rows(cold_lines, warm_lines)

summer_5yr_summary = summer_5yr_summary %>%
  left_join(site_ranges, by = c("staSeq", "WaterbodyName", "facet_label")) %>%
  mutate(
    y_min = ifelse(min_temp > COLD_MAX, 17, 13)
  )

site_list = split(summer_5yr_summary, summer_5yr_summary$facet_label)
site_lines_list = split(site_lines, site_lines$facet_label)
THRESHOLD_LINES_LEGEND = tibble(
  yintercept = c(COLD_MAX, WARM_MIN),
  line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
)

plots = list()
for (facet in names(site_list)) {
  df = site_list[[facet]]
  min_temp = df$min_temp[1]
  max_temp = max(df$max_temp, na.rm = TRUE)
  y_min = if (min_temp < COLD_MAX) {
    13
  } else if (min_temp <= WARM_MIN) {
    17
  } else {
    19
  }
  y_max = max_temp + 2.5
  
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
  
  # Dummy lines for legend (always both, invisible)
  THRESHOLD_LINES_LEGEND = tibble(
    yintercept = c(COLD_MAX, WARM_MIN),
    line_type = c("Cold threshold (18.29°C)", "Warm threshold (21.70°C)")
  )
  
  p = ggplot(df, aes(x = year_group, y = mean_temp)) +
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
    geom_col(fill = "gray70") +
    geom_text(
      aes(label = paste0("n = ", n_years), y = mean_temp + 0.5),
      vjust = 0.3,
      size = 3.5
    ) +
    labs(
      title = facet,
      x = "5-Year Period",
      y = "Mean Summer Temperature (°C)",
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
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  plots[[facet]] = p
}

plots_per_page = 9
n_pages = ceiling(length(plots) / plots_per_page)

for (i in seq_len(n_pages)) {
  idx = ((i - 1) * plots_per_page + 1):(min(i * plots_per_page, length(plots)))
  page_plot = wrap_plots(plots[idx], ncol = 3, nrow = 3, guides = "collect") +
    plot_annotation(
      title = paste("5-Year Mean Summer Water Temperatures by Site (Page", i, "of", n_pages, ")")
    ) &
    theme(legend.position = "bottom")
  print(page_plot)
}