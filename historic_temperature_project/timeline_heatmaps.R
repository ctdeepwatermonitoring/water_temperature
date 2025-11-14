# Loading libraries
library(tidyverse)
library(broom)
library(ggforce)
library(patchwork)

#Helper function for temperature category
temp_category = function(temp, cold_thresh = 18.29, warm_thresh = 21.70) {
  case_when(
    temp < cold_thresh ~ "Cold",
    temp < warm_thresh ~ "Cool",
    TRUE ~ "Warm"
  )
}

#Read in the data
initial_data = data.table::fread("/home/deepuser/ContDataQC/historic_temperature_project/temperature.csv")
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
  #filter(staSeq %in% landscape_cover$staSeq) %>%
  left_join(
    sites_clean %>%
      select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  ) %>%
  mutate(
    staSeq_waterbodyName = paste(staSeq, "-", WaterbodyName)
  )

#Target year
target_year = 2019

rm(initial_data)

out_dir_base = file.path("/home/deepuser/ContDataQC/historic_temperature_project", "outputs")
dir.create(out_dir_base, recursive = TRUE, showWarnings = FALSE)

make_safe_filename = function(x) {
  x = iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}

# Calculating daily means
daily_means_summer_2019 = daily_means %>%
  filter(year == target_year, month %in% 6:8) %>%
  mutate(category = temp_category(mean_temp))

# Top 30 sites by coverage
top_sites = daily_means_summer_2019 %>%
  count(staSeq, name = "n_days") %>%
  arrange(desc(n_days)) %>%
  slice_head(n = 30) %>%
  pull(staSeq)

#Filtering for heatmap
daily_means_summer_2019_top = daily_means_summer_2019 %>%
  filter(staSeq %in% top_sites)

# Heatmap of temperature categories in a specific year
p_daily_heatmap_2019 = ggplot(daily_means_summer_2019_top, aes(
  x = date,
  y = staSeq_waterbodyName,
  fill = category
)) +
  geom_tile(width = 1, height = 0.8) +
  scale_fill_manual(values = c("Cold" = "blue", "Cool" = "cornflowerblue", "Warm" = "red")) +
  scale_x_date(
    breaks = as.Date(c(
      paste0(target_year, "-06-01"),
      paste0(target_year, "-07-01"),
      paste0(target_year, "-08-01")
    )),
    labels = c("Jun 1", "Jul 1", "Aug 1"),
    expand = c(0, 0)
  ) +
  labs(
    title = paste("Daily Mean Water Temperature by Site in", target_year, "(June–August)"),
    x = "Date",
    y = "Site ID",
    fill = "Temperature Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

# save
timeline_out_dir = file.path(out_dir_base, "timeline_heatmaps")
dir.create(timeline_out_dir, recursive = TRUE, showWarnings = FALSE)
fname = file.path(timeline_out_dir, "daily_mean_heatmap_2019.png")
ggsave(fname, plot = p_daily_heatmap_2019, width = 12, height = 10, dpi = 300)

#Plotting June-August Temperatures over the years
summer_avg_all = daily_means %>%
  filter(month %in% 6:8) %>%
  group_by(staSeq, year, WaterbodyName) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") %>%
  mutate(category = temp_category(avg_temp))

site_year_counts = summer_avg_all %>%
  count(staSeq, name = "n_years")

top_sites_longterm = site_year_counts %>%
  arrange(desc(n_years)) %>%
  slice_head(n = 30)

summer_avg_top = summer_avg_all %>%
  inner_join(top_sites_longterm, by = "staSeq") %>%
  mutate(staSeq_waterbodyName = paste(staSeq, "-", WaterbodyName))

#Heatmap: categorical
p_summer_categorical = ggplot(summer_avg_top, aes(
  x = year,
  y = fct_reorder(staSeq_waterbodyName, n_years),
  fill = category
)) +
  geom_tile(width = 1, height = 0.8) +
  scale_fill_manual(values = c("Cold" = "blue", "Cool" = "cornflowerblue", "Warm" = "red")) +
  labs(
    title = "Summer Mean Water Temperatures (June-August)",
    x = "Year",
    y = "Site (Waterbody)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

fname = file.path(timeline_out_dir, "summer_categorical_heatmap.png")
ggsave(fname, plot = p_summer_categorical, width = 10, height = 12, dpi = 300)





p_summer_continuous = ggplot(summer_avg_top, aes(
  x = year,
  y = fct_reorder(staSeq_waterbodyName, n_years),
  fill = avg_temp
)) +
  geom_tile(width = 1, height = 0.8) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Summer Mean Water Temperatures (June-August)",
    x = "Year",
    y = "Site (Waterbody)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

fname = file.path(timeline_out_dir, "summer_continuous_heatmap.png")
ggsave(fname, plot = p_summer_continuous, width = 10, height = 12, dpi = 300)


#June-august temperature
summer_avg = daily_means %>%
  filter(month(date) %in% 6:8) %>%
  group_by(staSeq, year) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop")

#Top 4 sites by years of data
top_4_sites = site_year_counts %>%
  arrange(desc(n_years)) %>%
  slice_head(n = 4)

summer_avg_top4 = summer_avg %>%
  inner_join(top_4_sites, by = "staSeq") %>%
  left_join(
    sites_clean %>%
      select(STA_SEQ, WaterbodyName),
    by = c("staSeq" = "STA_SEQ")
  ) %>%
  mutate(staSeq_waterbodyName = paste(staSeq, "-", WaterbodyName))

#Tracking temperature increase at top sites over time
p_top4_trends = summer_avg_top4 %>%
  ggplot(aes(x = year, y = avg_temp)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~staSeq_waterbodyName) +
  theme_minimal() +
  labs(
    title = "Summer Temperature Trends by Site",
    x = "Year",
    y = "Mean Summer Temperature (°C)"
  )

fname = file.path(timeline_out_dir, "top4_summer_trends.png")
ggsave(fname, plot = p_top4_trends, width = 12, height = 8, dpi = 300)




#Plotting overall summer temp trends
overall_trend = summer_avg %>%
  group_by(year) %>%
  summarise(mean_temp = mean(avg_temp, na.rm = TRUE))

p_overall_trend = ggplot(overall_trend, aes(x = year, y = mean_temp)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Overall Summer Mean Temperature Trend",
    x = "Year",
    y = "Mean Summer Temperature (°C)"
  ) +
  theme_minimal()

fname = file.path(timeline_out_dir, "overall_summer_trend.png")
ggsave(fname, plot = p_overall_trend, width = 8, height = 5, dpi = 300)

#Subsetting waterbody type

sites_type_subset = sites %>%
  mutate(
    STA_SEQ = as.character(STA_SEQ),
    WaterbodyType = case_when(
      str_detect(WaterbodyName, regex("river", ignore_case = TRUE)) ~ "River",
      str_detect(WaterbodyName, regex("brook|stream|creek", ignore_case = TRUE)) ~ "Stream/Brook/Creek",
      str_detect(WaterbodyName, regex("lake|reservoir", ignore_case = TRUE)) ~ "Lake/Reservoir",
      str_detect(WaterbodyName, regex("reservoir", ignore_case = TRUE)) ~ "Reservoir",
      str_detect(WaterbodyName, regex("pond", ignore_case = TRUE)) ~ "Pond",
      TRUE ~ "Other"
    )
  ) %>%
  select(STA_SEQ, WaterbodyName, WaterbodyType)







summer_avg_filtered = summer_avg %>%
  inner_join(site_year_counts %>% filter(n_years >= 10), by = "staSeq")

# Slopes per site
# Compute slopes for all sites
site_slopes = summer_avg_filtered %>%
  group_by(staSeq) %>%
  do({
    fit = lm(avg_temp ~ year, data = .)
    tibble(slope = coef(fit)[["year"]])
  }) %>%
  ungroup()


# Top warming & cooling (can adjust n = 5, 10, etc.)
top_warming = site_slopes %>% arrange(desc(slope)) %>% slice_head(n = 5)
top_cooling = site_slopes %>% arrange(slope) %>% slice_head(n = 5)

extreme_sites = site_slopes %>%
  slice_max(abs(slope), n = 5) %>%
  pull(staSeq)


# Join slopes into the data frame for plotting
summer_avg_extreme = summer_avg %>%
  filter(staSeq %in% extreme_sites) %>%
  left_join(sites_clean %>% 
              select(STA_SEQ, WaterbodyName),
            by = c("staSeq" = "STA_SEQ")) %>%
  left_join(site_slopes, by = "staSeq") %>%
  mutate(staSeq_waterbodyName = paste0(staSeq, " - ", WaterbodyName)) %>%
  # make sure slope is not NA
  filter(!is.na(slope))


p_extreme_trends = ggplot(summer_avg_extreme, aes(x = year, y = avg_temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ fct_reorder(staSeq_waterbodyName, slope, .na_rm = TRUE),
             scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Most Extreme Summer Temperature Trends",
    x = "Year",
    y = "Mean Summer Temperature (°C)"
  )

fname = file.path(timeline_out_dir, "extreme_summer_trends.png")
ggsave(fname, plot = p_extreme_trends, width = 12, height = 8, dpi = 300)