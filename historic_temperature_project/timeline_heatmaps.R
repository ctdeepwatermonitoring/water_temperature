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
  filter(staSeq %in% landscape_cover$staSeq) %>%
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

# Prepare daily means for June–August
daily_means_summer_2019 = daily_means %>%
  filter(year == target_year, month %in% 6:8)

rm(initial_data)

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
ggplot(daily_means_summer_2019_top, aes(
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

#July temperature
july_avg = daily_means %>%
  filter(month(date) == 7) %>%
  group_by(staSeq, year) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE))

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
ggplot(summer_avg_top, aes(
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




#Plotting Temperatures at specific sites over the years
ansel_sites = read_csv("/home/deepuser/Documents/temp_categories_31_sites.csv") %>%
  mutate(AWQ = as.character(AWQ))

avg_ansel_summer = ansel_sites %>%
  left_join(summer_avg_all, by = c("AWQ" = "staSeq")) %>%
  mutate(staSeq_waterbodyName = paste(AWQ, "-", `Station Name`))

avg_all = daily_means %>%
  group_by(staSeq, year, WaterbodyName) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") %>%
  mutate(category = temp_category(avg_temp)) %>%
  select(-WaterbodyName)

avg_ansel = ansel_sites %>%
  left_join(avg_all, by = c("AWQ" = "staSeq")) %>%
  mutate(staSeq_waterbodyName = paste(AWQ, "-", `Station Name`))

values_to_keep = c("14316", "14317", "14341", "14410", "14413", "14442", "14484", "14581", "14605", "15312", "16119", "16124", "16127", "16128")
ansel_subset = avg_ansel %>%
  filter(AWQ %in% values_to_keep)
ansel_summer_subset = avg_ansel_summer %>%
  filter(AWQ %in% values_to_keep)


#Heatmap: categorical
ggplot(ansel_summer_subset, aes(
  x = year,
  y = staSeq_waterbodyName,
  fill = category
)) +
  geom_tile(width = 1, height = 0.8) +
  scale_fill_manual(values = c(
    "Cold" = "blue",
    "Cool" = "cornflowerblue",
    "Warm" = "red"
  )) +
  labs(
    title = "Mean Summer (June-August) Water Temperatures - HOBO 2019 Removals on Long-Term Trend List",
    x = "Year",
    y = "Site (Waterbody)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

summer_summary = avg_ansel_summer %>%
  count(AWQ, `Station Name`, category) %>%
  group_by(AWQ, `Station Name`) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

year_round_with_summer_cat = avg_ansel %>%
  left_join(
    summer_summary %>% select(AWQ, predominant_summer = category),
    by = "AWQ"
  )

cool_sites = year_round_with_summer_cat %>%
  filter(predominant_summer == "Cool")

cold_sites = year_round_with_summer_cat %>%
  filter(predominant_summer == "Cold")

warm_sites = year_round_with_summer_cat %>%
  filter(predominant_summer == "Warm")

no_data_sites = year_round_with_summer_cat %>%
  filter(is.na(predominant_summer))

write_csv(cool_sites, "/home/deepuser/Documents/sites_cool.csv")
write_csv(cold_sites, "/home/deepuser/Documents/sites_cold.csv")
write_csv(warm_sites, "/home/deepuser/Documents/sites_warm.csv")
write_csv(no_data_sites, "/home/deepuser/Documents/sites_nodata.csv")









#Heatmap: continuous
ggplot(summer_avg_top, aes(
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
summer_avg_top4 %>%
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




#Plotting overall summer temp trends
overall_trend = summer_avg %>%
  group_by(year) %>%
  summarise(mean_temp = mean(avg_temp, na.rm = TRUE))

ggplot(overall_trend, aes(x = year, y = mean_temp)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Overall Summer Mean Temperature Trend",
    x = "Year",
    y = "Mean Summer Temperature (°C)"
  ) +
  theme_minimal()

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


ggplot(summer_avg_extreme, aes(x = year, y = avg_temp)) +
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