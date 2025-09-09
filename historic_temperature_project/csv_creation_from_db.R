# Loading RSQLite library
library(RSQLite)
library(tidyverse)

# Establishing database connection
con = dbConnect(SQLite(), dbname = '/home/deepuser/ContDataQC/historic_temperature_project/historic_temperature.db')

# Data cleaning
SQL = 'SELECT * FROM temperature;'
whole_db = dbGetQuery(conn = con, SQL)
dbDisconnect(con)

# Standardizing uom to Degrees C (Correcting cases. Also checked that anything
# that says unit is meant to be Degrees C. They are.)
standard_degrees = whole_db
standard_degrees$uom[standard_degrees$uom != "Degrees C"] = "Degrees C"
rm(whole_db)

# Removing QA post deploy in the parameters
drop_qa_post = standard_degrees
drop_qa_post = drop_qa_post[drop_qa_post$parameter != "QA post deploy", ]
rm(standard_degrees)

# Changing all parameters to Water Temperature (Correcting capitalization and filling in NA)
standard_water_temp = drop_qa_post
standard_water_temp$parameter[standard_water_temp$parameter != "Water Temperature" | is.na(standard_water_temp$parameter)] = "Water Temperature"
rm(drop_qa_post)

# Dropping all negative probes
probe_drop_neg = standard_water_temp[!grepl("^-", standard_water_temp$probeID), ]
rm(standard_water_temp)

# Dropping where probe is 123456
drop_123456 = probe_drop_neg[probe_drop_neg$probeID != "123456", ]
rm(probe_drop_neg)

# Dropping where probe is NA
drop_na_probes = drop_123456[!is.na(drop_123456$probeID), ]
rm(drop_123456)

# Changing NA comments to ""
drop_na_probes$comment[is.na(drop_na_probes$comment)] = "None"

# Changing NA createDate to "Missing"
drop_na_probes$createDate[is.na(drop_na_probes$createDate)] = "Missing"

# Converting all times to 24 hours. Previous testing confirms date-only mTime values are meant to be 00:00:00
standard_24_time= drop_na_probes %>%
  mutate(
    mTime = case_when(
      str_detect(mTime, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ "00:00:00",
      str_detect(mTime, "^\\d{1,2}:\\d{2}:\\d{2} ?[AaPp][Mm]$") ~ format(strptime(mTime, "%I:%M:%S %p"), "%H:%M:%S"),
      str_detect(mTime, "^\\d{1,2}/\\d{1,2}/\\d{4} ?\\d{1,2}:\\d{2}(:\\d{2})? ?[AaPp][Mm]$") ~ format(strptime(mTime, "%m/%d/%Y %I:%M:%S %p"), "%H:%M:%S"),
      TRUE ~ mTime
    )
  )
rm(drop_na_probes)

# Dropping duplicates
time_dup_resolved = distinct(standard_24_time, probeID, staSeq, mDate, mTime, .keep_all = TRUE)
rm(standard_24_time)

#Adding mDateTime
added_dt = time_dup_resolved %>%
  mutate(
    mDateTime = paste(mDate, mTime)
  )
rm(time_dup_resolved)

#Split deployments
gap_threshold = 1

deployments = added_dt %>%
  arrange(staSeq, probeID, mDateTime) %>%
  group_by(staSeq, probeID) %>%
  mutate(
    time_diff = as.numeric(difftime(as.POSIXct(mDateTime, format = "%Y-%m-%d %H:%M:%S"), as.POSIXct(lag(mDateTime), format = "%Y-%m-%d %H:%M:%S"), units = "days")),
    new_deployment = ifelse(is.na(time_diff) | time_diff > gap_threshold, 1, 0),
    deployment_id = cumsum(new_deployment)
  ) %>%
  ungroup()

# Group and split into list
df_list = deployments %>%
  group_by(staSeq, probeID, deployment_id) %>%
  group_split()

# Get keys for file names
group_keys = deployments %>%
  group_by(staSeq, probeID, deployment_id) %>%
  summarise(
    startDeploymentDate = format(min(as.POSIXct(mDateTime, format = "%Y-%m-%d %H:%M:%S")), "%Y%m%d"),
    endDeploymentDate = format(max(as.POSIXct(mDateTime, format = "%Y-%m-%d %H:%M:%S")), "%Y%m%d"),
    .groups = "drop"
  )

# Create file names
file_names = paste0(
  group_keys$staSeq, "_Water_", 
  group_keys$startDeploymentDate, "_",
  group_keys$endDeploymentDate, ".csv"
)

# Directory path
dir_path = "/home/deepuser/ContDataQC/historic_temperature_project/data_to_qc"

# Write all files
for (i in seq_along(df_list)) {
  df = df_list[[i]]
  
  probeID = unique(df$probeID)
  
  # Remove extra logic columns before saving
  df = df %>% select(-time_diff, -new_deployment, -deployment_id)
  
  subfolder_name = paste0(probeID, "_", sub(".csv", "", file_names[i]))
  
  subfolder_path = paste0(dir_path, "/", subfolder_name)
  
  dir.create(file.path(dir_path, subfolder_name))
  
  file_path = file.path(subfolder_path, file_names[i])
  
  write.csv(df, file_path, row.names = FALSE)
  
  print(paste("Processed", i, "of", length(df_list), "files"))
}