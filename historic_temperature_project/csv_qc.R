###############################################################################
# GitHub link to the original: https://github.com/USEPA/ContDataQC/tree/main
# GitHub link to CT DEEP's: https://github.com/ctdeepwatermonitoring/water_temperature/tree/main

# Have to run these 2 lines of code in the terminal to get this to run, 
# or make sure they're already installed:
# sudo apt install libcurl4-openssl-dev
# sudo apt install libxml2-dev

# reshape2 didn't install automatically so I moved it up here. 
# Run it first if not already installed.
if(!require(reshape2)){install.packages("reshape2")}

# Installs remotes if needed
if(!require(remotes)){install.packages("remotes")}
# Installs ContDataQC package from GitHub
remotes::install_github("ctdeepwatermonitoring/water_temperature", 
                        force = TRUE, 
                        build_vignettes = FALSE)

# Installs non-CRAN packages
remotes::install_github("jasonelaw/iha", 
                        force = TRUE, 
                        build_vignettes = FALSE)
remotes::install_github("tsangyp/StreamThermal", 
                        force = TRUE, 
                        build_vignettes = FALSE)

# Load library and dependent libraries
require("ContDataQC")

###############################################################################
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

#Specifying QC directories
input_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/data_to_qc"
output_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/qced_data"

#Listing all files and getting totals
all_csv_files = list.files(
  path = input_main_directory,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

total_files = length(all_csv_files)

#Loop through all deployment files. Assign defaults if less than 5 entries in a deployment and call QC method otherwise. Historic data uses the config_deep.R file.
for (i in seq_along(all_csv_files)) {
  file_path = all_csv_files[i]
  print(paste("Processing", i, "of", total_files, "total files"))
  file_name = basename(file_path)
  parts = strsplit(file_name, "_")[[1]]
  site_id = parts[1]
  start_date = parts[3]
  end_date = gsub("\\.csv$", "", parts[4])
  
  df = read_csv(file_path, show_col_types = FALSE)
  
  if (nrow(df) < 5) {
    df_out = df %>%
      mutate(
        mDateTime = as.character(mDateTime),
        createDate = as.character(createDate),
        Month = lubridate::month(as.POSIXct(mDateTime, format = "%Y-%m-%d %H:%M:%S")),
        Day = lubridate::day(as.POSIXct(mDateTime, format = "%Y-%m-%d %H:%M:%S")),
        Year = lubridate::year(as.POSIXct(mDateTime, format = "%Y-%m-%d %H:%M:%S")),
        MonthDay = as.integer(paste0(Month, Day)),
        `Flag.Gross.temp` = "X",
        `Flag.Spike.temp` = "X",
        `Flag.RoC.temp` = "X",
        `Flag.Flat.temp` = "X",
        `Flag.temp` = "X",
        `Comment.MOD.mDateTime` = "",
        `Comment.MOD.probeID` = "",
        `Comment.MOD.staSeq` = "",
        `Comment.MOD.mDate` = "",
        `Comment.MOD.mTime` = "",
        `Comment.MOD.parameter` = "",
        `Comment.MOD.temp` = "",
        `Comment.MOD.uom` = "",
        `Comment.MOD.createDate` = "",
        `Comment.MOD.comment` = "",
        `RAW.mDateTime` = as.character(mDateTime),
        `RAW.probeID` = probeID,
        `RAW.staSeq` = staSeq,
        `RAW.mDate` = mDate,
        `RAW.mTime` = mTime,
        `RAW.parameter` = parameter,
        `RAW.temp` = temp,
        `RAW.uom` = uom,
        `RAW.createDate` = as.character(createDate),
        `RAW.comment` = comment
      )
    
    myDir.import = dirname(file_path)
    rel_folder = basename(myDir.import)
    myDir.export = file.path(output_main_directory, rel_folder)
    dir.create(myDir.export, showWarnings = TRUE, recursive = TRUE)
    
    file_out = file.path(myDir.export, paste0("QC_", file_name))
    write_csv(df_out, file_out)
    
    print(paste("Skipped QC (too few rows). Wrote default file for", file_name))
    
  } else {
    myData.Operation = "QCRaw"
    myData.SiteID = site_id
    myData.Type = "Water"
    myData.DateRange.Start = paste0(substr(start_date,1,4), "-", substr(start_date,5,6), "-", substr(start_date,7,8))
    myData.DateRange.End = paste0(substr(end_date,1,4), "-", substr(end_date,5,6), "-", substr(end_date,7,8))
    
    myDir.import = dirname(file_path)
    rel_folder = basename(myDir.import)
    myDir.export = file.path(output_main_directory, rel_folder)
    dir.create(myDir.export, showWarnings = TRUE, recursive = TRUE)
    
    myReport.format = "html"
    myConfig = "/home/deepuser/ContDataQC/historic_temperature_project/config_deep.R"
    
    ContDataQC(myData.Operation,
               myData.SiteID,
               myData.Type,
               myData.DateRange.Start,
               myData.DateRange.End,
               myDir.import,
               myDir.export,
               fun.myConfig = myConfig,
               fun.myReport.format = myReport.format,
               fun.AddDeployCol = FALSE)
  }
}
