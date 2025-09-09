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
# Parameters
library(RSQLite)
library(tidyverse)

input_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/data_to_qc"
output_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/qced_data"

all_csv_files <- list.files(
  path = input_main_directory,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

total_files = length(all_csv_files)

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
    
    file_out <- file.path(myDir.export, paste0("QC_", file_name))
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
