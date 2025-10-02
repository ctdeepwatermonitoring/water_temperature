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
library(tidyverse)

#Directories
input_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/raw_data"
rename_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/data_to_qc"
output_main_directory = "/home/deepuser/ContDataQC/historic_temperature_project/qced_data"
error_directory = "/home/deepuser/ContDataQC/historic_temperature_project/error_files"
error_log = "/home/deepuser/ContDataQC/historic_temperature_project/error_files/error_log"
script_name = "csv_qc2.R"

#List of all csv files
all_csv_files = list.files(
  path = input_main_directory,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

#Total files
total_files = length(all_csv_files)

#Loop along all csv files, rename in expected format and save to subfolders
for (i in seq_along(all_csv_files)) {
  df_path = all_csv_files[i]
  
  tryCatch({
    col_map = c(
      "DATE_TIME"   = "Date_Time",
      "Date Time"   = "Date_Time",
      "Date Time, GMT-04:00" = "Date_Time",
      "TEMP"        = "Temp",
      "Temp, \xb0C (LGR S/N: 10122051, SEN S/N: 10122051)" = "Temp",
      "UOM"         = "UOM",
      "PROBEID"     = "ProbeID",
      "ProbID"      = "ProbeID",
      "Probe ID"    = "ProbeID",
      "ProbeID "    = "ProbeID",
      "Probe_ID"    = "ProbeID",
      "SID"         = "SID",
      "COLLECTOR"   = "Collector",
      "PROBE_TYPE"  = "ProbeType"
    )
    
    first_line = readLines(df_path, n = 1)
    delim = ifelse(grepl("\t", first_line), "\t", ",")
    
    df = read_delim(df_path, 
                    delim = delim, 
                    locale = locale(encoding = "latin1"),
                    show_col_types = FALSE) %>%
      select(-any_of(c("ShedsID", "SHEDS"))) %>%
      rename_with(~ recode(., !!!col_map), everything()) %>%
      rename_with(~ str_remove(., ",.*"), matches("Date_Time|Temp"))
    
    # If ProbeType column is missing, add it with "HOBO"
    if (!"ProbeType" %in% names(df)) df$ProbeType = "HOBO"
    
    df_clean = df %>%
      mutate(
        Date_Time = parse_date_time(Date_Time, orders = c("mdy HMS", "mdy HM")),
        ProbeType = ifelse(ProbeType == "TIDB", "TIDBIT", ProbeType),
        Collector = case_when(
          Collector == " VOL" ~ "VOL",
          TRUE ~ Collector
        ),
        UOM = case_when(
          UOM == "degC" ~ "deg C",
          UOM == "deg" & temp < 30 ~ "deg C",
          UOM == "Logged" & temp < 30 ~ "deg C",
          TRUE ~ UOM
        )
      ) %>%
      filter(!is.na(Date_Time), !is.na(SID), !is.na(ProbeID)) %>%
      arrange(Date_Time) %>%
      distinct()
    
    # Compute deployment dates
    startDeploymentDate = format(min(df_clean$Date_Time), "%Y%m%d")
    endDeploymentDate   = format(max(df_clean$Date_Time), "%Y%m%d")
    
    df_clean = df_clean %>%
      mutate(
        mDate = format(Date_Time, "%m/%d/%Y"),
        mTime = format(Date_Time, "%H:%M:%S"),
        Date_Time = format(Date_Time, "%Y-%m-%d %H:%M:%S")
      )
    
    probeID = unique(df_clean$ProbeID)
    staSeq = unique(df_clean$SID)
    out_identifier = paste0(staSeq, "_Water_", startDeploymentDate, "_", endDeploymentDate)
    out_name = paste0(out_identifier, ".csv")
    out_folder = file.path(rename_main_directory, paste0(probeID, "_", out_identifier))
    out_path = file.path(out_folder, out_name)
    
    dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
    write_csv(df_clean, out_path)
    
  }, error = function(e) {
    # On error: print message, log error, and move file to error folder
    message(paste("Error processing file:", df_path))
    message("Error message:", e$message)
    error_file = file.path(
      error_log,
      paste0(tools::file_path_sans_ext(basename(df_path)), ".txt")
    )
    
    writeLines(
      c(paste("Timestamp:", Sys.time()),
        paste("Script:", script_name),
        paste("File:", basename(df_path)),
        paste("Error:", conditionMessage(e))
      ),
      con = error_file
    )
    
    file.rename(df_path, file.path(error_directory, basename(df_path)))
  })
}






#QCing data
qc_prep_files = list.files(
  path = rename_main_directory,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

#total files
total_qc_prep_files = length(qc_prep_files)

#Loop along csv files, QC them, and save them into their subfolders
for (i in seq_along(qc_prep_files)) {
  file_path = qc_prep_files[i]
  print(paste("Processing", i, "of", total_qc_prep_files, "total files"))
  tryCatch({
    file_name = basename(file_path)
    parts = strsplit(file_name, "_")[[1]]
    site_id = parts[1]
    start_date = parts[3]
    end_date = gsub("\\.csv$", "", parts[4])
    
    df = read_csv(file_path, show_col_types = FALSE)
    
    #Fill default values for files with less than 5 rows
    if (nrow(df) < 5) {
      df_out = df %>%
        mutate(
          Date_Time = as.character(Date_Time),
          Month = lubridate::month(as.POSIXct(Date_Time, format = "%Y-%m-%d %H:%M:%S")),
          Day = lubridate::day(as.POSIXct(Date_Time, format = "%Y-%m-%d %H:%M:%S")),
          Year = lubridate::year(as.POSIXct(Date_Time, format = "%Y-%m-%d %H:%M:%S")),
          MonthDay = as.integer(paste0(Month, Day)),
          `Flag.Gross.temp` = "X",
          `Flag.Spike.temp` = "X",
          `Flag.RoC.temp` = "X",
          `Flag.Flat.temp` = "X",
          `Flag.temp` = "X",
          `Comment.MOD.Date_Time` = "",
          `Comment.MOD.Temp` = "",
          `Comment.MOD.UOM` = "",
          `Comment.MOD.ProbeID` = "",
          `Comment.MOD.SID` = "",
          `Comment.MOD.Collector` = "",
          `Comment.MOD.ProbeType` = "",
          `Comment.MOD.mDate` = "",
          `Comment.MOD.mTime` = "",
          `RAW.Date_Time` = "",
          `RAW.Temp` = "",
          `RAW.UOM` = "",
          `RAW.ProbeID` = "",
          `RAW.SID` = "",
          `RAW.Collector` = "",
          `RAW.ProbeType` = "",
          `RAW.mDate` = "",
          `RAW.mTime` = "",
        )
      
      #Writing out with uniquely identifying subfolder names
      myDir.import = dirname(file_path)
      rel_folder = basename(myDir.import)
      myDir.export = file.path(output_main_directory, rel_folder)
      dir.create(myDir.export, showWarnings = TRUE, recursive = TRUE)
      
      file_out = file.path(myDir.export, paste0("QC_", file_name))
      write_csv(df_out, file_out)
      
      print(paste("Skipped QC (too few rows). Wrote default file for", file_name))
      
    } else {
      #Call QC method. Uses config_deep2.R file.
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
      myConfig = "/home/deepuser/ContDataQC/historic_temperature_project/config_deep2.R"
      
      ContDataQC::ContDataQC(myData.Operation,
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
  }, error = function(e) {
    # On error: print message and move file to error folder
    message(paste("Error processing file:", df_path))
    message("Error message:", e$message)
    
    error_file = file.path(
      error_log,
      paste0(tools::file_path_sans_ext(basename(df_path)), ".txt")
    )
    
    writeLines(
      c(paste("Timestamp:", Sys.time()),
        paste("Script:", script_name),
        paste("File:", basename(df_path)),
        paste("Error:", conditionMessage(e))
      ),
      con = error_file
    )
    
    file.rename(df_path, file.path(error_directory, basename(df_path)))
  })
}