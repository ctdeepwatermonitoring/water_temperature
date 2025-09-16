library(tidyverse)

#Directories
old_directory = "/home/deepuser/ContDataQC/historic_temperature_project/qced_data"
new_directory = "/home/deepuser/TemperatureDB/testFTP/Upload/Cont_Data"

#Check if directory exists and if not, create one
if (!dir.exists(new_directory)) {
  dir.create(new_directory, recursive = TRUE)
}

#Listing all csv files
files_to_prep = list.files(
  path = old_directory,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

#Go through all csv files
for (f in files_to_prep) {
  #Read data
  data = read.csv(f, stringsAsFactors = FALSE, quote = "\"")
  
  data_out = data %>%
    mutate(
      dataFlag = Flag.Temp,
      comment = ""
    ) %>%
    transmute(
      Date_Time = as.character(Date_Time),
      Temp = Temp,
      UOM = UOM,
      ProbeID = ProbeID,
      SID = SID,
      Collector = Collector,
      ProbeType = ProbeType,
      dataFlag = dataFlag,
      comment = comment
    )
  
  folder_name = basename(dirname(f))
  parts = strsplit(folder_name, "_")[[1]]
  probeID = parts[1]
  
  old_file_name = basename(f)
  new_file_name = sub("^QC_", paste0("QC_", probeID, "_"), old_file_name)
  
  #Setting directory and writing out
  new_file = file.path(new_directory, new_file_name)
  write.csv(data_out, new_file, row.names = FALSE, na = "", quote = TRUE)
}