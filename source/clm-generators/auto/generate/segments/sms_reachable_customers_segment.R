# Date: 22/05/2025
# Author: Abhijit Nandi

# Objective: automate new customer segment in terms of RFM

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(arrow)
library(purrr)
library(tidyverse)
# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_lib <- file.path(path_proj, "auto/library")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")
path_segment <- file.path(path_dbase,"seg","segment")
cur_date <- toupper(format(as.Date(Sys.Date()), "%d%b%Y"))
path_dlr_logs_karix <- file.path("D:/database/dlr/processed/karix/sms_logs")
path_dlr_logs_route <- file.path("D:/database/dlr/processed/route/sms",(format(Sys.Date(),"%b_%Y")))

out_path <- file.path(path_dbase,"seg","seg_sms_reachable_customers")
file_name_to_save <- paste0(cur_date, "_sms_reachable_customers.parquet")


source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

# period <- paste0(seq.Date(from = Sys.Date()-90,to = Sys.Date()-1,by = "days"),collapse = "|")

dt_logs_karix <- READ_FOLDER(pathFold = file.path(path_dlr_logs_karix),patternFile = "parquet",
                             contain = toupper(format(Sys.Date(),"%b%Y")))
dt_logs_karix <- dt_logs_karix[,.(Customer_Mobile,Delivery_Status)]

dt_route <- READ_FOLDER(pathFold = file.path(path_dlr_logs_route),patternFile = "parquet",
                        contain = (format(Sys.Date(),"%b_%Y")))

if(nrow(dt_route)>0){
  dt_route <- dt_route[,.(Customer_Mobile,Delivery_Status)]
  dt_logs <- rbind(dt_logs_karix,dt_route)
  rm(dt_logs_karix,dt_route);gc()
}else{
  dt_logs <- dt_logs_karix
  dt
  rm(dt_logs_karix);gc()
}


dt_logs$Delivery_Status <- ifelse(dt_logs$Delivery_Status %in% c("Read","Delivered"),1,0)
dt_logs <- dt_logs[,.("seg_sms_reachable_customers" = sum(Delivery_Status,na.rm = TRUE)),.(Customer_Mobile)]
dt_logs <- dt_logs[,"seg_sms_reachable_customers":=ifelse(seg_sms_reachable_customers>0,1,0)]

dt_temp <- READ_FOLDER(pathFold = out_path,patternFile = "parquet")
dt_logs <- rbind(dt_logs,dt_temp)
rm(dt_temp);gc()

dt_logs <- dt_logs[!duplicated(Customer_Mobile)]
dt_logs <- dt_logs[seg_sms_reachable_customers==1]

dt_logs[, names(dt_logs) := lapply(.SD, as.character)]

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_logs, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_SMS_REACHABLE"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_SMS_REACHABLE"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_SMS_REACHABLE")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_SMS_REACHABLE")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_SMS_REACHABLE)")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: Segment_rfm.R", type = "text")
