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
path_mt_rfm <- file.path(path_dbase,"rfm/customer/division_group/offline/tagged/td/today_7")
path_gsheet_cred <- file.path("C:/R - Projects/credentials/gsheet/digital")
cur_date <- toupper(format(as.Date(Sys.Date()), "%d%b%Y"))
out_path <- file.path(path_dbase,"seg","seg_customer_value_bucket")
file_name_to_save <- paste0(cur_date, "_customer_value_bucket.parquet")
sheet_url <- "https://docs.google.com/spreadsheets/d/1HHys6bER6IXNcx4XkX0hJPdXYuCkWbjRD1WiJQYI1i4/edit?usp=sharing"

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

dt_segment <- READ_FOLDER(pathFold = path_segment,patternFile = "parquet")
mt_rfm <- READ_FOLDER(pathFold = path_mt_rfm,patternFile = "parquet",selCols = c("Customer_Mobile","Store"))
mt_rfm$Customer_Mobile <- as.character(mt_rfm$Customer_Mobile)

dt_segment <- left_join(dt_segment,mt_rfm)

googlesheets4::gs4_auth(path = file.path(path_gsheet_cred,"gmail_credentials.json"),email = "digital@vmartretail.com")

dt_sheet <- googlesheets4::read_sheet(ss = sheet_url,sheet = "Data")
setDT(dt_sheet)

dt_sheet <- dt_sheet[,.(Store,Segment,Bucket)]

setnames(dt_sheet,"Bucket","seg_customer_value_bucket")
setnames(dt_segment,"segment","Segment")

dt_segment <- left_join(dt_segment,dt_sheet,by=c("Store","Segment"))
dt_segment <- dt_segment[!is.na(seg_customer_value_bucket)]
dt_segment <- dt_segment[!duplicated(Customer_Mobile)]

dt_segment <- dt_segment[,.(Customer_Mobile,seg_customer_value_bucket)]
dt_segment[, names(dt_segment) := lapply(.SD, as.character)]
dt_segment <- dt_segment[seg_customer_value_bucket=="Low Value",seg_customer_value_bucket:="low_value_customer"]
dt_segment <- dt_segment[seg_customer_value_bucket=="Mid Value",seg_customer_value_bucket:="medium_value_customer"]
dt_segment <- dt_segment[seg_customer_value_bucket=="High Value",seg_customer_value_bucket:="high_value_customer"]
# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_segment, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VALUE_BUCKET"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VALUE_BUCKET"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_VALUE_BUCKET")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_VALUE_BUCKET")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VALUE_BUCKET")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: Segment_rfm.R", type = "text")
