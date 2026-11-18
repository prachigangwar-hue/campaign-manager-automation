#' Date: 03APR2023 Time: 03:00PM
#' Author: Archana Pandita

# Objective: automate diwali segment generator
# diwali: who shooped in last  diwali campaigns
# event count = bill date in last diwali duration
#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)
library(tidyverse)

# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_lib <- file.path(path_proj, "auto/library")

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

#----  Path/variable declaration----
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/11TG_7c1je4f-rDUB5pKGUJlvIoCiB2o3P3HqKJHFt2M/edit#gid=714304411")

# set output path
out_path <- file.path(database$database$vmart$segment$prev_diwali)

#---- Function Calls, Data Reading & Manipulations ----
# reading store master to filter active stores
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[mt_event$`event(acronym)` == "diwali(D)", ]
mt_event <- mt_event[(nrow(mt_event) - 1 ), ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_PREV_DIWALI")
file_name <- rownames(files)[which.max(files$mtime)]

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
  toupper(format(as.Date(mt_event[length(mt_event$sno)]$`end period`), "%d%b%Y")), "_CUSTOMER_PREV_DIWALI.parquet"
)

start_dates <- as.Date(mt_event$`start period`)
end_dates <- as.Date(mt_event$`end period`)

day <- REQUIRED_PERIODS(start_dates, end_dates)

# read rfm data from folder
mt_cust <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency")
)

#---- Displaying period, ----
# get the minimum and maximum dates as the period
if (nrow(mt_cust) > 0) {
  date <- data.table()
  setDT(date)
  date$f_txn <- as.data.table(unique(mt_cust$First_Transaction_Date))
  date$l_txn <- as.data.table(unique(mt_cust$Last_Transaction_Date))
  
  date$f_txn <- as.Date(date$f_txn, "%d-%b-%y")
  date$l_txn <- as.Date(date$l_txn, "%d-%b-%y")
  
  min_date <- toupper(format(as.Date(min(date[["f_txn"]])), "%d%b%Y"))
  max_date <- toupper(format(as.Date(max(date[["l_txn"]])), "%d%b%Y"))
  
  # group on customer mobile number
  mt_cust <- mt_cust[, .(Total_Freq = sum(Frequency, na.rm = TRUE)), .(Customer_Mobile)]
  
  # Removing duplicated records
  mt_cust <- mt_cust[!duplicated(Customer_Mobile), ]
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_PREV_DIWALI")
}

# replace na with 0
mt_cust[is.na(mt_cust)] <- 0

# replace value >= 1 to 1 else 0
mt_cust <- mt_cust[, (2:ncol(mt_cust)) := lapply(.SD, function(x) ifelse(x >= 1, 1, 0)), .SDcols = 2:ncol(mt_cust)]

# create new column as  _prevDIWALI
mt_cust$seg_prev_diwali <- 1

# check mobile number
mt_cust <- mob_check(mt_cust, "Customer_Mobile")

# Removing duplicated records
mt_cust <- mt_cust[!duplicated(Customer_Mobile), ]

mt_cust <- mt_cust[, c("Customer_Mobile", "seg_prev_diwali"), with = FALSE]

# set datatype of customer mobile
mt_cust$Customer_Mobile <- as.character(mt_cust$Customer_Mobile)
mt_cust$seg_prev_diwali <- as.character(mt_cust$seg_prev_diwali)

# total number of customers belongs to prev DIWALI segment.
nrow(mt_cust)

# view list of files in particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(mt_cust, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_PREV_DIWALI"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_PREV_DIWALI"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_PREV_DIWALI")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_PREV_DIWALI")
}



LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_PREV_DIWALI")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: prev_diwali_segment.R", type = "text")
