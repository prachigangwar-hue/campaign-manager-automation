#' Date: 28MAR2023 Time: 11:30PM
#' Author: Archana Pandita

# Objective: automate active customer segment generation
# active customer: active customer grouped into following categories active most recent(a-mr), active recent(a-r), active about to lapse(atl)

# last transaction date = bill date on which customer transacted at store for the last time
# if last transaction date = in(today, today - 60) then active most recent
# if last transaction date = in(today - 61, today - 120) then active recent
# if last transaction date = in(today - 121, today - 180) then active about to lapse

#----Cleaning Environment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)
# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_lib <- file.path(path_proj, "auto/library")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

#----  Path/variable declaration----
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$td$today_7)
ids_to_send <- c("745474876")

out_path <- file.path(database$database$vmart$segment$active_group)

#---- Function Calls, Data Reading & Manipulations ----

# read rfm data from folder
dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = "",  recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Last_Transaction_Date")
)

#set datatype of last transaction date
dt_rfm$Last_Transaction_Date <- as.Date(dt_rfm$Last_Transaction_Date, format = "%d-%b-%y")

min_date <-  toupper(format(as.Date(min(dt_rfm$Last_Transaction_Date)), "%d%b%Y"))
max_date <-toupper(format(as.Date(max(dt_rfm$Last_Transaction_Date)), "%d%b%Y"))

#filename to be saved
file_name_to_save <- paste0(min_date, "-", max_date, "_ACTIVE_GROUP.parquet")

dt_rfm$Last_Transaction_Date <- as.Date(dt_rfm$Last_Transaction_Date, "%d-%b-%y")
dt_rfm$days_recency <- as.numeric(Sys.Date() - 7 - dt_rfm$Last_Transaction_Date)
dt_rfm$active_group <- cut(dt_rfm$days_recency, breaks = c(-Inf, 90, 150, 180, 360, 720, Inf), c("a-mr", "a-r", "a-atl", "l", "o", "p"))

# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

#---- Displaying period, ----
dt_rfm <- dt_rfm[,.(Customer_Mobile, active_group)]

#set datatype of customer mobile 
dt_rfm$active_group <- as.character(dt_rfm$active_group) 
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile) 

#total number of customers belongs to active group
nrow(dt_rfm)

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_rfm, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_ACTIVE_GROUP"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_ACTIVE_GROUP"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_ACTIVE_GROUP")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_ACTIVE_GROUP")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_ACTIVE_GROUP")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: seg_active_group.R", type = "text")

