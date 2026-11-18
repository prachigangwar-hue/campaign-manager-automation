#' Date: 3JULY2024 Time: 12:14AM
#' Author: Archana Pandita

#' Objective: automate DOB segment generation
#'  DOB in ranges of ((16-20),(21-22),(>=25))  

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
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

path_data <-  "D:/database/mstr/dob/"
ids_to_send <- c("745474876")
current_date <- toupper(format(as.Date(Sys.Date()), "%d%b%Y")) 
#---- Function Calls, Data Reading & Manipulations ----
dt_dob <- READ_FOLDER(pathFold = path_data, patternFile = "parquet")

#---- Displaying period, ----


out_path <- file.path(database$database$vmart$segment$age_group)
file_name_to_save <- paste0(current_date, "_CUSTOMER_AGE_GROUP.parquet")

dt_dob <- dt_dob[, c("Customer_Mobile", "Date_Of_Birth")]

dt_dob$current_date <- Sys.Date()
dt_dob$age <- as.integer(difftime(dt_dob$current_date, dt_dob$Date_Of_Birth, units = "weeks") / 52.25)
 
# Categorize ages into groups
dt_dob[, seg_age_group := cut(
  age,
  breaks = c(-Inf, 18, 25, 30, 35, 40, 45, 50, 55, 60, Inf),  # Define the break points
  labels = c("0-18", "19-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55","55-60","60+"),  # Define the labels
  right = TRUE,  # Include the right endpoint in the intervals
  include.lowest = TRUE  # Include the lowest endpoint in the intervals
)]

dt_dob <- dt_dob[!(is.na(dt_dob$seg_age_group)), ]

# check mobile number
dt_dob <- mob_check(dt_dob, "Customer_Mobile")

# Removing duplicated records
dt_dob <- dt_dob[!duplicated(Customer_Mobile), ]

dt_dob <- dt_dob[, c("Customer_Mobile", "seg_age_group"), with = FALSE]

# set datatype 
dt_dob$Customer_Mobile <- as.character(dt_dob$Customer_Mobile)
dt_dob$seg_age_group <- as.character(dt_dob$seg_age_group)

dt_dob <- dt_dob[!(duplicated(dt_dob$Customer_Mobile)),]
# total number of customers belongs to discount tier segment.
nrow(dt_dob)

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_dob, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_AGE_GROUP"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_AGE_GROUP"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_AGE_GROUP")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_AGE_GROUP")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_AGE_GROUP")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: age_group_segment.R", type = "text")
