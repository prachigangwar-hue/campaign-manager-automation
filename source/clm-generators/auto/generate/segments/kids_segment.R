#' Date: 28MAR2023 Time: 03:00PM
#' Author: Archana Pandita

#' Objective: automate kids segment generation
#' family: who bought only kids as a separate category in last two years
#' if divisions in ('kids') 

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
path_data <- file.path(database$database$vmart$rfm$customer$division$offline$tagged$day)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----

# Considering period from 01 apr 2020 to 31 mar 2022
end_date <- Sys.Date() - 1
start_date <- seq(end_date, length = 2, by = "-2 years")[2]

day <- REQUIRED_PERIODS(start_date,end_date)

# read rfm data from folder
dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
  selCols = c(
    "Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Net_Amount_SUM_Boys",
    "Net_Amount_SUM_Girls", "Net_Amount_SUM_Infant",   "Net_Amount_SUM_Toys N Games" 
  )
)

#---- Displaying period, ----
# get the minimum and maximum dates as the period
# set output file name and path
mt_date <- data.table()
setDT(mt_date)
mt_date$f_txn <- as.data.table(unique(dt_rfm$First_Transaction_Date))
mt_date$l_txn <- as.data.table(unique(dt_rfm$Last_Transaction_Date))

mt_date$f_txn <- as.Date(mt_date$f_txn, "%d-%b-%y")
mt_date$l_txn <- as.Date(mt_date$l_txn, "%d-%b-%y")
min_date <- toupper(format(as.Date(min(mt_date[["f_txn"]])), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(mt_date[["l_txn"]])), "%d%b%Y"))

out_path <- file.path(database$database$vmart$segment$kids)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_KIDS.parquet")

# set fixed column names for data
fixed_columns <- c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date") 

# get column names except fixed ones
old_names <- names(dt_rfm[, !fixed_columns, with = FALSE])

# get new columns name
new_names <- gsub(".*_", "", old_names)

# replace old column names with new one
colnames(dt_rfm)[!colnames(dt_rfm) %in% fixed_columns] <- new_names

# set selected columns for output
dt_rfm <- dt_rfm[, !c("Store"), with = FALSE] 

# group on customer mobile number
dt_rfm <- dt_rfm[, .(
  Boys = sum(Boys, na.rm = TRUE),
  Girls = sum(Girls, na.rm = TRUE),
  Infant = sum(Infant, na.rm = TRUE),
  `Toys N Games` = sum(`Toys N Games`, na.rm = TRUE)
), .(Customer_Mobile)]

dt_rfm$Kids <- dt_rfm$Boys + dt_rfm$Girls + dt_rfm$Infant + dt_rfm$`Toys N Games`

# set value of columns except fixed one's if value is >=1 then 1 else 0
dt_rfm$Kids[dt_rfm$Kids >= 1] <- 1
dt_rfm$Kids[dt_rfm$Kids != 1] <- 0
  
# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

# create new column as kids
dt_rfm$seg_kids <- 0

# set value in kids according to mentioned condition
dt_rfm$seg_kids[dt_rfm$Kids == 1] <- 1

dt_rfm <- dt_rfm[dt_rfm$seg_kids == 1, ]

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# set datatype of customer mobile
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile)

dt_rfm <- dt_rfm[, c("Customer_Mobile", "seg_kids"), with = FALSE]

#set datatype of customer mobile 
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile) 
dt_rfm$seg_kids <- as.character(dt_rfm$seg_kids) 
#total number of customers belongs to kids segment.
nrow(dt_rfm)

# view list of files in particular location
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_KIDS"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_KIDS"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_KIDS")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_KIDS")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_KIDS")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: kids_segment.R", type = "text")
