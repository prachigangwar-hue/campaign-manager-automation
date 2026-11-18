#' Date: 06FEB2022 Time: 03:00PM
#' Author: Archana Pandita

# Objective: automate abv bucket segment generation
# abv bucket: abv: Net_Amount_SUM_Total/ Bill_No_Total

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
# path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$fy$all)

path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
ids_to_send <- c("745474876")

out_path <- file.path(database$database$vmart$segment$abv_bucket)
#---- Function Calls, Data Reading & Manipulations ----

# read rfm data from folder
dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = "",  recursiveFile = FALSE,
  selCols = c("Customer_Mobile", "Net_Amount_SUM_Total", "Bill_No_Total","Last_Transaction_Date","First_Transaction_Date")
)

#set datatype  of first and last transaction date
dt_rfm$First_Transaction_Date <- as.Date(dt_rfm$First_Transaction_Date, format = "%d-%b-%y")
dt_rfm$Last_Transaction_Date <- as.Date(dt_rfm$Last_Transaction_Date, format = "%d-%b-%y")

min_date <-  toupper(format(as.Date(min(dt_rfm$First_Transaction_Date)), "%d%b%Y"))
max_date <-toupper(format(as.Date(max(dt_rfm$Last_Transaction_Date)), "%d%b%Y"))


#filename to be saved
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_ABV_BUCKET.parquet")

dt_rfm <- dt_rfm[,.("Net_Amount_SUM_Total" = sum(Net_Amount_SUM_Total, na.rm = TRUE), 
                    "Bill_No_Total" = sum(Bill_No_Total, na.rm = TRUE)),.(Customer_Mobile)]
dt_rfm$abv <- round(dt_rfm$Net_Amount_SUM_Total/dt_rfm$Bill_No_Total,0)
dt_rfm$abv[dt_rfm$abv > 5000] <- 5100 
dt_rfm$abv[dt_rfm$abv < 0] <- 0 
dt_rfm$abv_bucket <- cut(dt_rfm$abv, breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5200), dig.lab = 5, include.lowest = TRUE)


# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

#---- Displaying period, ----
dt_rfm <- dt_rfm[,.(Customer_Mobile, abv_bucket)]

#set datatype of customer mobile 
dt_rfm$abv_bucket <- as.character(dt_rfm$abv_bucket) 
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile) 

#total number of customers belongs to abv bucket segment.
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_ABV_BUCKET"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_ABV_BUCKET"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_ABV_BUCKET")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_ABV_BUCKET")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_ABV_BUCKET")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: abv_bucket.R", type = "text")
