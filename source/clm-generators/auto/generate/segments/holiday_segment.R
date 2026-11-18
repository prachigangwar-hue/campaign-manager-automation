#' Date: 10MAY2023 Time: 9:00AM
#' Author: Archana Pandita

# Objective: automate holiday segment
#----Cleaning Enviornment----
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
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")
path_lib <- file.path(path_proj, "auto/library")

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))
source(file.path(path_lib, "break_and_save_function(BREAK_AND_SAVE).R"))

#----  Path/variable declaration----
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)

ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_mt_str <- "D:/database/mstr/store_auto"
path_wedd_holi_str <- "D:/products/clm_generators/input/wedding_and_holiday_stores_list"

# set output path
out_path <- file.path(database$database$vmart$segment$storewise_holiday)

# read rfm data
mt_rfm <- READ_FOLDER(pathFold = path_data, patternFile = "parquet", selCols = c("Customer_Mobile", "Store", "Last_Transaction_Date"))

setDT(mt_rfm)
mt_rfm <- mt_rfm[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
  "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y")
)]

# reading store master
mt_str <- READ_FOLDER(path_mt_str, patternFile = "csv", contain = "", selCols = c("store_code", "store"))
mt_str <- mt_str[, .("Store_code" = store_code, "Store" = store)]

mt_rfm <- left_join(mt_rfm, mt_str, by = "Store")

# @action: eliminate manual fill of the store code
mt_rfm$Store_code[mt_rfm$Store == "DHANBAD-BINOD BIHARI CHOW"] <- "101667"
mt_rfm$Store_code[mt_rfm$Store == "DEHRI ON SANE"] <- "101446"

# rename store values as its actual name
mt_rfm$Store[mt_rfm$Store_code == "101667"] <- "DHANBAD-BINODBIHARI CHOWK"
mt_rfm$Store[mt_rfm$Store_code == "101446"] <- "DEHRI ON SONE"

#----------

mt_rfm_final <- mt_rfm[, .(Last_Transaction_Date = max(Last_Transaction_Date, na.rm = TRUE)), .(Customer_Mobile, Store, Store_code)]


# Order the data.table by Last_Transaction_Date in descending order
setorder(mt_rfm_final, Customer_Mobile, -Last_Transaction_Date)

# Use the unique() and .SD functions to select the top row for each customer
mt_rfm_final <- mt_rfm_final[, .SD[1], by = Customer_Mobile]
#--------

# read wedding stores
mt_holiday_str <- READ_FOLDER(pathFold = path_wedd_holi_str, patternFile = "csv", selCols = c("Store Code", "Site", "Revised AM Window"))

setDT(mt_holiday_str)
mt_holiday_str <- mt_holiday_str[mt_holiday_str$`Revised AM Window` == "Holiday"]

# get details for holiday customers only
mt_holiday_data <- mt_rfm_final[mt_rfm_final$Store_code %in% mt_holiday_str$`Store Code`, ]
setDT(mt_holiday_data)

# check mobile number
mt_holiday_data <- mob_check(mt_holiday_data, "Customer_Mobile")

# Removing duplicated records
mt_holiday_data <- mt_holiday_data[!duplicated(mt_holiday_data), ]

# create new column as holiday
mt_holiday_data$seg_holiday <- 1

# final output columns
mt_holiday_data <- mt_holiday_data[, c("Customer_Mobile", "Store", "seg_holiday")]

mt_holiday_data$seg_holiday <- as.character(mt_holiday_data$seg_holiday)

# filename to be saved
file_name_to_save <- paste0("STOREWISE_HOLIDAY_CUSTOMER.parquet")
# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))

  # file saved in particular path
  arrow::write_parquet(mt_holiday_data, paste0(out_path, "/", file_name_to_save))

  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_HOLIDAY"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_HOLIDAY"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_HOLIDAY")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_HOLIDAY")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_HOLIDAY")
)
