#' Date: 04MAY2023 Time: 12:00PM
#' Author: Archana Pandita

# Objective: automate active family segment generator
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
path_family_data <- file.path(database$database$vmart$segment$family) 

path_cust_name <- file.path(path_proj, "input", "persistent_data_storewise") 
path_cust_fname <- file.path(path_proj, "input", "persistent_data")

ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/11TG_7c1je4f-rDUB5pKGUJlvIoCiB2o3P3HqKJHFt2M/edit#gid=714304411")
path_act_str <- file.path("https://docs.google.com/spreadsheets/d/1VWHvESic2cQUCsq2wE1lYJ9WVrX6wxUM72m2ZoptuNE/edit#gid=0")
path_act_vmart_str <- file.path("https://docs.google.com/spreadsheets/d/1cmclH9b9k8--yqnOvtXuYWKB8aDyb92Ke4PlzslgN78/edit#gid=0")
path_mt_str <- "D:/database/mstr/store_auto"

# set output path
out_path <- file.path(database$database$vmart$segment$storewise_active_family)

# read family customer last two years
mt_family <- READ_FOLDER(pathFold = path_family_data, patternFile = "parquet")

setDT(mt_family)

# read rfm data
mt_rfm <- READ_FOLDER(pathFold = path_data, patternFile = "parquet",selCols = c("Customer_Mobile", "Store", "Last_Transaction_Date"))

setDT(mt_rfm)

mt_rfm <- mt_rfm[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
  "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y")
)]

# get deatils for family customers only
mt_family_data <- mt_rfm[Customer_Mobile %in% mt_family$Customer_Mobile, ]

setDT(mt_family_data)

# max last transaction date on which customer shopped
mt_family_data <- mt_family_data[, .(Last_Transaction_Date = max(Last_Transaction_Date, na.rm = TRUE)), .(Customer_Mobile, Store)]

# Order the data.table by Last_Transaction_Date in descending order
setorder(mt_family_data, Customer_Mobile, -Last_Transaction_Date)

# Use the unique() and .SD functions to select the top row for each customer
mt_family_data <- mt_family_data[, .SD[1], by = Customer_Mobile]

mt_family_data$days_recency <- as.numeric(Sys.Date() - 7 - mt_family_data$Last_Transaction_Date)
mt_family_data$group <- cut(mt_family_data$days_recency, breaks = c(-Inf, 180), c("a"))

# check mobile number
mt_family_data <- mob_check(mt_family_data, "Customer_Mobile")

# Removing duplicated records
mt_family_data <- mt_family_data[!duplicated(Customer_Mobile), ]

mt_family_data <- mt_family_data[group == "a", ]

# get customer names
mt_cust_fname <- READ_FOLDER(
  pathFold = path_cust_fname, patternFile = "parquet", recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "First_Name")
)

setDT(mt_cust_fname)
mt_cust_fname <- mt_cust_fname[, .("Customer_Mobile" = `Customer_Mobile`, "First_Name" = `First_Name`)]

mt_family_data <- left_join(mt_family_data, mt_cust_fname, by = "Customer_Mobile")

# reading store master

mt_str <- READ_FOLDER(path_mt_str, patternFile = "csv", contain = "", selCols = c("store_code", "store"))
mt_str <- mt_str[, .("Store_code" = store_code, "Store" = store)]

mt_family_data <- left_join(mt_family_data, mt_str, by = "Store")


# filename to be saved
file_name_to_save <- paste0("STOREWISE_ACTIVE_FAMILY_CUSTOMER.parquet")
# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(mt_family_data, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_ACTIVE_FAMILY"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_ACTIVE_FAMILY"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_ACTIVE_FAMILY")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_ACTIVE_FAMILY")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_STOREWISE_ACTIVE_FAMILY")
)

