#' Date: 06FEB2022 Time: 03:00PM
#' Author: Archana Pandita

# Objective: automate new customer segment generation

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
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
ids_to_send <- c("745474876")
path_mt_str <- "D:/database/mstr/store_auto"
out_path <- file.path(database$database$vmart$segment$storewise_store_customer)
path_mt_str_seg <- "D:/products/clm_generators/input/store_master"

#---- Function Calls, Data Reading & Manipulations ----

# read rfm data from folder
mt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = "", recursiveFile = TRUE,
  selCols = c("Customer_Mobile","Store" ,"First_Transaction_Date")
)


setDT(mt_rfm)
mt_rfm <- mt_rfm[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
  "First_Transaction_Date" = as.Date(First_Transaction_Date, "%d-%b-%y"))]

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


min_date <- toupper(format(as.Date(min(mt_rfm$First_Transaction_Date)), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(mt_rfm$First_Transaction_Date)), "%d%b%Y"))

# filename to be saved
file_name_to_save <- paste0(min_date, "-", max_date, "_STORE_CUSTOMER.parquet")

# group by for first txn date per customer
mt_rfm <- mt_rfm[, .(
  First_Transaction_Date = min(First_Transaction_Date, na.rm = TRUE)
), .( Customer_Mobile,Store, Store_code)]


# Order the data.table by First_Transaction_Date in descending order
setorder(mt_rfm, Customer_Mobile, First_Transaction_Date)

# Use the unique() and .SD functions to select the top row for each customer
mt_rfm <- mt_rfm[, .SD[1], by = Customer_Mobile]


mt_str_seg <- READ_FOLDER(path_mt_str_seg, patternFile = "csv", contain = "", selCols = c("Site Code","Store Type (23-24)"))
mt_str_seg <- mt_str_seg[, .("Store_code" =as.character(`Site Code`), "seg_store"=`Store Type (23-24)`)]


dt_rfm <- left_join(mt_str_seg,mt_rfm, by = "Store_code")

dt_rfm <- dt_rfm[,c("Store"  ,"Customer_Mobile" , "seg_store" )]
 
# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

dt_rfm <- dt_rfm[!duplicated(dt_rfm), ]

#---- Displaying period, ---- 

# set datatype of customer mobile
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile)
dt_rfm$Store <- as.character(dt_rfm$Store)
dt_rfm$seg_store <- as.character(dt_rfm$seg_store)

# total number of customers belongs to new customer
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_STORE_CUSTOMER"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_STORE_CUSTOMER"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_STORE_CUSTOMER")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_STORE_CUSTOMER")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_STORE_CUSTOMER")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: store_customer_segment.R", type = "text")
