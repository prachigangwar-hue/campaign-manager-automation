# Date: 13MAR2023 11:00 AM
# Author: Vmart Retail (Archana Pandita)

# Objective: automate consolidate of multiple customer segment generation

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

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

#----  Path/variable declaration----
cur_date <- toupper(format(as.Date(Sys.Date()), "%d%b%Y"))
out_path <- file.path(database$database$vmart$segment$Segment)
file_name_to_save <- paste0(cur_date, "_RFM.parquet")
folder_path <- file.path(database$database$vmart$segment)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----

# create a function to read a single parquet file and set the first column as character
read_parquet_file <- function(filename) {
  dt <- arrow::read_parquet(filename)
  setDT(dt)
  setnames(dt, c("Customer_Mobile", names(dt)[-1]))
  dt[, Customer_Mobile := as.character(Customer_Mobile)]
  return(dt)
}

latest_files <- list()
for (i in seq_along(folder_path)) {
  seg_lst <- c("customer_type", "abv_bucket", "frequency")
  seg_lst <- paste(seg_lst, collapse = "|")
  # exclude the "high_frequency" directory using grep
  folder_path <- folder_path[!grepl("/high_frequency$", folder_path)]

  # include the "seg_lst" directory using grep
  folder_path <- folder_path[grepl(seg_lst, folder_path)]

  # Get a list of all the parquet files in the subfolder
  parquet_files <- list.files(folder_path[i], pattern = "\\.parquet$", full.names = TRUE)

  # Get the latest file based on its modification time
  latest_file <- parquet_files[which.max(file.info(parquet_files)$mtime)]

  # Store the latest file in the list
  if (length(parquet_files) > 0) {
    latest_files[[i]] <- latest_file
  }
}

# read in all the parquet files using the function and store them in a list
dt_rfm <- lapply(latest_files, read_parquet_file)

# consolidate the data using a full join
mt_rfm <- reduce(dt_rfm, full_join, by = "Customer_Mobile")

# replace na with "p" for customer_type
mt_rfm$customer_type <- as.character(mt_rfm$customer_type)
mt_rfm$customer_type[is.na(mt_rfm$customer_type)] <- "p"

# replace na with 0 for frequency
mt_rfm$frequency <- as.character(mt_rfm$frequency)
mt_rfm$frequency[is.na(mt_rfm$frequency)] <- "0"

# replace na with 0 for abv_bucket
mt_rfm$abv_bucket <- as.character(mt_rfm$abv_bucket)
mt_rfm[, abv_bucket := ifelse(is.na(abv_bucket), 0, abv_bucket)]
# mt_rfm$abv_bucket[mt_rfm$abv_bucket == "NA"] <- "[0,0]"

# Subset the data table to remove rows containing NA values
# mt_rfm <- mt_rfm[complete.cases(mt_rfm)]

# check mobile number
mt_rfm <- mob_check(mt_rfm, "Customer_Mobile")

# remove duplicates
mt_rfm <- mt_rfm[!(duplicated(mt_rfm)), ]

# get rfm customers
mt_rfm$segment <- paste0(mt_rfm$customer_type, "_", mt_rfm$frequency, "_", mt_rfm$abv_bucket)

# remove columns if it contains NA
# mt_rfm <- mt_rfm[!grepl("NA", mt_rfm$segment)]

mt_rfm <- mt_rfm[, c("Customer_Mobile", "segment"), with = FALSE]

# set datatype of customer mobile
mt_rfm$Customer_Mobile <- as.character(mt_rfm$Customer_Mobile)

# total number of customers belongs to  frequency segment.
nrow(mt_rfm)

# set all columns as character type
mt_rfm[, names(mt_rfm) := lapply(.SD, as.character)]

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))

  # file saved in particular path
  arrow::write_parquet(mt_rfm, paste0(out_path, "/", file_name_to_save))

  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_RFM"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_RFM"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_RFM")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_RFM")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_RFM")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: Segment_rfm.R", type = "text")
