# Date: 22/09/2025
# Author: Abhijit Nandi

# Objective: automate Most preferred Division for a customer
# Rule : For a division to be most preffered Division
#Qty wise Contribution >=70% for that division
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
path_txn <- file.path(path_dbase,"txn/offline/generate/day")

cur_date <- toupper(format(as.Date(Sys.Date()), "%d%b%Y"))
out_path <- file.path(path_dbase,"seg","seg_most_preferred_division")
file_name_to_save <- paste0(cur_date, "_seg_most_preferred_division.parquet")


source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

cur_date <- toupper(format(as.Date(Sys.Date()), "%d%b%Y"))

start_date <- Sys.Date()-730
end_date <- Sys.Date()-1

dt_txn <- READ_FOLDER(pathFold = path_txn,patternFile = "parquet",
                       contain = REQUIRED_PERIODS(start_period = start_date,end_period = end_date),
                      selCols = c("Customer_Mobile","Division_Group","Division","Bill_Qty_SUM"))

dt_txn <- dt_txn[Division_Group=="1.Apparels" | Division_Group=="2.Non Apparels" ]
dt_txn$Division_Group <- NULL
dt_txn$Division <- ifelse(dt_txn$Division %in% c("Women Ethnic","Women Western"),"Women",dt_txn$Division)
dt_txn <- mob_check(dt_txn,var = "Customer_Mobile")
dt_txn <- dt_txn[,.("Bill_Qty" = sum(Bill_Qty_SUM)),.(Customer_Mobile,Division)]
dt_txn<-  dt_txn[,"Total_Qty" := sum(Bill_Qty),.(Customer_Mobile)]
dt_txn <- dt_txn[,"percentage_contribution" := (Bill_Qty/Total_Qty)]
dt_txn <- dt_txn[order(-dt_txn$Customer_Mobile,-dt_txn$percentage_contribution)]
# dt_txn <- dt_txn[percentage_contribution>=0.7]
dt_txn <- dt_txn[!duplicated(Customer_Mobile)]

setnames(dt_txn, "Division", "seg_most_preferred_division")
dt_txn <- dt_txn[,.(Customer_Mobile,seg_most_preferred_division)]

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_txn, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_MOST_PREFERRED_DIVISION"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_MOST_PREFERRED_DIVISION"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_MOST_PREFERRED_DIVISION")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_MOST_PREFERRED_DIVISION")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_MOST_PREFERRED_DIVISION")
)
