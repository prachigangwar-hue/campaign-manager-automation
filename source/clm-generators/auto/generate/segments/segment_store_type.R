#' Date: 19APR2023 Time: 03:00PM
#' Author: Archana Pandita

#' Objective: automate frequency segment generation
#' frequency: 

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(lubridate)
# sourcing external file"
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
path_store_mstr <- file.path(path_dbase,"mstr","store_auto")

mt_rfm <- READ_FOLDER(pathFold = path_data,patternFile = "parquet",selCols = c("Customer_Mobile","Store"))
mt_store <- READ_FOLDER(pathFold = path_store_mstr,patternFile = "csv",selCols = c("store","type"))

mt_rfm <- left_join(mt_rfm,mt_store,by = c("Store" = "store"))
mt_rfm[is.na(type)]$type <- "Fashion"
mt_rfm <- mt_rfm[,.(Customer_Mobile,"seg_store_type" = type)]
mt_rfm$Customer_Mobile <- as.character(mt_rfm$Customer_Mobile )

out_path <- file.path(path_dbase,"seg","seg_store_type")
file_name_to_save <- paste0("Segment_Store_Type.parquet")

arrow::write_parquet(mt_rfm, paste0(out_path, "/", file_name_to_save))
