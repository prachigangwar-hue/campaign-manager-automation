# Season launch loyalist : customers who shopped in Feb'24 to May'24 with frequency >2 
#' Date: 24SEP2022 Time: 03:00PM
#' Author: Archana Pandita

#' Objective: automate Season launch loyalist SU25 and AW24 with frequency >2 
#' Buyers who bought in first 120 days of season launch (winter'24 and summer '24) WI24 & SS24 with frequency >=2

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
path_data <- file.path(database$database$transaction$offline)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----

# Considering period from 01 jun 2021 to till date
end_date <- Sys.Date() - 1
start_date <- seq(end_date, length = 2, by = "-3 years")[2]

day <- REQUIRED_PERIODS(start_date,end_date)

# read mt_txn data from folder
mt_txn <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
  selCols = c(
    "Customer_Mobile",   "Bill_Date", "DESC2","Region"
  )
)

setDT(mt_txn)
mt_txn <- mt_txn[, .(
  "Customer_Mobile" = as.character(Customer_Mobile),  
  "Bill_Date" = as.Date(Bill_Date, "%d-%b-%y"),
  "DESC2" = as.character(DESC2),
  "Region" = as.character(Region)
)]
#---- Displaying period, ----
# set output file name and path
out_path <- "D:/database/seg/season_launch_loyalist"
file_name_to_save <- paste0("CUSTOMER_SEASON_LAUNCH_LOYALIST.parquet")

#get min date for winter 24/25 season launch
mt_txn_winter_all <-mt_txn[mt_txn$DESC2=="WI24" |mt_txn$DESC2=="WI25" ,] 
mt_txn_winter <- mt_txn_winter_all

#get min date for summer 25/26 season launch
mt_txn_summer_all <-mt_txn[mt_txn$DESC2=="SS25"|mt_txn$DESC2=="SS26",] 
mt_txn_summer <- mt_txn_summer_all

mt_txn_summer <- mt_txn_summer[!(mt_txn_summer$Region==""),]
mt_txn_summer <- mt_txn_summer[!(is.na(mt_txn_summer$Region)),]

#SUMMER
unique_Region_summer <- unique(mt_txn_summer$Region)

mt_summer <- list()
for (i in 1:length(unique_Region_summer)) {
  print(unique_Region_summer[i])
  data <- mt_txn_summer[mt_txn_summer$Region == unique_Region_summer[i],]
  min_dt_summer <- min(data$Bill_Date)
  print(min_dt_summer)
  
  summer <- mt_txn_summer[(mt_txn_summer$Bill_Date >= min_dt_summer &  mt_txn_summer$Bill_Date <= (min_dt_summer + 120)),]
  setDT(summer)
  
  # Add a frequency column based on unique Bill_Date
  summer[, Frequency := uniqueN(Bill_Date), by = Customer_Mobile]
  summer <- summer[summer$Frequency >= 2,]
  summer <- summer[,c("Customer_Mobile")]
  summer <- summer[!(duplicated(summer)),]
  mt_summer[[i]] <- summer
}
su24 <- rbindlist(mt_summer)

#WINTER

setDT(mt_txn_winter)
mt_txn_winter[, Region := ifelse(Region %in% c("IHAR 2B", "R 2BIHA"), "BIHAR 2", Region)]  
mt_txn_winter <- mt_txn_winter[!(mt_txn_winter$Region==""),]
mt_txn_winter <- mt_txn_winter[!(is.na(mt_txn_winter$Region)),]

unique_Region_winter <- unique(mt_txn_winter$Region)

mt_winter <- list()
for (i in 1:length(unique_Region_winter)) {
  data <- mt_txn_winter[mt_txn_winter$Region==unique_Region_winter[i],]
  min_dt_winter <- min(data$Bill_Date)
  print(min_dt_winter)
  
  winter <- mt_txn_winter[(mt_txn_winter$Bill_Date >= min_dt_winter &  mt_txn_winter$Bill_Date <= (min_dt_winter + 120)),]
  setDT(winter)
  
  # Add a frequency column based on unique Bill_Date
  winter[, Frequency := uniqueN(Bill_Date), by = Customer_Mobile]
  winter <- winter[winter$Frequency >= 2,]
  winter <- winter[,c("Customer_Mobile")]
  winter <- winter[!(duplicated(winter)),]
  mt_winter[[i]] <- winter
}
wi24 <- rbindlist(mt_winter)

season_launch_loyalist <- rbindlist(list(su24,wi24))

#remove null add function cm
# check mobile number
season_launch_loyalist <- mob_check(season_launch_loyalist, "Customer_Mobile")

season_launch_loyalist$seg_season_launch_loyalist <- 1

season_launch_loyalist$seg_season_launch_loyalist <- as.character(season_launch_loyalist$seg_season_launch_loyalist)

#total number of customers belongs to family segment.
nrow(season_launch_loyalist)

arrow::write_parquet(season_launch_loyalist, paste0(out_path, "/", file_name_to_save))
# 
# # view list of files in particular location
# files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# 
# # check file exist or not
# file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
# if (!file_status) {
#   files <- list.files(file.path(out_path))
#   
#   # file saved in particular path
#   arrow::write_parquet(season_launch_loyalist, paste0(out_path, "/", file_name_to_save))
#   
#   # remove previous dated files save in folder
#   if (length(list.files(file.path(out_path))) == 2) {
#     file.remove(file.path(out_path, files))
#     LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_CUSTOMER_SEASON_LAUNCH_LOYALIST"))
#   } else {
#     LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_CUSTOMER_SEASON_LAUNCH_LOYALIST"))
#   }
#   LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_CUSTOMER_SEASON_LAUNCH_LOYALIST")
# } else {
#   LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_CUSTOMER_SEASON_LAUNCH_LOYALIST")
# }

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_CUSTOMER_SEASON_LAUNCH_LOYALIST")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: season_launch_loyalist_segment.R", type = "text")
