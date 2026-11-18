#' Date: 28MAR2023 Time: 09:30PM
#' Author: Archana Pandita

# Objective: automate early adopter segment generation
#  early adopter: who shopped fresh_summer_product of summer_season during launch_summer_months in BOTH SELECTED YEARS
# OR
# early_adopter : shopped fresh_winter_product of winter_season during launch_winter_months in BOTH SELECTED YEARS
# seasonal_buying_frequency : no of times bought durng the season in CY

# selected_years  = Bill_Date  in (2018,2019)
# summer_season = Attribute2 in ('SUMMER')
# winter_season = Attribute2 in ('WINTER')

# summer season months = 01FEB to 31OCT
# winter season months = 01NOV to 31JAN

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
# Define a set of different years
years <- c(year(Sys.Date())-1, year(Sys.Date()) - 2,year(Sys.Date()))

# Create a vector of starting and ending dates for summer
start_dates <- c(paste0(years[2], "-11-01"), paste0(years[1], "-11-01"))
end_dates <- c(paste0(years[1], "-01-31"), paste0(years[3], "-01-31"))

# Generate a sequence of dates for each start and end date
date_ranges <- lapply(1:length(start_dates), function(x) {
  seq(from = as.Date(start_dates[x]), to = as.Date(end_dates[x]), by = "days")
})

# Combine the date ranges into a single data frame
day <- lubridate::as_date(
  purrr::simplify(date_ranges) 
)

# read rfm data from folder
dt_bill <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(toupper(format(day, "%d%b%Y")), collapse = "|"), recursiveFile = FALSE,
  selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "DESC2", "Bill_Qty_SUM")
)

#---- Displaying period, ----
# get the minimum and maximum dates as the period
# set output file name and path
dt_bill <- dt_bill[dt_bill$INVATTR2_NAME %in% c("WINTER"), ]
# filter for Bill_Qty_SUM > 0
dt_bill <- dt_bill[Bill_Qty_SUM > 0, ]
dt_bill <- mob_check(dt_bill, "Customer_Mobile")
dt_bill$Bill_Date <- as.Date(dt_bill$Bill_Date,"%d-%b-%y")

dt_ly <- dt_bill[(Bill_Date>=start_dates[1]) & (Bill_Date<=end_dates[1]) ]
dt_cy <- dt_bill[(Bill_Date>=start_dates[2]) & (Bill_Date<=end_dates[2]) ]

dt_early_adopters <- data.table()
dt_early_adopters$Customer_Mobile <- intersect(dt_ly[Bill_Date<=as.Date(start_dates[1])+30]$Customer_Mobile,dt_cy[Bill_Date<=as.Date(start_dates[2])+30]$Customer_Mobile)
dt_early_adopters <- dt_early_adopters[!duplicated(Customer_Mobile), ] 

# create new column as early adopter
dt_early_adopters$seg_season_winter <- "early_adopters"

rm(dt_ly,dt_cy,dt_bill);gc()

dt_cy <- READ_FOLDER(pathFold = path_data, patternFile = "parquet", 
                     contain = REQUIRED_PERIODS(start_period = as.Date(start_dates[2]),end_period = Sys.Date()-1),
                     selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "Bill_Qty_SUM"))

# dt_bill <- dt_bill[dt_bill$INVATTR2_NAME %in% c("WINTER"), ]
# dt_bill <- dt_bill[Bill_Qty_SUM > 0, ]
# dt_bill <- mob_check(dt_bill, "Customer_Mobile")

dt_cy <- dt_cy[!Customer_Mobile %in% dt_early_adopters$Customer_Mobile]
dt_cy <- dt_cy[,.("frequency" = n_distinct(Bill_Date)),.(Customer_Mobile)]
dt_cy$seg_season_winter <- ifelse(dt_cy$frequency>3,'3+',dt_cy$frequency)

dt_early_adopters <- rbind(dt_early_adopters,dt_cy[,.(Customer_Mobile,seg_season_winter)])
dt_early_adopters <- dt_early_adopters[!duplicated(Customer_Mobile)]

# min_date <- toupper(format(as.Date(start_dates[1]),"%d%b%Y"))
# max_date <- toupper(format(as.Date(end_dates[2]),"%d%b%Y"))
# out_path <-  file.path(database$database$vmart$segment$seg_season_winter)
out_path <- file.path(path_dbase,"seg","seg_season_winter")
file_name_to_save <- paste0("SEASON_ADOPTER_WINTER.parquet")

dt_early_adopters$Customer_Mobile <- as.character(dt_early_adopters$Customer_Mobile)
dt_early_adopters$seg_season_winter <- as.character(dt_early_adopters$seg_season_winter)


# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
# file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
arrow::write_parquet(dt_early_adopters, paste0(out_path, "/", file_name_to_save))
# if (!file_status) {
#   files <- list.files(file.path(out_path))
#   
#   # file saved in particular path
#   arrow::write_parquet(dt_early_adopters, paste0(out_path, "/", file_name_to_save))
#   
#   # remove previous dated files save in folder
#   if (length(list.files(file.path(out_path))) == 2) {
#     file.remove(file.path(out_path, files))
#     LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER"))
#   } else {
#     LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER"))
#   }
#   LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER")
# } else {
#   LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER")
# }

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: early_adopter_segment.R", type = "text")
