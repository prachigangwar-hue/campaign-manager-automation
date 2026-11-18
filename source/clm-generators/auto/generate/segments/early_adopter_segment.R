#' Date: 28MAR2023 Time: 09:30PM
#' Author: Archana Pandita

# Objective: automate early adopter segment generation
#  early adopter: who shopped fresh_summer_product of summer_season during launch_summer_months in atleast one of the selected_years
# OR
# shopped fresh_winter_product of winter_season during launch_winter_months in atleast one of the selected_years

# selected_years  = Bill_Date  in (2018,2019)
# summer_season = Attribute2 in ('SUMMER')
# winter_season = Attribute2 in ('WINTER')
# fresh_summer_product = Desc2 in ('SS18', 'SS19', 'AW18', 'AW19')
# launch summer months = 01FEB to 31MAR
# launch winter months = 01OCT to 30NOV

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
years <- c(year(Sys.Date()) - 1, year(Sys.Date()) - 2)

# Create a vector of starting and ending dates
start_dates <- c(paste0(years[2], "-02-01"), paste0(years[1], "-02-01"), paste0(years[2], "-10-01"), paste0(years[1], "-10-01"))
end_dates <- c(paste0(years[2], "-03-31"), paste0(years[1], "-03-31"), paste0(years[2], "-11-30"), paste0(years[1], "-11-30"))

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
  pathFold = path_data, patternFile = "parquet", contain = paste0(toupper(format(day, "%d%b%Y")), collapse = "|"), recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "DESC2", "Bill_Qty_SUM")
)

#---- Displaying period, ----
# get the minimum and maximum dates as the period
# set output file name and path

mt_date <- data.table()
setDT(mt_date)
mt_date$bill_dt <- as.data.table(unique(dt_bill$Bill_Date))
mt_date$bill_dt <- as.Date(mt_date$bill_dt, "%d-%b-%y")

min_date <- toupper(format(as.Date(min(mt_date[["bill_dt"]])), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(mt_date[["bill_dt"]])), "%d%b%Y"))

out_path <-  file.path(database$database$vmart$segment$early_adopter)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_EARLY_ADOPTER.parquet")

# filter for summer/winter season
dt_bill <- dt_bill[dt_bill$INVATTR2_NAME %in% c("SUMMER", "WINTER"), ]

# filter for fresh product for summer/winter season
desc <- c("SS", "AW")

desc_list <- sapply(1:length(desc), function(x) {
  lapply(1:length(years), function(y) {
    paste0(desc[x], substr(years[y], 3, 4))
  })
})
desc_list <- unlist(unlist(desc_list))

dt_bill <- dt_bill[dt_bill$DESC2 %in% desc_list, ]

# filter for Bill_Qty_SUM > 0
dt_bill <- dt_bill[Bill_Qty_SUM > 0, ]

# Removing duplicated records
dt_bill <- dt_bill[!duplicated(Customer_Mobile), ]

# check mobile number
dt_bill <- mob_check(dt_bill, "Customer_Mobile")

# set datatype of customer mobile
dt_bill$Customer_Mobile <- as.character(dt_bill$Customer_Mobile)
dt_bill$seg_early_adopter <- as.character(dt_bill$seg_early_adopter)

# create new column as early adopter
dt_bill$seg_early_adopter <- 1

dt_bill <- dt_bill[dt_bill$seg_early_adopter == 1, ]

dt_bill <- dt_bill[, c("Customer_Mobile", "seg_early_adopter"), with = FALSE]

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_bill, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EARLY_ADOPTER")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: early_adopter_segment.R", type = "text")
