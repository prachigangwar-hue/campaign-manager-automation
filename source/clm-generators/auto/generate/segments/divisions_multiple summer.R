#' Date: 31MAR2023 Time: 09:00PM
#' Author: Archana Pandita

# Objective: To create a script to segment for divisions_multiple summer
# customer shopped more than 3 apparel divsions in last summer season
# apparel divisions = count unique(men, women, boys, girls, infant)
# if apparel divisions >= 3 in last summer season(mar to oct) then divisions_multiple

#----Cleaning Enviornment----
rm(list = ls())
gc()

t1 <- Sys.time()
d1 <- Sys.Date()

#----Sourcing Libraries and external files----
# loading libraries
library(dreaweRTools)
library(dplyr)
library(data.table)
library(pbapply)
library(kableExtra)
library(janitor)
library(stringr)

# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

path_lib <- file.path(path_proj, "auto/library")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))
source(file.path(path_lib, "log_lib.R"))

#----  Path/variable declaration----
path_data <- file.path(database$database$transaction$offline)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----
# bill date in [2022]{last year}
# Considering period for last summer month from 01 mar to 31 oct
# last summer -> desc2 in ('SS22')
# season - > attribute2 in ('SUMMER')

# Define a last years
year <- year(Sys.Date()) - 1

# Create a vector of starting and ending dates
start_dates <- paste0(year, "-03-01")
end_dates <- paste0(year, "-10-31")

# Generate a sequence of dates for each start and end date
date_ranges <- toupper(format(seq(from = as.Date(start_dates), to = as.Date(end_dates), by = "days"), "%d%b%Y"))

# read rfm data from folder
dt_bill <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(date_ranges, collapse = "|"), recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "DESC2", "Bill_Qty_SUM", "Division_Group", "Division")
)

#---- Displaying period, ----
# get the minimum and maximum dates as the period

date <- data.table()
setDT(date)
date$bill_dt <- as.data.table(unique(dt_bill$Bill_Date))

date$bill_dt <- as.Date(date$bill_dt, "%d-%b-%y")

min_date <- toupper(format(as.Date(min(date[["bill_dt"]])), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(date[["bill_dt"]])), "%d%b%Y"))

# set output file name and path
out_path <- file.path(database$database$vmart$segment$division_multiple_summer)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_DIVISION_MULTIPLE_SUMMER.parquet")

# filter for apparels
dt_bill <- dt_bill[dt_bill$Division_Group == "1.Apparels", ]

# filter for summer season
dt_bill <- dt_bill[dt_bill$INVATTR2_NAME %in% c("SUMMER"), ]

# filter for fresh product for summer season
desc_ss <- paste0("SS", substr(year, 3, 4))

dt_bill <- dt_bill[dt_bill$DESC2 %in% desc_ss, ]

# remove is.na
dt_bill <- dt_bill[!(is.na(dt_bill$Customer_Mobile)), ]

# filter for Bill_Qty_SUM > 0
dt_bill <- dt_bill[Bill_Qty_SUM > 0, ]

# check mobile number
dt_bill <- mob_check(dt_bill, "Customer_Mobile")

# set datatype of customer mobile
dt_bill$Customer_Mobile <- as.character(dt_bill$Customer_Mobile)

# division count
dt_bill <- dt_bill[, .("division_count" = n_distinct(Division, na.rm = TRUE)), .(Customer_Mobile)]

# create new column as divisions_multiple summer
dt_bill$seg_division_multiple_summer <- ifelse(dt_bill$division_count >= 3, 1, 0)

dt_bill <- dt_bill[dt_bill$seg_division_multiple_summer == 1, ]

dt_bill <- dt_bill[, c("Customer_Mobile", "seg_division_multiple_summer"), with = FALSE]

dt_bill$seg_division_multiple_summer <- as.character(dt_bill$seg_division_multiple_summer)

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
    LOG_INFO(
      what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"),
      special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_DIVISION_MULTIPLE_SUMMER")
    )
  } else {
    LOG_INFO(
      what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"),
      special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_DIVISION_MULTIPLE_SUMMER")
    )
  }
  LOG_INFO(
    what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"),
    special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_DIVISION_MULTIPLE_SUMMER"
  )
} else {
  LOG_INFO(
    what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"),
    special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_DIVISION_MULTIPLE_SUMMER"
  )
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_DIVISION_MULTIPLE_SUMMER")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: division_multiple_summer_segment.R", type = "text")
