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
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----
# Considering period of FY
today <- Sys.Date() # get current date
ifelse(today < ymd(paste(year(today), "-04-01")), # check if current date is before or after April 1st
  start_date <- ymd(paste(year(today) - 1, "-04-01")), # if before, use last year's April 1st
  start_date <- ymd(paste(year(today), "-04-01"))
) # if after, use current year's April 1st

end_date <- start_date %m+% years(1) - days(1) # get end of FY (subtract 1 day from next year's April 1st)

day <- REQUIRED_PERIODS(start_date, end_date)

# read rfm data from folder
# dt_rfm <- READ_FOLDER(
#   pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
#   selCols = c("Frequency", "Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date")
# )

dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet",  
  selCols = c("Frequency", "Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date")
)

setDT(dt_rfm)

dt_rfm <- dt_rfm[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Frequency" = as.numeric(Frequency),
  "First_Transaction_Date" = as.Date(First_Transaction_Date,"%d-%b-%y"), "Last_Transaction_Date" = as.Date(Last_Transaction_Date,"%d-%b-%y")
)]

#---- Displaying period, ----
# get the minimum and maximum dates as the period
# set output file name and path
mt_date <- data.table()
setDT(mt_date)
mt_date$f_txn <- as.data.table(unique(dt_rfm$First_Transaction_Date))
mt_date$l_txn <- as.data.table(unique(dt_rfm$Last_Transaction_Date))

mt_date$f_txn <- as.Date(mt_date$f_txn, "%d-%b-%y")
mt_date$l_txn <- as.Date(mt_date$l_txn, "%d-%b-%y")
min_date <- toupper(format(as.Date(min(mt_date[["f_txn"]])), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(mt_date[["l_txn"]])), "%d%b%Y"))

out_path <- file.path(database$database$vmart$segment$frequency)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_FREQUENCY.parquet")

setDT(dt_rfm)


# Calculate the maximum last_txn_date for each customer
max_dates <- dt_rfm[, .(max_date = max(Last_Transaction_Date)), by = Customer_Mobile]

# Join the data table to itself on Customer_Mobile and filter by date range
dt_rfm <- dt_rfm[max_dates, on = "Customer_Mobile"][Last_Transaction_Date >= max_date - 365 & Last_Transaction_Date <= max_date]

# group by for getting frequency of purchasing more than 1,2,3,4,>4 times in a year
dt_rfm <- dt_rfm[, .(
  Frequency = sum(Frequency, na.rm = TRUE)
), .(Customer_Mobile)]

# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

# create new column as high_frequency
dt_rfm$Frequency[dt_rfm$Frequency > 4] <- ">4"

# changinf frequency name
setnames(dt_rfm,old = "Frequency",new = "frequency")

#set datatype of customer mobile
dt_rfm$frequency <- as.character(dt_rfm$frequency) 
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile) 

#total number of customers belongs to  frequency segment.
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_FREQUENCY"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_FREQUENCY"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_FREQUENCY")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_FREQUENCY")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_FREQUENCY")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: frequency.R", type = "text")
