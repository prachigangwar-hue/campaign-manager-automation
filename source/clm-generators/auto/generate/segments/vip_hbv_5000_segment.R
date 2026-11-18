#' Date: 24SEP2022 Time: 03:00PM
#' Author: Archana Pandita

#' Objective: automate vip/high bill value 5000 segment generation
#' vip/high bill value 5000: customer abv in fy is >= 5000 
#' abv = sum(sale value) / uniuqe count(bills) in current fy
#' if abv >= 5000 then vip_hbv_5000

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(lubridate)
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

#---- Function Calls, Data Reading & Manipulations ----

# Considering period of FY
# today <- Sys.Date() # get current date
# ifelse(today < ymd(paste(year(today), '-04-01')), # check if current date is before or after April 1st
#        start_date <- ymd(paste(year(today) - 1, '-04-01')), # if before, use last year's April 1st
#        start_date <- ymd(paste(year(today), '-04-01'))) # if after, use current year's April 1st
# 
# end_date <- start_date %m+% years(1) - days(1) # get end of FY (subtract 1 day from next year's April 1st)

end_date <- Sys.Date() - 1
start_date <- seq(end_date, length = 2, by = "-1 years")[2]

day <- REQUIRED_PERIODS(start_date,end_date)

# read rfm data from folder
dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
  selCols = c(
    "Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date", "Net_Amount_SUM_Total"
  )
)

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

out_path <-  file.path(database$database$vmart$segment$vip_hbv_5000)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_VIP_HBV_5000.parquet")

# set fixed column names for data
fixed_columns <- c("Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date") 

# get column names except fixed ones
old_names <- names(dt_rfm[, !fixed_columns, with = FALSE])

# get new columns name
new_names <- gsub(".*_", "", old_names)

# replace old column names with new one
colnames(dt_rfm)[!colnames(dt_rfm) %in% fixed_columns] <- new_names

# group by for getting frequency of purchasing more than 3 times in a year
dt_rfm <- dt_rfm[, .(
  Total = sum(Total, na.rm = TRUE)
), .(Customer_Mobile)]

# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

# create new column as vip_hbv_5000
dt_rfm$seg_vip_hbv_5000 <- 0

# set value in fmcg according to mentioned condition
dt_rfm$seg_vip_hbv_5000[dt_rfm$Total >= 5000] <- 1

dt_rfm <- dt_rfm[dt_rfm$seg_vip_hbv_5000 == 1, ]

dt_rfm <- dt_rfm[, c("Customer_Mobile", "seg_vip_hbv_5000"), with = FALSE]

# set datatype of customer mobile
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile)
dt_rfm$seg_vip_hbv_5000 <- as.character(dt_rfm$seg_vip_hbv_5000)

# total number of customers belongs to high value one timer segment.
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VIP_HBV_5000"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VIP_HBV_5000"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_VIP_HBV_5000")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_VIP_HBV_5000")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VIP_HBV_5000")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: vip_hbv_5000_segment.R", type = "text")
