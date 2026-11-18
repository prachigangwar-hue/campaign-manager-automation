#' Date: 07MAR2023 Time: 03:00PM
#' Author: Archana Pandita

#' Objective: automate discount tier segment generation
#'  discount : who availed in ranges of ((0-5)%,(6-10)%,(11-15)%...,(96-100)%) average discount in their visits since 2018
#' discount % = sum(sale value) /sum(discount)
#' if discount % >= 40% then discount_seeker

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
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----
# Considering period from 01 jan 2018 to today -1
end_date <- Sys.Date() - 2
start_date <- as.Date("2018-01-01")

# day <- REQUIRED_PERIODS(start_date, end_date)
day <- paste0(toupper(format(seq.Date(from = start_date,to = end_date,by = "month"),"%b%Y")),
              collapse="|")

# read rfm data from folder
dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = FALSE,
  selCols = c("Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date", "Net_Amount_SUM_Total", "Total_Discount_Amount_SUM_Total", "Promo_Amount_SUM_Total")
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

out_path <- file.path(database$database$vmart$segment$discount_tier)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_DISCOUNT_TIER.parquet")

# set fixed column names for data
fixed_columns <- c("Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date")

# replace old column names with new one

colnames(dt_rfm)[!colnames(dt_rfm) %in% fixed_columns] <- c("sale_value", "discount", "promo")

# discount is discount + promo
dt_rfm$discount_promo <- dt_rfm$discount + dt_rfm$promo

# group by for getting sale value and discount of purchasing since 2018
dt_rfm <- dt_rfm[, .(
  sale_value = sum(sale_value, na.rm = TRUE),
  discount = sum(discount_promo, na.rm = TRUE)
), .(Customer_Mobile)]

# sales value and discount should be >= 0
dt_rfm <- dt_rfm[(dt_rfm$sale_value >= 0 & dt_rfm$discount >= 0), ]

dt_rfm <- dt_rfm[(dt_rfm$sale_value > dt_rfm$discount),]

# group by for getting discount %
dt_rfm <- dt_rfm[, .(
  discount_perc = round(((discount / sale_value)*100), 0)
), .(Customer_Mobile)]

# create groups
dt_rfm$seg_discount_tier <- cut(dt_rfm$discount_perc,
  breaks = c(-0.001, 5, 15, 25, 35, 45, 50, 100),
  labels = c("0-5", "6-15", "16-25", "26-35", "36-45", "46-50", "51-100")
)

dt_rfm <- dt_rfm[!(is.na(dt_rfm$seg_discount_tier)), ]

# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

dt_rfm <- dt_rfm[, c("Customer_Mobile", "seg_discount_tier"), with = FALSE]

# set datatype of customer mobile
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile)
dt_rfm$seg_discount_tier <- as.character(dt_rfm$seg_discount_tier)

# total number of customers belongs to discount tier segment.
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_DISCOUNT_TIER"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_DISCOUNT_TIER"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_DISCOUNT_TIER")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_DISCOUNT_TIER")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_DISCOUNT_TIER")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: discount_tier_segment.R", type = "text")
