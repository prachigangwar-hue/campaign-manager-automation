#' Date: 23MAY2023 Time: 10:50AM
#' Author: Archana Pandita

#' Objective: automate high value high frequency segment generation
#' high value high frequency: whom frequency >=3 & average customer value(acv) >= 2500 in last year 
#' visits = count unique(bill date) in current fy
#' abv = sum(sale value) / uniuqe count(bills) in current fy
#' if visits >= 3 & abv >= 2500 then high value one timer

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
path_mt_str <- "D:/database/mstr/store_auto"
#---- Function Calls, Data Reading & Manipulations ----

# Considering period of FY
# today <- Sys.Date() # get current date
# ifelse(today < ymd(paste(year(today), '-04-01')), # check if current date is before or after April 1st
#        start_date <- ymd(paste(year(today) - 1, '-04-01')), # if before, use last year's April 1st
#        start_date <- ymd(paste(year(today), '-04-01'))) # if after, use current year's April 1st
# 
# end_date <- start_date %m+% years(1) - days(1) # get end of FY (subtract 1 day from next year's April 1st)


# Considering period of last year
end_date <- Sys.Date() - 1
start_date <- seq(end_date, length = 2, by = "-1 years")[2]


day <- REQUIRED_PERIODS(start_date,end_date)

# read rfm data from folder
mt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
  selCols = c(
    "Customer_Mobile","Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency","Bill_No_Total", "Net_Amount_SUM_Total"
  )
)

setDT(mt_rfm)
mt_rfm <- mt_rfm[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
  "First_Transaction_Date" = as.Date(First_Transaction_Date, "%d-%b-%y"),
  "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y"),
  "Frequency"= as.numeric(Frequency),"Bill_No_Total"= as.numeric(Bill_No_Total),
  "Net_Amount_SUM_Total"= as.numeric(Net_Amount_SUM_Total)
)]


# reading store master
mt_str <- READ_FOLDER(path_mt_str, patternFile = "csv", contain = "", selCols = c("store_code", "store"))
mt_str <- mt_str[, .("Store_code" = store_code, "Store" = store)]

mt_rfm <- left_join(mt_rfm, mt_str, by = "Store")

# @action: eliminate manual fill of the store code
mt_rfm$Store_code[mt_rfm$Store == "DHANBAD-BINOD BIHARI CHOW"] <- "101667"
mt_rfm$Store_code[mt_rfm$Store == "DEHRI ON SANE"] <- "101446"

# rename store values as its actual name
mt_rfm$Store[mt_rfm$Store_code == "101667"] <- "DHANBAD-BINODBIHARI CHOWK"
mt_rfm$Store[mt_rfm$Store_code == "101446"] <- "DEHRI ON SONE"

#---- Displaying period, ----
# get the minimum and maximum dates as the period
# set output file name and path
mt_date <- data.table()
setDT(mt_date)
mt_date$f_txn <- as.data.table(unique(mt_rfm$First_Transaction_Date))
mt_date$l_txn <- as.data.table(unique(mt_rfm$Last_Transaction_Date))

mt_date$f_txn <- as.Date(mt_date$f_txn, "%d-%b-%y")
mt_date$l_txn <- as.Date(mt_date$l_txn, "%d-%b-%y")
min_date <- toupper(format(as.Date(min(mt_date[["f_txn"]])), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(mt_date[["l_txn"]])), "%d%b%Y"))

out_path <-  file.path(database$database$vmart$segment$high_value_high_freq)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_HIGH_VALUE_HIGH_FREQUENCY.parquet")

# group by for getting sum of total and frequency 
mt_rfm <- mt_rfm[, .(
  First_Transaction_Date= min(First_Transaction_Date ,na.rm = TRUE),
  Last_Transaction_Date  = max(Last_Transaction_Date ,na.rm = TRUE),
  Frequency = sum(Frequency, na.rm = TRUE),
  Bill_No_Total = sum(Bill_No_Total, na.rm = TRUE),
  Net_Amount_SUM_Total  = sum(Net_Amount_SUM_Total , na.rm = TRUE)
), .(Customer_Mobile,Store,Store_code )]

# Order the data.table by First_Transaction_Date in descending order
setorder(mt_rfm, Customer_Mobile, First_Transaction_Date)

# Use the unique() and .SD functions to select the top row for each customer
mt_rfm <- mt_rfm[, .SD[1], by = Customer_Mobile]

#filter for frequency
mt_rfm <- mt_rfm[mt_rfm$Frequency >= 3]

#create abv column
mt_rfm[, Average_Bill_Value := Net_Amount_SUM_Total / Bill_No_Total]

#calculate recency
mt_rfm$Last_Transaction_Date <- as.Date(mt_rfm$Last_Transaction_Date, "%d-%b-%y")
mt_rfm$days_recency <- as.numeric(Sys.Date() - 7 - mt_rfm$Last_Transaction_Date)

#calculate latency
mt_rfm$latency <- (length(strsplit(day, "\\|")[[1]]))/mt_rfm$Frequency

#calculate lbr
mt_rfm$lbr <- mt_rfm$days_recency / mt_rfm$latency

#get only those value whose lbr is in between 0.95 to 1.25
mt_rfm <- mt_rfm[lbr >= 0.95 & lbr <= 1.25, .SD]

# check mobile number
mt_rfm <- mob_check(mt_rfm, "Customer_Mobile")

# Removing duplicated records
mt_rfm <- mt_rfm[!duplicated(mt_rfm), ]

# create new column as high_value_high_freq
mt_rfm$high_value_high_freq <- 0

# set value in fmcg according to mentioned condition
mt_rfm$high_value_high_freq[ (mt_rfm$Average_Bill_Value >= 2500)] <- 1

mt_rfm <- mt_rfm[mt_rfm$high_value_high_freq == 1, ]

mt_rfm <- mt_rfm[, c("Customer_Mobile","Store" ,"Store_code", "high_value_high_freq"), with = FALSE]

# set datatype of customer mobile
mt_rfm$Customer_Mobile <- as.character(mt_rfm$Customer_Mobile)
mt_rfm$Store <- as.character(mt_rfm$Store)
mt_rfm$Store_code <- as.character(mt_rfm$Store_code)
mt_rfm$high_value_high_freq <- as.character(mt_rfm$high_value_high_freq)

# total number of customers belongs to high value hogh freq segment.
nrow(mt_rfm)

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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_HIGH_VALUE_HIGH_FREQUENCY"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_HIGH_VALUE_HIGH_FREQUENCY"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_HIGH_VALUE_HIGH_FREQUENCY")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_HIGH_VALUE_HIGH_FREQUENCY")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_HIGH_VALUE_HIGH_FREQUENCY")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: high_value_high_freq_segment.R", type = "text")
