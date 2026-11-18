#' Date: 21MAR2023 Time: 11:20PM
#' Author: Archana Pandita

#' Objective: automate vip/most_engaged segment generation
#' vip/most_engaged: customer bills is 4 in fy and purchased at least 3 divisions
#' frequency/bills = count unique(bill date) in fy
#' shopped divisions/divisions = count unique(divisions) in fy
#' if bills >= 4 and divisions >= 3 then vip/most_engaged


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
path_data <- file.path(database$database$vmart$rfm$customer$division$offline$tagged$day)
ids_to_send <- c("745474876")

#---- Function Calls, Data Reading & Manipulations ----
# # Considering period of FY
# today <- Sys.Date() # get current date
# ifelse(today < ymd(paste(year(today), "-04-01")), # check if current date is before or after April 1st
#        start_date <- ymd(paste(year(today) - 1, "-04-01")), # if before, use last year's April 1st
#        start_date <- ymd(paste(year(today), "-04-01"))
# ) # if after, use current year's April 1st
# 
# end_date <- start_date %m+% years(1) - days(1) # get end of FY (subtract 1 day from next year's April 1st)

end_date <- Sys.Date() - 1
start_date <- seq(end_date, length = 2, by = "-1 years")[2]

day <- REQUIRED_PERIODS(start_date, end_date)

# read rfm data from folder
dt_rfm <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE
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

out_path <- file.path(database$database$vmart$segment$vip_most_engaged)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_VIP_MOST_ENGAGED.parquet")

#get women related columns sum
cols_available <- grep("Net_Amount_SUM_Women Ethnic|Net_Amount_SUM_Women Western", names(dt_rfm), value = TRUE)

if(length(cols_available)>0)
{
  # adding columns related women section
  dt_rfm$Net_Amount_SUM_Women <- dt_rfm$`Net_Amount_SUM_Women Ethnic` + dt_rfm$`Net_Amount_SUM_Women Western`
}

#create frequency column
dt_rfm$Frequency <- 1

# set fixed column names for data
fixed_columns <- c("Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date")

# get column names except fixed ones
old_names <- names(dt_rfm[, !fixed_columns, with = FALSE])

# get new columns name
new_names <- gsub(".*_", "", old_names)

# replace old column names with new one
colnames(dt_rfm)[!colnames(dt_rfm) %in% fixed_columns] <- new_names

setDT(dt_rfm)
dt_rfm$Division <- apply(dt_rfm[, c(
  "Boys", "Consumable Goods", "Fixed Assets", "FMCG Food", "FMCG Non Food", "Foot Mart",
  "Girls", "Home Mart", "Infant", "Lifestyle", "Marketing N Advertisement", "Men",
  "Others", "Repair and Maintenance", "Staples", "Toys N Games", "Women"
)], 1, function(x) length(which(x != 0)))

# dt_rfm$Division <- apply(dt_rfm[, -c(
#   "Customer_Mobile","Store", "First_Transaction_Date","Last_Transaction_Date","Total","Frequency","Women Ethnic","Women Western"
# )], 1, function(x) length(which(x != 0)))

# group by for getting frequency of purchasing more than 4 times in a year
dt_rfm <- dt_rfm[, .(
  Frequency = sum(Frequency, na.rm = TRUE),
  Division = sum(Division, na.rm = TRUE),
  Total = sum(Total, na.rm = TRUE)
), .(Customer_Mobile)]

# set value of columns except fixed one's if value is >=4 then 1 else 0
dt_rfm$Frequency[dt_rfm$Frequency >= 4] <- 1
dt_rfm$Frequency[dt_rfm$Frequency != 1] <- 0

# set value of columns except fixed one's if value is >=3 then 1 else 0
dt_rfm$Division[dt_rfm$Division >= 3] <- 1
dt_rfm$Division[dt_rfm$Division != 1] <- 0

# check mobile number
dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

# Removing duplicated records
dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

# create new column as vip_most_engaged
dt_rfm$seg_vip_most_engaged <- 0

# set value in fmcg according to mentioned condition
dt_rfm$seg_vip_most_engaged[((dt_rfm$Frequency == 1) | (dt_rfm$Division == 1))] <- 1

dt_rfm <- dt_rfm[dt_rfm$seg_vip_most_engaged == 1, ]

dt_rfm <- dt_rfm[, c("Customer_Mobile", "seg_vip_most_engaged"), with = FALSE]

#set datatype of customer mobile 
dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile) 
dt_rfm$seg_vip_most_engaged <- as.character(dt_rfm$seg_vip_most_engaged) 
#total number of customers belongs to engaged segment.
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
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VIP_MOST_ENGAGED"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VIP_MOST_ENGAGED"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_VIP_MOST_ENGAGED")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_VIP_MOST_ENGAGED")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_VIP_MOST_ENGAGED")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: vip_most_engaged_segment.R", type = "text")
