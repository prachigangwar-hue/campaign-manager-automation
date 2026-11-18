#' Date: 014FEB2022 Time: 03:00PM
#' Author: Archana Pandita

# Objective: automate eoss loyal segment generator
# eoss loyal: who shooped >= n/2 times in first all days of last 'n' eoss campaigns
# event count = bill date in (eoss1, eoss2, eoss3, ...,eoss n)
# if event count  >= n/2 then eoss_loyal
#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)
library(tidyverse)

# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_lib <- file.path(path_proj, "auto/library")

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))

#----  Path/variable declaration----
path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/11TG_7c1je4f-rDUB5pKGUJlvIoCiB2o3P3HqKJHFt2M/edit#gid=714304411")

# set output path
out_path <- file.path(database$database$vmart$segment$eoss_loyal)

#---- Function Calls, Data Reading & Manipulations ----
# reading store master to filter active stores
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[mt_event$`event(acronym)` == "end of season sale(EOSS)", ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_EOSS_EARLY")
file_name <- rownames(files)[which.max(files$mtime)]

if (length(file_name) > 0) {
  
  # create column for end period in particular date format
  mt_event$end <- toupper(format(as.Date(mt_event$`end period`), "%d%b%Y"))
  
  # get duration for newly add events
  period <- strsplit(basename(file_name), "_")[[1]][1]
  start_period <- strsplit(period, "-")[[1]][1]
  end_period <- strsplit(period, "-")[[1]][2]
  
  # get last row details on which event already occured
  last_sno <- mt_event[mt_event$end %in% end_period, c("sno")]
  
  # get next rows details on which events will be occured
  new_index <- mt_event[mt_event$sno > as.numeric(last_sno), which = TRUE]
  mt_event <- mt_event[new_index[1]:length(mt_event$sno), ]
}
# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
  toupper(format(as.Date(mt_event[length(mt_event$sno)]$`end period`), "%d%b%Y")), "_CUSTOMER_EOSS_LOYAL.parquet"
)


mt_rfm <- list()
#
for (i in 1:length(mt_event$sno)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)

  # read rfm data from folder
  dt_rfm <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
    selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency")
  )

  #---- Displaying period, ----
  # get the minimum and maximum dates as the period
  if (nrow(dt_rfm) > 0) {
    date <- data.table()
    setDT(date)
    date$f_txn <- as.data.table(unique(dt_rfm$First_Transaction_Date))
    date$l_txn <- as.data.table(unique(dt_rfm$Last_Transaction_Date))

    date$f_txn <- as.Date(date$f_txn, "%d-%b-%y")
    date$l_txn <- as.Date(date$l_txn, "%d-%b-%y")

    min_date <- toupper(format(as.Date(min(date[["f_txn"]])), "%d%b%Y"))
    max_date <- toupper(format(as.Date(max(date[["l_txn"]])), "%d%b%Y"))

    period <- paste0(min_date, "-", max_date)

    # group on customer mobile number
    dt_rfm <- dt_rfm[, .(Total_Freq = sum(Frequency, na.rm = TRUE)), .(Customer_Mobile)]

    # Removing duplicated records
    dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

    # rename column
    colnames(dt_rfm)[2] <- paste0(colnames(dt_rfm)[2], "_", period)

    # combine all dataframe in list
    mt_rfm[[i]] <- dt_rfm
  } else {
    LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EOSS_LOYAL")
  }
}

# set output file name
# file_name_to_save <- paste0(
#   toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
#   toupper(format(as.Date(mt_event[nrow(mt_event)]$`start period`), "%d%b%Y")), "_CUSTOMER_EOSS_LOYAL.parquet"
# )

# join all data of different duration
rfm_data <- Reduce(full_join, mt_rfm)

# replace na with 0
rfm_data[is.na(rfm_data)] <- 0

# replace value >= 1 to 1 else 0
rfm_data <- rfm_data[, (2:ncol(rfm_data)) := lapply(.SD, function(x) ifelse(x >= 1, 1, 0)), .SDcols = 2:ncol(rfm_data)]

# sum of all column values rowwise except customer mobile column
rfm_data[, Total := rowSums(.SD[, -1, with = FALSE])]

# create new column as eoss_loyal and set value in eoss_loyal according to mentioned condition
rfm_data$seg_eoss_loyal <- ifelse(rfm_data$Total >= (length(mt_event$sno) / 2), 1, 0)

# replace na with 0
rfm_data <- rfm_data[!(is.na(rfm_data$seg_eoss_loyal)), ]

# check mobile number
rfm_data <- mob_check(rfm_data, "Customer_Mobile")

# Removing duplicated records
rfm_data <- rfm_data[!duplicated(Customer_Mobile), ]

rfm_data <- rfm_data[rfm_data$seg_eoss_loyal == 1, ]

rfm_data <- rfm_data[, c("Customer_Mobile", "seg_eoss_loyal"), with = FALSE]

# set datatype of customer mobile
rfm_data$Customer_Mobile <- as.character(rfm_data$Customer_Mobile)
rfm_data$seg_eoss_loyal <- as.character(rfm_data$seg_eoss_loyal)

# total number of customers belongs to eoss loyal segment.
nrow(rfm_data)

# view list of files in particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))

  # file saved in particular path
  arrow::write_parquet(rfm_data, paste0(out_path, "/", file_name_to_save))

  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EOSS_LOYAL"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EOSS_LOYAL"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EOSS_LOYAL")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_LOYAL")
}



LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EOSS_LOYAL")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: eoss_loyal_segment.R", type = "text")
