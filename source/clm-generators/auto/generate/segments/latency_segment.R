#' Date: 06APR2022 Time: 03:00PM
#' Author: Archana Pandita

# Objective: automate latency segment generator
# latency: customer next transaction date is in +- 10 days of the event period

# next transaction date = last transaction date + latency
# latency = (last transaction date - first transaction date) / (frequency - 1)
# if next transaction date in (event period - 10, event period + 10) then latency


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
out_path <- file.path(database$database$vmart$segment$latency)

#---- Function Calls, Data Reading & Manipulations ----
# reading store master to filter active stores
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[mt_event$`event(acronym)` == "latency(L)", ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_LATENCY")
file_name <- rownames(files)[which.max(files$mtime)]

if (length(file_name) > 0) {
  # create column for end period in particular date format
  mt_event$end <- toupper(format(as.Date(mt_event$`end period`), "%d%b%Y"))

  # get duration for newly add events
  period <- strsplit(basename(file_name), "_")[[1]][1]
  start_period <- strsplit(period, "-")[[1]][1]
  end_period <- strsplit(period, "-")[[1]][2]

  # get last row details on which event already occured  #subtract 10days in end_period
  last_sno <- mt_event[mt_event$end == toupper(format(as.Date(as.Date(end_period, "%d%b%Y") - 10, "%d%b%Y"), "%d%b%Y")), c("sno")]

  # get next rows details on which events will be occured
  new_index <- mt_event[mt_event$sno > as.numeric(last_sno), which = TRUE]
  mt_event <- mt_event[new_index[1]:length(mt_event$sno), ]
}
# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
  toupper(format(as.Date(mt_event[length(mt_event$sno)]$`end period`), "%d%b%Y")), "_CUSTOMER_LATENCY.parquet"
)

mt_rfm <- list()
#
for (i in 1:length(mt_event$sno)) {
  event_start_dates <- as.Date(mt_event$`start period`[i])
  event_end_dates <- as.Date(mt_event$`end period`[i])

  event_day <- REQUIRED_PERIODS(event_start_dates, event_end_dates)

  # for getting last and first transaction date
  # end_date <- Sys.Date() - 1
  # start_date <- seq(end_date, length = 2, by = "-2 years")[2]

  # for getting last and first transaction date
  end_date <- as.Date(paste0(mt_event$year[i], "-12-31"))
  start_date <- as.Date(paste0(mt_event$year[i], "-01-01"))
  day <- REQUIRED_PERIODS(start_date, end_date)


  # read rfm data from folder
  dt_rfm <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
    selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency")
  )

  setDT(dt_rfm)
  dt_rfm <- dt_rfm[, .(
    "Customer_Mobile" = as.character(Customer_Mobile), "Store" = Store,
    "First_Transaction_Date" = as.Date(First_Transaction_Date, "%d-%b-%y"),
    "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y"),
    "Frequency" = Frequency
  )]
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

    # group on customer mobile number
    dt_rfm_freq <- dt_rfm[, .(Frequency = sum(Frequency, na.rm = TRUE)), .(Customer_Mobile)]

    # subtract 1 to frequency
    dt_rfm_freq$Frequency <- dt_rfm_freq$Frequency - 1

    # get difference between last and first transaction data
    dt_rfm_txn_dt <- dt_rfm[, .(Last_Transaction_Date = max(Last_Transaction_Date), First_Transaction_Date = min(First_Transaction_Date)), .(Customer_Mobile)]
    dt_rfm_txn_dt$Ltd_Ftd <- dt_rfm_txn_dt$Last_Transaction_Date - dt_rfm_txn_dt$First_Transaction_Date

    # combine txn diffrences and frequency on customer mobile
    dt_rfm <- full_join(dt_rfm_txn_dt, dt_rfm_freq, by = c("Customer_Mobile"))

    # Removing duplicated records
    dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]

    # check mobile number
    dt_rfm <- mob_check(dt_rfm, "Customer_Mobile")

    # set datatype of customer mobile
    dt_rfm$Customer_Mobile <- as.character(dt_rfm$Customer_Mobile)

    # create latency column
    dt_rfm$latency <- dt_rfm$Ltd_Ftd / dt_rfm$Frequency

    # replace na with 0
    dt_rfm[is.na(dt_rfm$latency)] <- 0

    # get next transaction date
    dt_rfm$Next_Transaction_Date <- dt_rfm$Last_Transaction_Date + dt_rfm$latency

    # combine all dataframe in list
    mt_rfm[[i]] <- dt_rfm
  } else {
    LOG_INFO(
      what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"),
      special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_LATENCY"
    )
  }
}

# join all data of different duration
rfm_data <- Reduce(full_join, mt_rfm)

# replace na with 0
rfm_data[is.na(rfm_data)] <- 0

# add/subtract 10 days to event duration
event_start_dates <- as.Date(event_start_dates, "%d-%b-%y") - 10
event_end_dates <- as.Date(event_end_dates, "%d-%b-%y") + 10

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(event_start_dates), "%d%b%Y")), "-",
  toupper(format(as.Date(event_end_dates), "%d%b%Y")), "_CUSTOMER_LATENCY.parquet"
)

# if Next_Transaction_Date in event duration
event_day <- data.table(date = seq(from = event_start_dates, to = event_end_dates, by = "day"))

rfm_data <- rfm_data[rfm_data$Next_Transaction_Date %in% event_day$date, ]

rfm_data$seg_latency <- 1

# replace na with 0
rfm_data <- rfm_data[!(is.na(rfm_data$seg_latency)), ]

# check mobile number
rfm_data <- mob_check(rfm_data, "Customer_Mobile")

# Removing duplicated records
rfm_data <- rfm_data[!duplicated(Customer_Mobile), ]

rfm_data <- rfm_data[rfm_data$seg_latency == 1, ]

rfm_data <- rfm_data[, c("Customer_Mobile", "seg_latency"), with = FALSE]

# set datatype of customer mobile
rfm_data$Customer_Mobile <- as.character(rfm_data$Customer_Mobile)

rfm_data$seg_latency <- as.character(rfm_data$seg_latency)

# total number of customers belongs to latency segment.
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
    LOG_INFO(
      what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"),
      special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_LATENCY")
    )
  } else {
    LOG_INFO(
      what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"),
      special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_LATENCY")
    )
  }
  LOG_INFO(
    what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"),
    special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_LATENCY"
  )
} else {
  LOG_INFO(
    what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"),
    special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_LATENCY"
  )
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_LATENCY")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: latency_segment.R", type = "text")
