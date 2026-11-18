#' Date: 23MAY2023 Time: 03:40PM
#' Author: Archana Pandita

# Objective: automate eoss & rass segment generation
# eoss & early: who shooped 'n' eoss and ras campaigns
# abv >= 3000
# get customer having frequency > 2 in duration of last txn date -365 days and last txn date

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
path_mt_str <- "D:/database/mstr/store_auto"

# set output path
out_path <- file.path(database$database$vmart$segment$eoss_ras)

#---- Function Calls, Data Reading & Manipulations ----
# reading store master to filter active stores
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[((mt_event$`event(acronym)` == "red alert sale(RAS)") | (mt_event$`event(acronym)` == "end of season sale(EOSS)")), ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_EOSS_RAS")
file_name <- rownames(files)[which.max(files$mtime)]

# get dump from 01-JUNE-2019 till day - 1
#-------------------------------------------
start_dates <- as.Date("01-01-2018", "%d-%m-%Y")
end_dates <- Sys.Date() - 1

day <- REQUIRED_PERIODS(start_dates, end_dates)

# read rfm data from folder
mt_rfm_dump <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"),
  selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency", "Bill_No_Total", "Net_Amount_SUM_Total")
)

setDT(mt_rfm_dump)
mt_rfm_dump <- mt_rfm_dump[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
  "First_Transaction_Date" = as.Date(First_Transaction_Date, "%d-%b-%y"),
  "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y"),
  "Frequency" = as.numeric(Frequency), "Bill_No_Total" = as.numeric(Bill_No_Total),
  "Net_Amount_SUM_Total" = as.numeric(Net_Amount_SUM_Total)
)]


# reading store master
mt_str <- READ_FOLDER(path_mt_str, patternFile = "csv", contain = "", selCols = c("store_code", "store", "zone"))
mt_str <- mt_str[, .("Store_code" = store_code, "Store" = store, "zone" = zone)]

mt_rfm_dump <- left_join(mt_rfm_dump, mt_str, by = "Store")

# @action: eliminate manual fill of the store code
mt_rfm_dump$Store_code[mt_rfm_dump$Store == "DHANBAD-BINOD BIHARI CHOW"] <- "101667"
mt_rfm_dump$Store_code[mt_rfm_dump$Store == "DEHRI ON SANE"] <- "101446"

# rename store values as its actual name
mt_rfm_dump$Store[mt_rfm_dump$Store_code == "101667"] <- "DHANBAD-BINODBIHARI CHOWK"
mt_rfm_dump$Store[mt_rfm_dump$Store_code == "101446"] <- "DEHRI ON SONE"

#---------------------------------------
# add code here

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(min(mt_event$`start period`)), "%d%b%Y")), "-",
  toupper(format(as.Date(max(mt_event$`end period`)), "%d%b%Y")), "_CUSTOMER_EOSS_RAS.parquet"
)

# combine all data in particular list
dt_rfm <- list()

for (i in 1:length(mt_event$sno)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)
  print(day)
  # read rfm data from folder
  mt_rfm <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"),
    selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency", "Bill_No_Total", "Net_Amount_SUM_Total")
  )

  setDT(mt_rfm)
  mt_rfm <- mt_rfm[, .(
    "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
    "First_Transaction_Date" = as.Date(First_Transaction_Date, "%d-%b-%y"),
    "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y"),
    "Frequency" = as.numeric(Frequency), "Bill_No_Total" = as.numeric(Bill_No_Total),
    "Net_Amount_SUM_Total" = as.numeric(Net_Amount_SUM_Total)
  )]


  # reading store master
  mt_str <- READ_FOLDER(path_mt_str, patternFile = "csv", contain = "", selCols = c("store_code", "store", "zone"))
  mt_str <- mt_str[, .("Store_code" = store_code, "Store" = store, "zone" = zone)]

  mt_rfm <- left_join(mt_rfm, mt_str, by = "Store")

  # @action: eliminate manual fill of the store code
  mt_rfm$Store_code[mt_rfm$Store == "DHANBAD-BINOD BIHARI CHOW"] <- "101667"
  mt_rfm$Store_code[mt_rfm$Store == "DEHRI ON SANE"] <- "101446"

  # rename store values as its actual name
  mt_rfm$Store[mt_rfm$Store_code == "101667"] <- "DHANBAD-BINODBIHARI CHOWK"
  mt_rfm$Store[mt_rfm$Store_code == "101446"] <- "DEHRI ON SONE"

  # chnage column name
  setnames(mt_rfm, old = "Last_Transaction_Date", new = "Reference_Date")

  if (nrow(mt_rfm) > 0) {
    # Removing duplicated records
    mt_rfm <- mt_rfm[!duplicated(mt_rfm), ]

    # combine all dataframe in list
    dt_rfm[[i]] <- mt_rfm
  } else {
    LOG_INFO(what = paste0("File not found. Please Check server"), 
             where = file.path(path_proj, "logs"), special_operation = "@Error",
             file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_EOSS_RAS")
  }
}

# Combine all dataframes into a single datatable
dt_rfm_combined <- rbindlist(dt_rfm)


# group by for getting sum of total and frequency
dt_rfm_combined <- dt_rfm_combined[, .(
  First_Transaction_Date = min(First_Transaction_Date, na.rm = TRUE),
  Reference_Date = max(Reference_Date, na.rm = TRUE),
  Frequency = sum(Frequency, na.rm = TRUE),
  Bill_No_Total = sum(Bill_No_Total, na.rm = TRUE),
  Net_Amount_SUM_Total = sum(Net_Amount_SUM_Total, na.rm = TRUE)
), .(Customer_Mobile, Store, Store_code, zone)]

# Order the data.table by First_Transaction_Date in descending order
setorder(dt_rfm_combined, Customer_Mobile, First_Transaction_Date)

# Use the unique() and .SD functions to select the top row for each customer
dt_rfm_combined <- dt_rfm_combined[, .SD[1], by = Customer_Mobile]

# create abv column
dt_rfm_combined[, Average_Bill_Value := Net_Amount_SUM_Total / Bill_No_Total]

# filter for ABV
dt_rfm_combined <- dt_rfm_combined[Average_Bill_Value >= 3000]
# combined_data1 <- combined_data[Customer_Mobile=='9997908432']
# mt_rfm_dump1 <- mt_rfm_dump[Customer_Mobile=='9997908432']
dt_rfm_combined <- inner_join(mt_rfm_dump[, c("Customer_Mobile", "Store", "Store_code", "zone", "Last_Transaction_Date")], dt_rfm_combined, by = c("Customer_Mobile", "Store", "Store_code", "zone"))


# get recency
dt_rfm_combined$days_recency <- as.numeric(dt_rfm_combined$Reference_Date - dt_rfm_combined$Last_Transaction_Date)

# get only those txn from ref_date -365 to ref_date
dt_rfm_combined <- dt_rfm_combined[days_recency > 0 & days_recency < 365, .SD]

# get frequency
dt_rfm_combined <- dt_rfm_combined[, .(
  Frequency = sum(Frequency, na.rm = TRUE)
), .(Customer_Mobile, Store, Store_code, zone)]

# Set filter on frequency
dt_rfm_combined <- dt_rfm_combined[Frequency > 2]

# SOUTH customer only
dt_rfm_combined <- dt_rfm_combined[zone == "South"]

# customer_counts <- rfm_data_south[, .(Num_customers = uniqueN(Customer_Mobile)), by = c("Store", "Store_code")]

# check mobile number
rfm_data <- mob_check(dt_rfm_combined, "Customer_Mobile")

# Removing duplicated records
rfm_data <- rfm_data[!duplicated(rfm_data), ]

rfm_data$seg_eoss_ras <- 1

rfm_data <- rfm_data[, c("Customer_Mobile","Store" ,"Store_code","zone" ,"seg_eoss_ras"), with = FALSE]

# set datatype of customer mobile
rfm_data$Customer_Mobile <- as.character(rfm_data$Customer_Mobile)
rfm_data$Store <- as.character(rfm_data$Store)
rfm_data$Store_code <- as.character(rfm_data$Store_code)
rfm_data$seg_eoss_ras <- as.character(rfm_data$seg_eoss_ras)
rfm_data$zone <- as.character(rfm_data$zone)

# total number of customers belongs to eoss ras segment.
nrow(rfm_data)

# view list of files in particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# file saved in particular path
arrow::write_parquet(rfm_data, paste0(out_path, "/", file_name_to_save))

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EOSS_RAS")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: eoss_ras_segment.R", type = "text")
