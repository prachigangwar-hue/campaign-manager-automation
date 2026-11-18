#' Date: 23MAY2023 Time: 03:40PM
#' Author: Archana Pandita

# Objective: automate ras(south stores only) segment generation
# ras: who shooped in given dates
# 1) 9th June'22 - 26th June'22
# 2) 9th Dec'22 - 1st Jan'23
# EOSS
# 2) 27th June'22 - 31st July'22 (UL)
# 4) 2nd Jan'23 - 17th Jan'23 (UL)
# get customer having frequency >= 2 in duration of last txn date -365 days and last txn date

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
out_path <- file.path(database$database$vmart$segment$eoss_ras_v2)

#---- Function Calls, Data Reading & Manipulations ----
# reading store master to filter active stores
# mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
# setDT(mt_event)
# mt_event <- mt_event[((mt_event$`event(acronym)` == "red alert sale(RAS)") | (mt_event$`event(acronym)` == "end of season sale(EOSS)")), ]



# 2) 27th June'22 - 31st July'22 (UL)
# 4) 2nd Jan'23 - 17th Jan'23 (UL)


# Create a vector of from_dates for the date ranges
`start period` <- c(as.Date("2022-06-09"), as.Date("2022-12-09"), as.Date("2022-06-27"), as.Date("2023-01-02"))

# Create a vector of to_dates for the date ranges
`end period` <- c(as.Date("2022-06-26"), as.Date("2023-01-01"), as.Date("2022-07-31"), as.Date("2023-01-17"))

# Create a data.table with the from_dates and to_dates
mt_event <- data.table(`start period` = `start period`, `end period` = `end period`)


# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_EOSS_RAS")
file_name <- rownames(files)[which.max(files$mtime)]


#---------------------------------------
# add code here

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(min(mt_event$`start period`)), "%d%b%Y")), "-",
  toupper(format(as.Date(max(mt_event$`end period`)), "%d%b%Y")), "_CUSTOMER_RAS_EOSS_UL.parquet"
)

# combine all data in particular list
dt_rfm <- list()

for (i in 1:nrow(mt_event)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)
  print(day)
  # read rfm data from folder
  mt_rfm <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"),
    selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency")
  )

  setDT(mt_rfm)
  mt_rfm <- mt_rfm[, .(
    "Customer_Mobile" = as.character(Customer_Mobile), "Store" = as.character(Store),
    "First_Transaction_Date" = as.Date(First_Transaction_Date, "%d-%b-%y"),
    "Last_Transaction_Date" = as.Date(Last_Transaction_Date, "%d-%b-%y"),"Frequency" = as.numeric(Frequency)
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
  
  #get only south stores
  mt_rfm <- mt_rfm[zone=="South",]

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
  Last_Transaction_Date = max(Last_Transaction_Date, na.rm = TRUE),
  Frequency = sum(Frequency, na.rm = TRUE)
), .(Customer_Mobile, Store, Store_code, zone)]

# Set filter on frequency
# dt_rfm_combined <- dt_rfm_combined[Frequency >= 2]

#get active
dt_rfm_combined$days_recency <- as.numeric(Sys.Date() - dt_rfm_combined$Last_Transaction_Date)
dt_rfm_combined$customer_type <- cut(dt_rfm_combined$days_recency, breaks = c(-Inf, 180, 360, 720, Inf), c("a", "l", "o", "p"))
dt_rfm_combined <- dt_rfm_combined[customer_type=="a",]
# Order the data.table by First_Transaction_Date in descending order
setorder(dt_rfm_combined, Customer_Mobile, -Last_Transaction_Date)

#test 
# x <- dt_rfm_combined[, .("uniq" = uniqueN(Store)), .(Customer_Mobile)]#7207852345
# Use the unique() and .SD functions to select the top row for each customer
dt_rfm_combined <- dt_rfm_combined[, .SD[1], by = Customer_Mobile]

dt_rfm_combined <- dt_rfm_combined[,c("Customer_Mobile","Store","Store_code" , "zone")]
dt_rfm_combined <- dt_rfm_combined[!(duplicated(dt_rfm_combined))]


# check mobile number
dt_rfm_combined <- mob_check(dt_rfm_combined, "Customer_Mobile")

# Removing duplicated records
dt_rfm_combined <- dt_rfm_combined[!duplicated(dt_rfm_combined), ]

rslt <- dt_rfm_combined[, .("uni_cm" = uniqueN(Customer_Mobile)), .(Store)]

dt_rfm_combined$seg_eoss_ras_ul <- 1

dt_rfm_combined <- dt_rfm_combined[, c("Customer_Mobile","Store" ,"Store_code","zone" ,"seg_eoss_ras_ul"), with = FALSE]

# set datatype of customer mobile
dt_rfm_combined$Customer_Mobile <- as.character(dt_rfm_combined$Customer_Mobile)
dt_rfm_combined$Store <- as.character(dt_rfm_combined$Store)
dt_rfm_combined$Store_code <- as.character(dt_rfm_combined$Store_code)
dt_rfm_combined$seg_eoss_ras_ul <- as.character(dt_rfm_combined$seg_eoss_ras_ul)
dt_rfm_combined$zone <- as.character(dt_rfm_combined$zone)

# total number of customers belongs to eoss ras segment.
nrow(dt_rfm_combined)

# view list of files in particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# file saved in particular path
arrow::write_parquet(dt_rfm_combined, paste0(out_path, "/", file_name_to_save))

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EOSS_RAS")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: eoss_ras_segment.R", type = "text")
