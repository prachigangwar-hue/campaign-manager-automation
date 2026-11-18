#' Date: 23MAY2023 Time: 03:40PM
#' Author: Archana Pandita

# Objective: automate eoss roi segment generation
# EOSS
# 1) 24th June'22 - 21st Aug'22 (ROI)
# 3) 24th Dec'22 - 17th Jan'23 (ROI)
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



# 1) 24th June'22 - 21st Aug'22 (ROI)
# 3) 24th Dec'22 - 17th Jan'23 (ROI)

# Create a vector of from_dates for the date ranges
`start period` <- c(as.Date("2022-06-24"), as.Date("2022-12-24"))

# Create a vector of to_dates for the date ranges
`end period` <- c(as.Date("2022-08-21"), as.Date("2023-01-17"))

# Create a data.table with the from_dates and to_dates
mt_event <- data.table(`start period` = `start period`, `end period` = `end period`)

#---------------------------------------
# add code here

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(min(mt_event$`start period`)), "%d%b%Y")), "-",
  toupper(format(as.Date(max(mt_event$`end period`)), "%d%b%Y")), "_CUSTOMER_EOSS_ROI.parquet"
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
  
  # set zone
  mt_rfm$zone[mt_rfm$Store_code == "101667"] <- "East/Jhk"
  mt_rfm$zone[mt_rfm$Store_code == "101446"] <- "Bihar"
  
  #get only south stores
  mt_rfm <- mt_rfm[!(zone %in% c("South","WAREHOUSE")),]

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

# Order the data.table by First_Transaction_Date in descending order
setorder(dt_rfm_combined, Customer_Mobile, -Last_Transaction_Date)

#test 
# x <- dt_rfm_combined[, .("uniq" = uniqueN(Store)), .(Customer_Mobile)]#6005476744

# Use the unique() and .SD functions to select the top row for each customer
dt_rfm_combined <- dt_rfm_combined[, .SD[1], by = Customer_Mobile]

dt_rfm_combined <- dt_rfm_combined[,c("Customer_Mobile","Store","Store_code" , "zone","days_recency","customer_type","Frequency")]
dt_rfm_combined <- dt_rfm_combined[!(duplicated(dt_rfm_combined))]


# check mobile number
dt_rfm_combined <- mob_check(dt_rfm_combined, "Customer_Mobile")

# Removing duplicated records
dt_rfm_combined <- dt_rfm_combined[!duplicated(dt_rfm_combined), ]

rslt <- dt_rfm_combined[, .("unique_customer_count" = uniqueN(Customer_Mobile)), .(Store)]


# Filter stores with unique_customer_count > 3000
filtered_stores_grt_3000 <- rslt[unique_customer_count  > 3000, Store]

# Create a set of filtered stores
filtered_store_set_grt_3000 <- unique(filtered_stores_grt_3000)

# Filter stores with unique_customer_coun < 3000
filtered_stores_less_3000 <- rslt[unique_customer_count  < 3000, Store]

# Create a set of filtered stores
filtered_store_set_less_3000 <- unique(filtered_stores_less_3000)

 
# filtered_store_set_grt_3000 <- c("KOLKATA-ESPLANADE","AGARTALA-SHAKUNTALA ROAD")
# Create an empty datatable to store the results
result_dt <- data.table()
# filtered_store_set_grt_3000 <- c("KOLKATA-ESPLANADE")
# Sort and retrieve 3000 rows for each filtered store
for (store in filtered_store_set_grt_3000) {
  # filtered_dt <- dt_rfm_combined[Store == store][order(-days_recency)]
  filtered_dt <- dt_rfm_combined[Store == store]
  # filtered_dt <- filtered_dt[Frequency >=2,]
  filtered_dt <- filtered_dt[order(days_recency)]
  filtered_dt <- filtered_dt[1:3000,]
  result_dt <- rbind(result_dt, filtered_dt)
}

# Create an empty datatable to store the results
result_dt1 <- data.table()

# Sort and retrieve 3000 rows for each filtered store#dt_rfm_combined <- dt_rfm_combined[customer_type=="a",]
for (store in filtered_store_set_less_3000) {
  filtered_dt1 <- dt_rfm_combined[Store == store]
  # filtered_dt1[, customer_type := "a"]  # Set customer_type to "a" for each filtered store
  result_dt1 <- rbind(result_dt1, filtered_dt1)
}

result <- rbind(result_dt,result_dt1)


result_pv <- result[, .("unique_customer_count" = uniqueN(Customer_Mobile)), .(Store)]

result$seg_eoss_roi <- 1

result <- result[, c("Customer_Mobile","Store" ,"Store_code","zone" ,"seg_eoss_roi"), with = FALSE]

# set datatype of customer mobile
result$Customer_Mobile <- as.character(result$Customer_Mobile)
result$Store <- as.character(result$Store)
result$Store_code <- as.character(result$Store_code)
result$seg_eoss_roi <- as.character(result$seg_eoss_roi)
result$zone <- as.character(result$zone)

# total number of customers belongs to eoss ras segment.
nrow(result)

# view list of files in particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# file saved in particular path
arrow::write_parquet(result, paste0(out_path, "/", file_name_to_save))

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_EOSS_RAS")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: eoss_ras_segment.R", type = "text")
