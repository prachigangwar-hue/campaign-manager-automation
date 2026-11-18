# Date: 09-Aug-2023
# Author: Akash Sharma

# Objective: To remove DND customer mobile no.s from campaign master

#---- Cleaning Enviornment ----
cat("\n\n")
cat("Cleaning enviornment","\n")
rm(list=ls()) 
gc()

#---- Sourcing Libraries and external files ----
# loading libraries
cat("\n\n")
cat("Loading libraries","\n")
library(dplyr)
library(data.table)
library(pbapply)
library(arrow)

# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")
path_lib <- file.path(path_proj, "auto/library")
path_dlr <- file.path(path_dbase,"dlr","processed")

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib,"required_period_function(REQUIRED_PERIODS).R"))

mob_check <- function (data, var = "CUSTOMER_MOBILE", replace = FALSE, stopNumbers = c(9999999999, 9999999990, 9999999993)) {
  if (var %in% colnames(data)) {
    if (replace == F) {
      data <- data[grepl("^[6-9]\\d{9}$", data[[var]]), 
      ]
      data <- data[!(data[[var]] %in% stopNumbers), ]
      print("Sucees!! Deleted non Indian or fraud numbers")
    }
    if (replace == T) {
      data[!(grepl("^[6-9]\\d{9}$", data[[var]])), 
           var] <- NA
      data[(data[[var]] %in% stopNumbers), var] <- NA
      print("Sucess!! Replaced non Indian or fraud numbers with NA")
    }
  }
  else {
    warning("Column not found in data")
  }
  return(data)
}

start_date <- Sys.Date()-60
end_date <- Sys.Date()-1

dt_dlr <- READ_FOLDER(pathFold = path_dlr,patternFile = "parquet",
                      contain = REQUIRED_PERIODS(start_period = start_date,end_period = end_date),
                      selCols = c("Customer_Mobile","Delivery_Status","campaign_name"))
gc();gc();gc();gc()
dt_dlr <- dt_dlr[grepl("^SEGMENT",campaign_name)]
dt_dlr$send_date <- substr(dt_dlr$campaign_name,8,16)
gc()
dt_date <- data.table(unique(dt_dlr$send_date))
colnames(dt_date) <- "send_date"
dt_date$f_date <- as.Date(dt_date$send_date,"%d%b%Y")
gc()
dt_dlr <- left_join(dt_dlr,dt_date,by="send_date")
dt_dlr$send_date <- NULL
setnames(dt_dlr,old = "f_date",new = "send_date")
gc()
dt_dlr$send_date <- as.Date(dt_dlr$send_date)
dt_dlr <- dt_dlr[!is.na(send_date)]

dt_dlr <- dt_dlr[send_date >= Sys.Date()-60]

dt_dlr <- dt_dlr[order(dt_dlr$send_date,decreasing = TRUE)]
dt_dlr[,c("campaign_name","send_date")] <- NULL
gc();gc()

dt_dlr <- dt_dlr[, .SD[1:min(.N,2)], by = Customer_Mobile]
dt_dlr$Customer_Mobile <- as.character(dt_dlr$Customer_Mobile)

pv_cust <- data.table(table(dt_dlr$Customer_Mobile))
colnames(pv_cust) <- c("Customer_Mobile","frequency")
pv_cust$Customer_Mobile <- as.character(pv_cust$Customer_Mobile)

dt_dlr <- left_join(dt_dlr,pv_cust)
rm(pv_cust);gc()
dt_dnd <- dt_dlr[, .(not_delivered_count = sum(Delivery_Status == "not_delivered")), by = Customer_Mobile]

dt_dnd <- left_join(dt_dlr,dt_dnd,by="Customer_Mobile")

dt_dnd <- dt_dnd[(frequency==1 & Delivery_Status == "not_delivered") | not_delivered_count == 2]
dt_dnd <- mob_check(dt_dnd,var = "Customer_Mobile")
unique(dt_dnd$Delivery_Status)

mt_dnd <- READ_FOLDER(pathFold = "D:/database/seg/dnd",patternFile = "parquet")
dt_dnd <- rbindlist(list(mt_dnd,dt_dnd[,"Customer_Mobile"]))
dt_dnd <- dt_dnd[!duplicated(Customer_Mobile)]

write_parquet(dt_dnd[,"Customer_Mobile"],"D:/database/seg/dnd/dnd_customers.parquet")
