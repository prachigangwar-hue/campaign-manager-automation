# Date: 09JAN2022 Time: 03:00PM
# Author: Riten Chadha

# Objective: automate Recency/Latency segment generator
# Recency refers to how many days back did the customer transact from reference date
# Latency refers to time between consecutive transactions

#----Cleaning Environment----


rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
cat("\n\n")
cat("Loading libraries", "\n")
# library(dreaweRTools)
library(dplyr)
library(data.table)
library(pbapply)
library(tidyr)
library(httr)
library(RCurl)

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
out_path <- file.path(database$database$vmart$segment$recency_by_latency)


# Setting last reference date as dynamic and converting string as.date

# last_date_ref <- Sys.Date() - 1
last_date_referance <- Sys.Date() - 2
last_date_referance <- gsub('-', '/', last_date_referance)
last_date_referance <-as.Date(last_date_referance, "%Y/ %m/ %d")
start_date_all <- "2018-02-01"
start_date_all <- gsub('-', '/', start_date_all)
start_date_all <-as.Date(start_date_all, "%Y/ %m/ %d")

day <- REQUIRED_PERIODS(start_date_all,last_date_referance)



typeof(day)
min_date <- substr(day, 1, 9) 
max_date <-substr(day, nchar(day) - 9 + 1, nchar(day))

file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_RECENCY_BY_LATENCY.parquet")

day <- toupper(format(seq.Date(from = start_date_all,to = last_date_referance,by = "month"),"%b%Y"))
# rfm data from 2018 till date
dt_rfm <- READ_FOLDER(pathFold = path_data,patternFile = "parquet",
                         contain = paste0(day, collapse = "|"),selCols = c("Customer_Mobile","Last_Transaction_Date","Frequency"))

# Changing date format so as to add/ subtract days
dt_rfm$Last_Transaction_Date <- gsub('-', '/', dt_rfm$Last_Transaction_Date)
dt_rfm$Last_Transaction_Date <-as.Date(dt_rfm$Last_Transaction_Date, "%d/%b/%y")

gc()

#getting max transaction date for each customer in RFM 
dt_rfm_max_date <- dt_rfm[ , .(max_transaction_date = max(Last_Transaction_Date)), by = c("Customer_Mobile")]

#getting reference date 364 days before to calculate rolling metrics
dt_rfm_max_date$start_transaction_date_ref <- (dt_rfm_max_date$max_transaction_date - as.difftime(364, unit = "days"))

#Joining to get rolling frequency
dt_rfm_rolling_frequency <-dt_rfm[dt_rfm_max_date, .(Customer_Mobile, max_transaction_date, start_transaction_date_ref,Frequency), on=.(Customer_Mobile, Last_Transaction_Date>=start_transaction_date_ref, Last_Transaction_Date<=max_transaction_date), nomatch=0L]

remove(dt_rfm)
remove(dt_rfm_max_date)

#Getting sum of rolling frequency per customer
dt_rfm_rolling_frequency <- dt_rfm_rolling_frequency[ , .(max_transaction_date = unique(max_transaction_date),Sum_freq = sum(Frequency)), by = c("Customer_Mobile")]

#calculating average latency by dividing 365 by the sum of rolling frequenccy
dt_rfm_rolling_frequency$avg_latency <- 365/(dt_rfm_rolling_frequency$Sum_freq)

#calculating recency by getting day differency between current date and max transaction date per customer
dt_rfm_rolling_frequency$recency <- ( last_date_referance - dt_rfm_rolling_frequency$max_transaction_date)

#Calculating R by L 
dt_rfm_rolling_frequency$R_by_L <- (dt_rfm_rolling_frequency$recency) / (dt_rfm_rolling_frequency$avg_latency)

dt_rfm_rolling_frequency$recency <- as.double(dt_rfm_rolling_frequency$recency)
dt_rfm_rolling_frequency$R_by_L <- as.double(dt_rfm_rolling_frequency$R_by_L)



#1. R/L – which will help us identify due customers : flag nomenclature 
# ( less than  0.75) || (0.76 to 1.25) || (1.26 to 1.50) || (1.51  greater )
#  Creating buckets using cut function


agebreaks <- c(0, 0.25, 0.50, 0.75, 1, 1.25, 1.5, Inf)
agelabels <- c("0-0.25","0.25-0.50","0.50-0.75","0.75-1","1-1.25","1.25-1.50","1.5+")


setDT(dt_rfm_rolling_frequency)[ , seg_r_l := cut(R_by_L, 
                                                        breaks = agebreaks, 
                                                        right = FALSE, 
                                                        labels = agelabels)]


unique(dt_rfm_rolling_frequency$seg_r_l)

dt_rfm_rolling_frequency = select(dt_rfm_rolling_frequency, c(`Customer_Mobile`,`seg_r_l`))

table(dt_rfm_rolling_frequency$seg_r_l)

gc()

#total number of customers belonging to recency_by_latency_segment.
nrow(dt_rfm_rolling_frequency)

dt_rfm_rolling_frequency <- mob_check(dt_rfm_rolling_frequency, "Customer_Mobile")

dt_rfm_rolling_frequency <- dt_rfm_rolling_frequency[!(duplicated(dt_rfm_rolling_frequency)),]


# Setting Customer Mobile as Character
dt_rfm_rolling_frequency$Customer_Mobile <- as.character(dt_rfm_rolling_frequency$Customer_Mobile) 
dt_rfm_rolling_frequency$seg_r_l <- as.character(dt_rfm_rolling_frequency$seg_r_l)



files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_rfm_rolling_frequency, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_RECENCY_BY_LATENCY"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_RECENCY_BY_LATENCY"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_RECENCY_BY_LATENCY")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_RECENCY_BY_LATENCY")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_RECENCY_BY_LATENCY")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: recency_by_latency_segment.R", type = "text")
