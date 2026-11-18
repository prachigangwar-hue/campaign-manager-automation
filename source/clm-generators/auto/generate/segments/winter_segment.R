#' Date: 25OCT2024 Time: 01:00PM
#' Author: Abhijit Nandi

# Objective: automate winter segment generator
# diwali: who shooped in last  winter duration
# event count = bill date in last winter duration
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
# path_data <- file.path(database$database$vmart$rfm$customer$division_group$offline$tagged$day)
# ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/11TG_7c1je4f-rDUB5pKGUJlvIoCiB2o3P3HqKJHFt2M/edit#gid=714304411")

# set output path
out_path <- file.path(database$database$vmart$segment$winter)

#############
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")
path_txn <- file.path(path_dbase,"txn/offline/generate/day")


start_dates <- as.Date("2023-10-01")
end_dates <- as.Date("2024-03-31")

day <- REQUIRED_PERIODS(start_dates, end_dates)

mt_cust <- READ_FOLDER(pathFold = path_txn, patternFile = "parquet",
                      contain = day,
                      selCols = c("Customer_Mobile","INVATTR2_NAME"))

mt_cust <- mt_cust[INVATTR2_NAME %in% c("WINTER","PRE WINTER")]
mt_cust <- mt_cust[!duplicated(Customer_Mobile)]
mt_cust <- mob_check(mt_cust,var = "Customer_Mobile")
mt_cust$Customer_Mobile <- as.character(mt_cust$Customer_Mobile)
mt_cust$seg_winter <- "1"
mt_cust <- mt_cust[,.(Customer_Mobile,seg_winter)]






