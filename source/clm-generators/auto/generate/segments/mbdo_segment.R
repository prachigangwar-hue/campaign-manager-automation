#' Date: 26APR2023 Time: 03:00PM
#' Author: Archana Pandita

# Objective: automate mbdo segment generator
# mbdo: customers whom mbdo coupons were issued in any of the last two occasions as 2022 & 2023
# mbdo coupon issued in (mbdo 1 | mbdo 2 | mbdo 3 | ,..., | mbdo n)"

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
path_data_y22 <- "D:/digital-transformation/apis/campaign-apis/Output/masters/19JAN2022_campaign_master_CAMP19JAN2022M52PJ.parquet"
path_data_y23 <- "D:/products/cpn_comm/output/campaigns/MBDO_2000ofon2000_BRAND/api_logs"
ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/11TG_7c1je4f-rDUB5pKGUJlvIoCiB2o3P3HqKJHFt2M/edit#gid=714304411")

# set output path
out_path <- file.path(database$database$vmart$segment$mbdo)

#---- Function Calls, Data Reading & Manipulations ----
# reading event master to filter recent events
years <- c(year(Sys.Date()), (year(Sys.Date()) - 1))
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[mt_event$`event(acronym)` == "maha bachat double offer(MBDO)" & mt_event$year %in% years, ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_MBDO")
file_name <- rownames(files)[which.max(files$mtime)]

if (length(file_name) > 0) {
  # create column for end period in particular date format
  mt_event$end <- toupper(format(as.Date(mt_event$`end period`), "%d%b%Y"))

  # get duration for newly add events
  period <- strsplit(basename(file_name), "_")[[1]][1]
  start_period <- strsplit(period, "-")[[1]][1]
  end_period <- strsplit(period, "-")[[1]][2]

  # get last row details on which event already occured
  last_sno <- mt_event[mt_event$end == toupper(format(as.Date(as.Date(end_period, "%d%b%Y"), "%d%b%Y"), "%d%b%Y")), c("sno")]

  # get next rows details on which events will be occured
  new_index <- mt_event[mt_event$sno > as.numeric(last_sno), which = TRUE]
  mt_event <- mt_event[new_index[1]:length(mt_event$sno), ]
}
# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
  toupper(format(as.Date(mt_event[length(mt_event$sno)]$`end period`), "%d%b%Y")), "_CUSTOMER_MBDO.parquet"
)

# read data for year: 2022
selected_columns <- c("Customer_Mobile", "visited_11JAN2022", "visited_19JAN2022")
dt_rfm_22 <- arrow::read_parquet(path_data_y22, col_select = selected_columns)
dt_rfm_22 <- dt_rfm_22[(visited_11JAN2022 == 1 | visited_19JAN2022 == 1), c("Customer_Mobile")]

# read rfm data from folderfor 2023
dt_rfm_23 <- READ_FOLDER(pathFold = path_data_y23, patternFile = "parquet", selCols = c("customer_mobile"))
setDT(dt_rfm_23)
dt_rfm_23 <- dt_rfm_23[, .("Customer_Mobile" = as.character(customer_mobile))]

mt_rfm <- rbind(dt_rfm_22, dt_rfm_23)

mt_rfm$seg_mbdo <- 1

# check mobile number
mt_rfm <- mob_check(mt_rfm, "Customer_Mobile")

# Removing duplicated records
mt_rfm <- mt_rfm[!duplicated(Customer_Mobile), ]

mt_rfm <- mt_rfm[mt_rfm$seg_mbdo == 1, ]

mt_rfm <- mt_rfm[, c("Customer_Mobile", "seg_mbdo"), with = FALSE]

# set datatype of customer mobile
mt_rfm$Customer_Mobile <- as.character(mt_rfm$Customer_Mobile)

mt_rfm$seg_mbdo <- as.character(mt_rfm$seg_mbdo)

# total number of customers belongs to mbdo segment.
nrow(mt_rfm)

# view list of files in particular location
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
    LOG_INFO(
      what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"),
      special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_MBDO")
    )
  } else {
    LOG_INFO(
      what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"),
      special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_MBDO")
    )
  }
  LOG_INFO(
    what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"),
    special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_MBDO"
  )
} else {
  LOG_INFO(
    what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"),
    special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_MBDO"
  )
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_MBDO")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: mbdo_segment.R", type = "text")
