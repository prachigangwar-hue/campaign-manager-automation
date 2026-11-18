#' Date: 27JUN2023 Time: 10:40PM
#' Author: Archana Pandita

# Objective: automate merge of ras and  eoss segment generation

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)
library(tidyverse)
library(purrr)
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
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/1WrITywWSW4zY8_qVMCSXQHpeVGTY-apxvLkCUGRyHV0/edit#gid=0")
path_mt_str <- "D:/database/mstr/store_auto"

# reading store master to filter active stores
mt_event <- googlesheets4::read_sheet(path_event_mstr)
setDT(mt_event)

# set output path
out_path <- file.path(database$database$vmart$segment$eoss)

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
  toupper(format(as.Date(mt_event[length(mt_event$sno)]$`end period`), "%d%b%Y")), "_EOSS_CUSTOMER.parquet"
)

dt_inp <-   READ_FOLDER("D:/database/seg/eoss/raw", patternFile = "parquet") 

dt_inp <- dt_inp[!duplicated(dt_inp)]

final <- dt_inp[, .(shopped = ifelse("early" %in% shopped & "multi_visit" %in% shopped & "rest_eoss" %in% shopped, "early",
                                       ifelse("rest_eoss" %in% shopped & "early" %in% shopped, "early",
                                              ifelse("early" %in% shopped & "multi_visit" %in% shopped, "early",
                                                     ifelse("rest_eoss" %in% shopped & "multi_visit" %in% shopped, "multi_visit",
                                                            ifelse("rest_eoss" %in% shopped, "rest_eoss",
                                                                   ifelse("multi_visit" %in% shopped, "multi_visit",
                                                                          ifelse("early" %in% shopped, "early", "")
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
)),
by = Customer_Mobile
]

# changinf frequency name
setnames(final,old = "shopped",new = "seg_eoss")

# check mobile number
final <- mob_check(final, "Customer_Mobile")

# Removing duplicated records
final <- final[!duplicated(Customer_Mobile), ]

# set datatype of customer mobile
final$Customer_Mobile <- as.character(final$Customer_Mobile)
final$seg_eoss <- as.character(final$seg_eoss)

# total number of customers belongs to eoss  segment.
nrow(final)


# view list of files in particular location
files <- file.info(list.files(path =out_path , full.names = T), pattern = "*.parquet")

# file saved in particular path
arrow::write_parquet(final, paste0(out_path, "/", file_name_to_save))

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("SEGMENT_GENERATION_EOSS_CUSTOMER")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: eoss_early_segment.R", type = "text")









