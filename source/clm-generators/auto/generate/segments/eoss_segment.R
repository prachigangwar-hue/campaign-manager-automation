#' Date: 27JUN2023 Time: 05:40PM
#' Author: Archana Pandita

# Objective: automate eoss segment generation
# eoss early: who shooped >= n/2 times in first 15 days of last 'n' eoss campaigns
# event  count = bill date in (eoss1 first 15 day, eoss2 first 15 day, eoss3 first 15 day, ...,eoss n first 15 day)
# if event count  >= n/2 then eoss_early

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
# set output path
out_path <- file.path(database$database$vmart$segment$eoss)

#---- Function Calls, Data Reading & Manipulations ----
# reading store master to filter active stores
mt_event <- googlesheets4::read_sheet(path_event_mstr)
setDT(mt_event)
mt_event <- mt_event[mt_event$`event(acronym)` == "EOSS", ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "CUSTOMER_EOSS")
file_name <- rownames(files)[which.max(files$mtime)]

# generate output file name
file_name_to_save <- paste0(
  toupper(format(as.Date(mt_event[1]$`start period`), "%d%b%Y")), "-",
  toupper(format(as.Date(mt_event[length(mt_event$sno)]$`end period`), "%d%b%Y")), "_CUSTOMER_EOSS.parquet"
)

# reading store master
mt_str <- READ_FOLDER(path_mt_str, patternFile = "csv", contain = "", selCols = c("store_code", "store", "zone"))
mt_str <- mt_str[, .("Store_code" = store_code, "Store" = store, "zone" = zone)]

# combine all data in particular list
mt_rfm <- list()

for (i in 1:length(mt_event$sno)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)


  # read rfm data from folder
  dt_rfm <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
    selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency")
  )
  #------filteration for vmart stores
  dt_rfm <- left_join(dt_rfm, mt_str, by = "Store")

  # @action: eliminate manual fill of the store code
  dt_rfm$Store_code[dt_rfm$Store == "DHANBAD-BINOD BIHARI CHOW"] <- "101667"
  dt_rfm$Store_code[dt_rfm$Store == "DEHRI ON SANE"] <- "101446"

  # rename store values as its actual name
  dt_rfm$Store[dt_rfm$Store_code == "101667"] <- "DHANBAD-BINODBIHARI CHOWK"
  dt_rfm$Store[dt_rfm$Store_code == "101446"] <- "DEHRI ON SONE"

  # get only vmart stores
  dt_rfm <- dt_rfm[!(zone %in% c("South","WAREHOUSE")),]
  #------filteration for vmart store



  # data for first seven days
  day <- sub("^(([^|]*\\|){6}[^|]*).*$", "\\1", day) # get first seven days only
  # read rfm data from folder
  dt_rfm7 <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
    selCols = c("Customer_Mobile", "Store", "First_Transaction_Date", "Last_Transaction_Date", "Frequency")
  )

  #------filteration for vmart stores
  dt_rfm7 <- left_join(dt_rfm7, mt_str, by = "Store")
  
  # @action: eliminate manual fill of the store code
  dt_rfm7$Store_code[dt_rfm7$Store == "DHANBAD-BINOD BIHARI CHOW"] <- "101667"
  dt_rfm7$Store_code[dt_rfm7$Store == "DEHRI ON SANE"] <- "101446"
  
  # rename store values as its actual name
  dt_rfm7$Store[dt_rfm7$Store_code == "101667"] <- "DHANBAD-BINODBIHARI CHOWK"
  dt_rfm7$Store[dt_rfm7$Store_code == "101446"] <- "DEHRI ON SONE"
  
  # get only vmart stores
  dt_rfm7 <- dt_rfm7[!(zone %in% c("South","WAREHOUSE")),]
  #------filteration for vmart store


  if (nrow(dt_rfm) > 0) {
    #---- Displaying period, ----
    # get the minimum and maximum dates as the period

    mt_date <- data.table()
    setDT(mt_date)
    mt_date$f_txn <- as.data.table(unique(dt_rfm$First_Transaction_Date))
    mt_date$l_txn <- as.data.table(unique(dt_rfm$Last_Transaction_Date))

    mt_date$f_txn <- as.Date(mt_date$f_txn, "%d-%b-%y")
    mt_date$l_txn <- as.Date(mt_date$l_txn, "%d-%b-%y")

    min_date <- toupper(format(as.Date(min(mt_date[["f_txn"]])), "%d%b%Y"))
    max_date <- toupper(format(as.Date(max(mt_date[["l_txn"]])), "%d%b%Y"))

    period <- paste0(min_date, "-", max_date)

    # group on customer mobile number
    dt_rfm <- dt_rfm[, .(Total_Freq = sum(Frequency, na.rm = TRUE)), .(Customer_Mobile)]

    # Removing duplicated records
    dt_rfm <- dt_rfm[!duplicated(Customer_Mobile), ]


    if (nrow(dt_rfm7) > 0) {
      #---- Displaying period, ----
      # get the minimum and maximum dates as the period

      mt_date <- data.table()
      setDT(mt_date)
      mt_date$f_txn <- as.data.table(unique(dt_rfm7$First_Transaction_Date))
      mt_date$l_txn <- as.data.table(unique(dt_rfm7$Last_Transaction_Date))

      mt_date$f_txn <- as.Date(mt_date$f_txn, "%d-%b-%y")
      mt_date$l_txn <- as.Date(mt_date$l_txn, "%d-%b-%y")

      min_date <- toupper(format(as.Date(min(mt_date[["f_txn"]])), "%d%b%Y"))
      max_date <- toupper(format(as.Date(max(mt_date[["l_txn"]])), "%d%b%Y"))

      period <- paste0(min_date, "-", max_date)

      # group on customer mobile number
      dt_rfm7 <- dt_rfm7[, .(Total_Freq = sum(Frequency, na.rm = TRUE)), .(Customer_Mobile)]

      # Removing duplicated records
      dt_rfm7 <- dt_rfm7[!duplicated(Customer_Mobile), ]
      dt_rfm7$shopped <- "early"
    }

    dtt <- dt_rfm[!(dt_rfm$Customer_Mobile %in% dt_rfm7$Customer_Mobile), ]

    dt_rfm_rest <- dtt[(dtt$Total_Freq == 1), ]
    dt_rfm_rest$shopped <- "rest_eoss"

    dt_rfm_mul <- dtt[!(dtt$Total_Freq == 1), ]
    dt_rfm_mul$shopped <- "multi_visit"

    dt_final <- rbind(dt_rfm7[, c("Customer_Mobile", "shopped")], dt_rfm_rest[, c("Customer_Mobile", "shopped")], dt_rfm_mul[, c("Customer_Mobile", "shopped")])
    # rename column
    # colnames(dt_rfm)[2] <- paste0(colnames(dt_rfm)[2], "_", period)

    # combine all dataframe in list
    mt_rfm[[i]] <- dt_final
  } else {
    LOG_INFO(what = paste0("Data not found for duration:", start_dates, " to ", end_dates), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "SEGMENT_GENERATION_CUSTOMER_RAS")
  }
}


# Filter out NULL elements from mt_rfm list
mt_rfm_filtered <- mt_rfm %>%
  discard(is.null)

# Perform full join on the remaining data frames
if (length(mt_rfm_filtered) > 1) {
  rfm_data <- Reduce(full_join, mt_rfm_filtered)
} else {
  rfm_data <- mt_rfm_filtered[[1]]
}

rfm_data <- rfm_data[!(duplicated(rfm_data))]

# combination of shopped
# x <- mt_rfm_dup[, .(shopped = toString(shopped)), by = Customer_Mobile]

final <- rfm_data[, .(shopped = ifelse("early" %in% shopped & "multi_visit" %in% shopped & "rest_eoss" %in% shopped, "early",
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


# check mobile number
final <- mob_check(final, "Customer_Mobile")

# Removing duplicated records
final <- final[!duplicated(Customer_Mobile), ]

# set datatype of customer mobile
final$Customer_Mobile <- as.character(final$Customer_Mobile)

# total number of customers belongs to eoss early segment.
nrow(final)


# view list of files in particular location
files <- file.info(list.files(path = paste0(out_path,"/raw"), full.names = T), pattern = "*.parquet")

# file saved in particular path
arrow::write_parquet(final, paste0(paste0(out_path,"/raw"), "/", file_name_to_save))

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("SEGMENT_GENERATION_CUSTOMER_RAS")
)

# SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: eoss_early_segment.R", type = "text")
