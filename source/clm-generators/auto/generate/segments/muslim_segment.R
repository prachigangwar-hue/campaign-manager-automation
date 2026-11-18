#' Date: 20FEB2022 Time: 12:00PM
#' Author: Archana Pandita

# Objective: automate muslim segment generator
# customer bought 'burkha' or 'hijab' OR
# customer has muslim name OR
# customer shopped in eid period not in holi and diwali OR
# customer shopped 'kurta-pyjama' in eid

# muslim department = shopped product in('burkha', 'hijab')
# muslim name = customer name in ('muslim' names)
# muslim fest = shopped in 'eid' but not in(holi, diwali)
# muslim fest muslim department = shopped 'kurta pyjama' in 'eid'
# if muslim department OR muslim name OR muslim fest OR muslim fest muslim department then muslim

#----Cleaning Enviornment----
rm(list = ls())
gc()


#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)

# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")
path_lib <- file.path(path_proj, "auto/library")

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))
source(file.path(path_lib, "chatbot-lib.R"))
source(file.path(path_lib, "required_period_function(REQUIRED_PERIODS).R"))
source(file.path(path_proj, "auto/library/MOB_CHECK_function.R"))
source(file.path(path_lib, "break_and_save_function(BREAK_AND_SAVE).R"))

#----  Path/variable declaration----
path_data <- file.path(database$database$transaction$offline)
# path_cust_name <- file.path(path_proj, "definitions", "persistent")
path_cust_name <- file.path(path_proj, "input", "persistent_data") # :\products\clm_generators\output\generate\0.persistent

ids_to_send <- c("745474876")
googlesheets4::gs4_auth(email = "ga@vmartretail.com", cache = file.path(path_proj, "credentials", "gsheet"))
path_event_mstr <- file.path("https://docs.google.com/spreadsheets/d/11TG_7c1je4f-rDUB5pKGUJlvIoCiB2o3P3HqKJHFt2M/edit#gid=714304411")

# set output path
out_path <- file.path(database$database$vmart$segment$muslim)

#---- Function Calls, Data Reading & Manipulations ----

current_year <- as.integer(format(Sys.Date(), "%Y"))
previous_year <- current_year - 1

# Create a data table with the current year and previous year
year_data <- data.table(Year = c(previous_year, current_year))

# reading event for eid
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[(mt_event$`event(acronym)` == "eid(E)" & mt_event$year %in% year_data$Year), ]

# get recent file from segment folder
files <- file.info(list.files(path = out_path, full.names = T), pattern = "parquet", pattern = "MUSLIM")
file_name <- rownames(files)[which.max(files$mtime)]

if (length(file_name) > 0) {
  # get duration for newly add events
  period <- strsplit(basename(file_name), "_")[[1]][1]
  end_period_eid <- strsplit(period, "-")[[1]][1]

  # get last row details on which event already occured
  last_sno <- mt_event[toupper(format(as.Date(mt_event$`end period`), "%d%b%Y")) %in% end_period_eid, c("sno")]

  # get next rows details on which events will be occured
  new_index <- mt_event[mt_event$sno > as.numeric(last_sno), which = TRUE]
  if (length(new_index) > 0) {
    mt_event <- mt_event[new_index[1]:length(mt_event$sno), ]
  } else {
    stop()
  }
}

# part 1:
# combine all data in particular list
date_ranges_eid <- list()

for (i in 1:length(mt_event$sno)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)

  date_ranges_eid[[i]] <- day[[1]] # combine all dataframe in list
}
date_ranges_eid <- do.call(paste, c(as.list(date_ranges_eid), sep = "|"))


# part 2:
# reading event for holi
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[(mt_event$`event(acronym)` %in% c("holi(H)") & mt_event$year %in% year_data$Year), ]


if (length(file_name) > 0) {
  # get duration for newly add events
  period <- strsplit(basename(file_name), "_")[[1]][1]
  end_period_holi <- strsplit(period, "-")[[1]][2]

  # get last row details on which event already occured
  last_sno <- mt_event[toupper(format(as.Date(mt_event$`end period`), "%d%b%Y")) %in% end_period_holi, c("sno")]

  # get next rows details on which events will be occured
  new_index <- mt_event[mt_event$sno > as.numeric(last_sno), which = TRUE]
  if (length(new_index) > 0) {
    mt_event <- mt_event[new_index[1]:length(mt_event$sno), ]
  } else {
    stop()
  }
}

# combine all data in particular list
date_ranges_h <- list()

for (i in 1:length(mt_event$sno)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)

  date_ranges_h[[i]] <- day[[1]] # combine all dataframe in list
}
date_ranges_h <- do.call(paste, c(as.list(date_ranges_h), sep = "|"))

# reading event for  diwali
mt_event <- googlesheets4::read_sheet(path_event_mstr, skip = 7)
setDT(mt_event)
mt_event <- mt_event[(mt_event$`event(acronym)` %in% c("diwali(D)") & mt_event$year %in% year_data$Year), ]


if (length(file_name) > 0) {
  # get duration for newly add events
  period <- strsplit(basename(file_name), "_")[[1]][1]
  end_period_diwali <- strsplit(period, "-")[[1]][3]

  # get last row details on which event already occured
  last_sno <- mt_event[toupper(format(as.Date(mt_event$`end period`), "%d%b%Y")) %in% end_period_diwali, c("sno")]

  # get next rows details on which events will be occured
  new_index <- mt_event[mt_event$sno > as.numeric(last_sno), which = TRUE]
  if (length(new_index) > 0) {
    mt_event <- mt_event[new_index[1]:length(mt_event$sno), ]
  } else {
    stop()
  }
}

# combine all data in particular list
date_ranges_d <- list()

for (i in 1:length(mt_event$sno)) {
  start_dates <- as.Date(mt_event$`start period`[i])
  end_dates <- as.Date(mt_event$`end period`[i])

  day <- REQUIRED_PERIODS(start_dates, end_dates)

  date_ranges_d[[i]] <- day[[1]] # combine all dataframe in list
}
date_ranges_d <- do.call(paste, c(as.list(date_ranges_d), sep = "|"))


# get date range for both holi and diwali
date_ranges_h_d <- paste0(date_ranges_d, "|", date_ranges_h)

# get only eid days not in holi and diwali days
date_range_e_not_hd <- setdiff(date_ranges_eid, date_ranges_h_d)


# part3 : customer purchased in eid but not in holi and diwali [MUSLIM FEST]
# criteria2: customer who shopped eid period but not in holi & diwali period
# read txn data from folder for shooped in eid but not in holi and diwali
mt_bill <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(date_range_e_not_hd, collapse = "|"), recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "DESC2", "Bill_Qty_SUM", "Division_Group", "Department")
)
setDT(mt_bill)
mt_bill_mfest <- mt_bill[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Bill_Date" = Bill_Date, "INVATTR2_NAME" = INVATTR2_NAME,
  "DESC2" = DESC2, "Bill_Qty_SUM" = Bill_Qty_SUM, "Division_Group" = Division_Group, "Department" = Department
)]

mt_bill_mfest <- mt_bill_mfest[, c("Customer_Mobile")]

# remove duplicates
mt_bill_mfest <- mt_bill_mfest[!(duplicated(mt_bill_mfest)), ]
# end of read txn data from folder for shooped in eid but not in holi and diwali


# part 2: [MUSLIM FEST MUSLIM DEPT]
# criteria3: customer who shopped kurta & pyjama during eid period
# read txn data from folder for shopped kurta-pyjama in eid
mt_bill <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(date_ranges_eid, collapse = "|"), recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "DESC2", "Bill_Qty_SUM", "Division_Group", "Department")
)
setDT(mt_bill)
mt_bill_eid <- mt_bill[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Bill_Date" = Bill_Date, "INVATTR2_NAME" = INVATTR2_NAME,
  "DESC2" = DESC2, "Bill_Qty_SUM" = Bill_Qty_SUM, "Division_Group" = Division_Group, "Department" = Department
)]

# filter for apparels
mt_bill_eid <- mt_bill_eid[mt_bill_eid$Division_Group == "1.Apparels", ]

# remove is.na
mt_bill_eid <- mt_bill_eid[!(is.na(mt_bill_eid$Customer_Mobile)), ]

# filter for Bill_Qty_SUM > 0
mt_bill_eid <- mt_bill_eid[Bill_Qty_SUM > 0, ]

# filter for kurta-pyjama  dept only
mt_bill_mfest_mdept <- mt_bill_eid[tolower(mt_bill_eid$Department) %like% "^kurta pyjama", ]

mt_bill_mfest_mdept <- mt_bill_mfest_mdept[, c("Customer_Mobile")]

# remove duplicates
mt_bill_mfest_mdept <- mt_bill_mfest_mdept[!(duplicated(mt_bill_mfest_mdept)), ]
# end of read txn data from folder for shooped kurta-pyjama in eid


# part 3:[MUSLIM NAME]
# criteria4: customer who has muslim names
# customer name (muslim name) details

mt_cust_name <- READ_FOLDER(
  pathFold = path_cust_name, patternFile = "parquet", recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "First_Name", "Last_Name")
)

setDT(mt_cust_name)
# create customer name from first and last name of customer
mt_cust_name$customer_name <- trimws(paste0(mt_cust_name$First_Name, " ", mt_cust_name$Last_Name))

# remove null values of customer name
mt_cust_name <- mt_cust_name[!(is.na(mt_cust_name$customer_name)), ]

# remove blank name
mt_cust_name <- mt_cust_name[!(mt_cust_name$customer_name == ""), ]

# mt_cust_name <- mt_cust_name[,c("Customer_Mobile","customer_name")]

# Define regular expression to match Muslim names
muslim_regex <- READ_FOLDER(pathFold = "D:/products/clm_generators/input/muslim_names", patternFile = "csv", recursiveFile = TRUE)

# extract only muslim name customers
mt_cust_name_f <- mt_cust_name[First_Name %in% muslim_regex$Name, ]

mt_cust_name_l <- mt_cust_name[Last_Name %in% muslim_regex$Name,]

mt_cust_name_c <- mt_cust_name[customer_name %in% muslim_regex$Name,]

mt_cust_name <- rbind(mt_cust_name_f[,c("Customer_Mobile","customer_name")],mt_cust_name_l[,c("Customer_Mobile","customer_name")],
                 mt_cust_name_c[,c("Customer_Mobile","customer_name")])

mt_cust_name <- mt_cust_name[!(duplicated(mt_cust_name)), ]

mt_mname <- mt_cust_name[!(duplicated(mt_cust_name)), ]

# set datatype of customer mobile
mt_mname$Customer_Mobile <- as.character(mt_mname$Customer_Mobile)

# part 4: [MUSLIM DEPT]
# criteria1: customer who shopped for burkha & hijab

# define a function to apply to each table
chk_empty_tbl <- function(dt) {
  # check if the table is empty
  if (!is.data.table(dt) || nrow(dt) == 0 || ncol(dt) == 0) {
    return(NULL)
  } else {
    return(dt)
  }
}

# filter for muslim dept only
muslim_dept_list <- c("Burkha[L]", "Hijab[L]")

mt_bill_mdept <- list()

# 
# if (length(file_name) > 0) {
#   # get duration for newly add events
#   period <- strsplit(basename(file_name), "_")[[1]][1]
#   end_period_mdept <- strsplit(period, "-")[[1]][4]
# } else {
#   end_period_mdept <- NULL
# }
# 
# if (length(end_period_mdept) > 0) {
#   years <- seq(as.numeric(format(as.Date(end_period_mdept, "%d%b%y"), "%Y")), as.numeric(format(Sys.Date(), "%Y")), by = 1)
# } else {
  years <- seq(2017, as.numeric(format(Sys.Date(), "%Y")), by = 1)
# }

for (yr in 1:length(years)) {
  start_dates <- as.Date(paste0(years[yr], "-01-01"))
  end_dates <- as.Date(paste0(years[yr], "-12-31"))

  day <- REQUIRED_PERIODS(start_dates, end_dates)

  # read txn data from folder for shopped burkha and hijab
  mt_bill <- READ_FOLDER(
    pathFold = path_data, patternFile = "parquet", contain = day, recursiveFile = TRUE,
    selCols = c("Customer_Mobile", "Bill_Date", "Bill_Qty_SUM", "Division_Group", "Department")
  )

  if (nrow(mt_bill) > 0) {
    # filter for apparels
    mt_bill <- mt_bill[mt_bill$Division_Group == "1.Apparels", ]

    # remove is.na
    mt_bill <- mt_bill[!(is.na(mt_bill$Customer_Mobile)), ]

    # filter for Bill_Qty_SUM > 0
    mt_bill <- mt_bill[Bill_Qty_SUM > 0, ]

    # filter for burkha and hijab only
    mt_bill <- mt_bill[tolower(mt_bill$Department) %in% tolower(muslim_dept_list), ]

    if (nrow(mt_bill) > 0) {

      # check mobile number
      mt_bill <- mob_check(mt_bill, "Customer_Mobile")

      # Removing duplicated records
      mt_bill <- mt_bill[!duplicated(Customer_Mobile), ]

      # set datatype of customer mobile
      mt_bill$Customer_Mobile <- as.character(mt_bill$Customer_Mobile)

      mt_bill_mdept[[yr]] <- mt_bill
    }
  }
}

# if(nrow(mt_bill_mdept)>0){
mt_bill_mdept <- mt_bill_mdept[lapply(mt_bill_mdept, length) > 0]

# join all data of different duration
mt_bill_mdept <- Reduce(full_join, mt_bill_mdept)

# replace na with 0
mt_bill_mdept[is.na(mt_bill_mdept)] <- 0

setDT(mt_bill_mdept)
mt_bill_mdept <- mt_bill_mdept[, .(
  "Customer_Mobile" = as.character(Customer_Mobile), "Bill_Date" = Bill_Date,
  "Bill_Qty_SUM" = Bill_Qty_SUM, "Division_Group" = Division_Group, "Department" = Department
)]

mt_bill_mdept <- mt_bill_mdept[, c("Customer_Mobile")]

# remove duplicates
mt_bill_mdept <- mt_bill_mdept[!(duplicated(mt_bill_mdept)), ]
# }else{
#   mt_bill_mdept <- data.table(Customer_Mobile = c("9999999999"))
# }

# set output file name and path
# filename_format
# last_date_eid <- as.Date(str_sub(date_ranges_eid, -9, -1), format = "%d%b%Y")
last_date_eid <- str_sub(date_ranges_eid, -9, -1)
last_date_holi <- str_sub(date_ranges_h, -9, -1)
last_date_diwali <- str_sub(date_ranges_d, -9, -1)

# filename to be saved
file_name_to_save <- paste0(last_date_eid, "_", last_date_holi, "_", last_date_diwali, "_CUSTOMER_MUSLIM.parquet")

mt_cust <- data.table()
# apply the function to each table using lapply()
mt_cust <- lapply(list(mt_bill_mfest[, c("Customer_Mobile")], mt_bill_mfest_mdept[, c("Customer_Mobile")], mt_bill_mdept[, c("Customer_Mobile")], mt_mname[, c("Customer_Mobile")]), chk_empty_tbl)

# bind the non-empty tables using rbindlist()
mt_cust <- rbindlist(mt_cust)

# print the combined table
if (!is.null(mt_cust)) {
  print(mt_cust)
} else {
  print("All tables are empty.")
}

# check mobile number
mt_cust <- mob_check(mt_cust, "Customer_Mobile")

# Removing duplicated records
mt_cust <- mt_cust[!duplicated(Customer_Mobile), ]

# set datatype of customer mobile
mt_cust$Customer_Mobile <- as.character(mt_cust$Customer_Mobile)

mt_cust$seg_muslim <- 1

mt_cust <- mt_cust[, c("Customer_Mobile", "seg_muslim"), with = FALSE]

mt_cust$seg_muslim <- as.character(mt_cust$seg_muslim)

# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")

# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))

  # file saved in particular path
  arrow::write_parquet(mt_cust, paste0(out_path, "/", file_name_to_save))

  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_MUSLIM"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_MUSLIM"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_MUSLIM")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_MUSLIM")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_MUSLIM")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: muslim_segment.R", type = "text")
