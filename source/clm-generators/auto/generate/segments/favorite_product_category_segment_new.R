#' Date: 25APR2024 Time: 13:00PM
#' Author: Archana Pandita 

# Objective: automate new  favorite_product_category segment generation 
# favorite_product_category: customer favorite department/group of department classified based on equal weightage to product quantity, sale value and visits in selected period
# product(s) 1...n
# qty  = q1..n,, frequency = f 1...n
# fpc score(i) = 0.50 * q(i) + 0.50 * f(i)
# i = 1....n
# fpc = product(s) with maximum fpc score

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
# install.packages("timeDate") # Install timeDate package
library("timeDate")
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
path_data <- file.path(database$database$transaction$offline)
ids_to_send <- c("745474876")

out_path <- file.path(path_proj, "output", "monthwise_txn_data")

#---- Function Calls, Data Reading & Manipulations ----
# get last two years
# years <- c(year(Sys.Date()) - 2, year(Sys.Date()) - 1, year(Sys.Date()))

# Get the current year
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Create a data.table with a sequence of years
years <- data.table(year = seq(2017, current_year))

# get month names
no_of_month <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "01", "02", "03")
current_month <- month(Sys.Date())

start_index <- match(format(Sys.Date(), "%m"), no_of_month)
# no_of_months <- c(no_of_month[start_index:length(no_of_month)], no_of_month[1:(start_index-1)])

# Get the current year
current_year <- as.integer(format(Sys.Date(), "%Y"))

# Create a data.table with a sequence of months
no_of_months <- data.table(month = 1:12)


# get month & year combination of last two years
# Get the current year and month
current_year <- lubridate::year(Sys.Date())
current_month <- lubridate::month(Sys.Date()) + 1

# Determine the start and end years of the last three financial years
if (current_month >= 4) {
  # If the current month is April or later, the current year is the end year of the most recent financial year
  end_year <- current_year
  start_year <- current_year - 2
} else {
  # If the current month is before April, the current year is the end year of the second most recent financial year
  end_year <- current_year - 1
  start_year <- current_year - 3
}

# Create a sequence of months and years for the last three financial years
# start_date <- lubridate::make_date(start_year, 4, 1) # Start date is 1st April of start_year
# end_date <- lubridate::make_date(end_year, 3, 31) # End date is 31st March of end_year

#remove previous month file because it consist only data on which script run
fname <- paste0(toupper(paste0(month.abb[month(Sys.Date())], year(Sys.Date()))), "_BILLS.parquet")
file.remove(file.path(out_path, fname))

end_date <- Sys.Date() - 1
start_date <- as.Date("2018-04-01")#seq(end_date, length = 2, by = "-2 years")[2]


months <- seq(start_date, end_date, by = "month") # Create a sequence of months
month_years <- paste0(toupper(format(months, "%b%Y")), "_BILLS.parquet") # Format the sequence as month and year (e.g. "Apr2021")
curr_mn_yr <- paste0(toupper(paste0(format(Sys.Date(), "%b"), format(Sys.Date(), "%Y"))), "_BILLS.parquet")
# concatenate all values into a single string with a "|" separator
files_nm <- paste(as.matrix(month_years), collapse = "|")
files_nm <- paste0(files_nm, "|", curr_mn_yr)
for (yr in 1:nrow(years))
{
  # print(years[yr])
  for (mn in 1:nrow(no_of_months))
  {
    # print(months[mn])
    # view list of files in particular location
    files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
    file_name_to_save <- paste0(toupper(paste0(month.abb[as.numeric(no_of_months[mn])], years[yr])), "_BILLS.parquet")
    cur_month_filename <- paste0(toupper(paste0(month.abb[month(Sys.Date())], year(Sys.Date()))), "_BILLS.parquet")

    # check file exist or not

    file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
    if ((((!file_status) | (file_status & (file_name_to_save == cur_month_filename))) & (file_name_to_save %in% unlist(strsplit(files_nm, "\\|"))))) {
      # if (((!file_status) | ( (file_status & (file_name_to_save == cur_month_filename)) &  (file_name_to_save %in% unlist(strsplit(files_nm, "\\|")))  )   )) {
      #    print("sd")
      # }else{print("no")}

      files <- list.files(file.path(out_path))

      # get data month and year wise
      if (mn <= 9) {
        start_date <- as.Date(paste(years[yr], no_of_months[mn], "01", sep = "-"))
        end_date <- as.Date(timeLastDayInMonth(start_date))
        day <- REQUIRED_PERIODS(start_date, end_date)
        file_name_to_save <- paste0(toupper(month.abb[as.numeric(no_of_months[mn])]), years[yr], "_BILLS.parquet")
      } else {
        if (yr == length(years)) {
          print("if")
          yr <- yr
        } else {
          print("else")
          yr <- yr + 1
        }
        start_date <- as.Date(paste(years[yr], no_of_months[mn], "01", sep = "-"))
        end_date <- as.Date(timeLastDayInMonth(start_date))
        day <- REQUIRED_PERIODS(start_date, end_date)
        file_name_to_save <- paste0(toupper(month.abb[as.numeric(no_of_months[mn])]), years[yr], "_BILLS.parquet")
      }
      print(paste0(yr, mn))
      # read rfm data from folder
      mt_cust <- READ_FOLDER(
        pathFold = path_data, patternFile = "parquet", contain = day, recursiveFile = TRUE,
        selCols = c("Customer_Mobile", "Bill_Date", "Department", "Bill_Qty_SUM", "Net_Amount_SUM")
      )

      if (nrow(mt_cust) > 0) {
        # get frequency, qty,amount on basis of customer mobile & department
        mt_cust <- mt_cust[, .(Frequency = n_distinct(Bill_Date), Qty = sum(Bill_Qty_SUM), Sale_Value = sum(Net_Amount_SUM)), by = c("Customer_Mobile", "Department")]

        # file saved in particular path
        arrow::write_parquet(mt_cust, paste0(out_path, "/", file_name_to_save))
      }

      # remove previous dated files save in folder
      if (length(list.files(file.path(out_path))) == 2) {
        # file.remove(file.path(out_path, files))
        LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY"))
      } else {
        LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY"))
      }
      LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY")
    } else {
      LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY")
    }

    LOG_INFO(
      what = paste0("File ", file_name_to_save, " succesfully saved"),
      where = file.path(path_proj, "logs"),
      special_operation = "@Monitor",
      file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY")
    )
  }
}

# get month & year combination of last two years
# Get the current year and month
current_year <- lubridate::year(Sys.Date())
current_month <- lubridate::month(Sys.Date()) + 1

# Determine the start and end years of the last three financial years
if (current_month >= 4) {
  # If the current month is April or later, the current year is the end year of the most recent financial year
  end_year <- current_year
  start_year <- current_year - 2
} else {
  # If the current month is before April, the current year is the end year of the second most recent financial year
  end_year <- current_year - 1
  start_year <- current_year - 3
}

# Create a sequence of months and years for the last three financial years
start_date <- as.Date("2018-01-01") #lubridate::make_date(start_year,  month(Sys.Date()), 1) # Start date is 1st April of start_year
end_d <- ceiling_date(Sys.Date(), "month") - days(1)
end_date <- lubridate::make_date(end_year, month(Sys.Date()), day(end_d)) # End date is 31st March of end_year
months <- seq(start_date, end_date, by = "month") # Create a sequence of months
month_years <- toupper(format(months, "%b%Y")) # Format the sequence as month and year (e.g. "Apr2021")

# concatenate all values into a single string with a "|" separator
day <- paste(as.matrix(month_years), collapse = "|")

# read month & year wise consolidate data of last two years only
# read rfm data from folder
mt_cust <- READ_FOLDER(
  pathFold = out_path, patternFile = "parquet", contain = day, recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Frequency", "Department", "Qty" )
)

# remove na values
mt_cust <- mt_cust[!(is.na(Customer_Mobile)), ]


prod <- read.csv("D:/database/mstr/product/product_master.csv")

mt_cust <- left_join(mt_cust,prod[,c("Division_Group","Department")], by = c("Department"))

mt_cust <- mt_cust[mt_cust$Division_Group %in% c("1.Apparels"),]#2.Non Apparels

mt_cust <- mt_cust[,c( "Customer_Mobile","Department", "Frequency", "Qty"  )]

# remove duplicates
# mt_cust <- mt_cust[duplicated(mt_cust),]

# get frequency, qty,amount on basis of customer mobile & department on consolidated data
mt_cust <- mt_cust[, .(Frequency = sum(Frequency), Qty = sum(Qty) ), by = c("Customer_Mobile", "Department")]

# formula for calculating fpc score
mt_cust$fpc_score <- (((mt_cust$Qty) * 0.50) +  ((mt_cust$Frequency) * 0.50))

# get max fpc score for customer department wise
mt_cust <- mt_cust[, .(fpc_score = max(fpc_score, na.rm = TRUE)), .(Customer_Mobile,Department)]

# Order the data.table by fpc_score in descending order
setorder(mt_cust, Customer_Mobile, -fpc_score)

# Use the unique() and .SD functions to select the top row for each customer
mt_cust <- mt_cust[, .SD[1], by = Customer_Mobile]

# get only the top one row with the max fpc_score for each customer
# mt_cust <- mt_cust[, .SD[rank(-fpc_score) == 1], by = .(Customer_Mobile)]

# Removing duplicated records
mt_cust <- mt_cust[!duplicated(mt_cust), ]

# check mobile number
mt_cust <- mob_check(mt_cust, "Customer_Mobile")

# set datatype of customer mobile
mt_cust$Customer_Mobile <- as.character(mt_cust$Customer_Mobile)

# change name
setnames(mt_cust,old = "Department",new = "seg_fpc_new")

mt_cust <- mt_cust[, c("Customer_Mobile", "seg_fpc_new"), with = FALSE]

# set datatype of seg_fpc_new
# mt_cust$Department <- as.character(mt_cust$Department)
mt_cust$seg_fpc_new <- as.character(mt_cust$seg_fpc_new)
# total number of customers belongs to  fpc segment.
nrow(mt_cust)

# set path for output
min_date <- tstrsplit(day, "\\|")[[1]]
max_date <- tstrsplit(day, "\\|")[[length(tstrsplit(day, "\\|"))]]
out_fpath <- file.path(database$database$vmart$segment$fpc_new)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_FPC_NEW.parquet")

# view list of files in particular location
files <- file.info(list.files(path = out_fpath, full.names = T), pattern = "*.parquet")

# check file exist or not
file_status <- file.exists(file.path(out_fpath, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_fpath))

  # file saved in particular path
  arrow::write_parquet(mt_cust, paste0(out_fpath, "/", file_name_to_save))

  # remove previous dated files save in folder
  if (length(list.files(file.path(out_fpath))) == 2) {
    file.remove(file.path(out_fpath, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY_NEW"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY_NEW"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY_NEW")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY_NEW")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_FAVORITE_PRODUCT_CATEGORY_NEW")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: fpc_segment_new.R", type = "text")
