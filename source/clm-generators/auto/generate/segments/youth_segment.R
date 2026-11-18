#' Date: 20FEB2022 Time: 12:00PM
#' Author: Archana Pandita

# Objective: automate youth segment generation
# youth: who shopped for atleast 1 of the dept in youth_dept_list
# youth_dept_list = Department in ( 'Top[L]', 'Skirt[L]', 'T-Shirt[M]' , etc..)

#----Cleaning Enviornment----
rm(list = ls())
gc()

#----Sourcing Libraries and external files----
# loading libraries
library(dplyr)
library(data.table)
library(stringr)
library(lubridate)

# sourcing external file
cat("\n\n")
cat("Sourcing external files", "\n")
source("C:/R - Projects/library.R")

path_proj <- GET_PROJECT_DIR(proj_id = "pid_29")
path_lib <- file.path(path_proj, "auto/library")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")

# use config
database <- config::get(file = file.path(path_proj, "config.yml"))

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

#---- Function Calls, Data Reading & Manipulations ----
# Considering period from 01 mar 2022 to 31 may 2022
# year <- year(Sys.Date()) - 1
# today <- Sys.Date() # get current date
# ifelse(today < ymd(paste(year(today), '-04-01')), # check if current date is before or after April 1st
#        start_date <- ymd(paste(year(today) - 1, '-04-01')), # if before, use last year's April 1st
#        start_date <- ymd(paste(year(today), '-04-01'))) # if after, use current year's April 1st
# 
# end_date <- start_date %m+% years(1) - days(1) # get end of FY (subtract 1 day from next year's April 1st)

end_date <- Sys.Date() - 1
start_date <- seq(end_date, length = 2, by = "-1 years")[2]

day <- REQUIRED_PERIODS(start_date,end_date)

# read rfm data from folder
dt_bill <- READ_FOLDER(
  pathFold = path_data, patternFile = "parquet", contain = paste0(day, collapse = "|"), recursiveFile = TRUE,
  selCols = c("Customer_Mobile", "Bill_Date", "INVATTR2_NAME", "DESC2", "Bill_Qty_SUM", "Division_Group","Department")
)

#---- Displaying period, ----
# get the minimum and maximum dates as the period
# set output file name and path
mt_date <- data.table()
setDT(mt_date)
mt_date$bill_dt <- as.data.table(unique(dt_bill$Bill_Date))

mt_date$bill_dt <- as.Date(mt_date$bill_dt, "%d-%b-%y")

min_date <- toupper(format(as.Date(min(mt_date[["bill_dt"]])), "%d%b%Y"))
max_date <- toupper(format(as.Date(max(mt_date[["bill_dt"]])), "%d%b%Y"))

out_path <-  file.path(database$database$vmart$segment$youth)
file_name_to_save <- paste0(min_date, "-", max_date, "_CUSTOMER_YOUTH.parquet")

# filter for apparels
dt_bill <- dt_bill[dt_bill$Division_Group == "1.Apparels", ]

# filter for summer season
dt_bill <- dt_bill[dt_bill$INVATTR2_NAME %in% c("SUMMER"), ]

# filter for fresh product for summer season
# desc_ss <- paste0("SS", substr(year, 3, 4))

desc_ss <- c("SS21","SS22","SS23","SS24","SS25","SS26","SS27","SS28")

dt_bill <- dt_bill[dt_bill$DESC2 %in% desc_ss, ]

#remove is.na 
dt_bill <- dt_bill[!(is.na(dt_bill$Customer_Mobile)), ]

# filter for Bill_Qty_SUM > 0
dt_bill <- dt_bill[Bill_Qty_SUM > 0, ]

# filter for youth only
#youth_dept_list = c( 'Top[L]', 'Skirt[L]', 'T-Shirt[M]')

youth_dept_list =c('T-Shirt[M]','Jeans[M]','Kurta Pyjama[M]',
                   'Cotton Trouser[M]',
                   'Pullover [M]',
                   'Jacket[M]',
                   'Bermuda[M]',
                   'W-Pyjama [M]',
                   'Pyjama[M]',
                   'Party Wear Shirt[M]',
                   'Casual Jackets [M]',
                   'Kurta[M]',
                   'T-Shirt FS[M]',
                   'Ethnic Winter [M]',
                   'Half Pant[M]',
                   'Casual Shirt FS[M]',
                   'Jamaican[M]',
                   'Shirt Pcs[M]',
                   'Pant Pcs[M]',
                   'Sherwani[M]',
                   'Casual Shirt[M]',
                   'Formal Blazer [M]',
                   'Formal Shirt FS[M]',
                   'Safari Suit[M]',
                   'Suit2Pc[M]',
                   'Sweater [M]',
                   'Mens-Upper-B1G1[M]',
                   'Casual Blazer [M]',
                   'Track Suit[M]',
                   'Coat [M]',
                   'Jeans B1G1[M]',
                   'Nehru Jacket[M]',
                   'Sweat Shirt [M]',
                   'Formal Shirt[M]',
                   'Pant & Shirt Pcs[M]',
                   'Suit3Pc[M]',
                   'Sando[M]',
                   'Pant[M]',
                   'Jacket Pcs [M]',
                   'Khadi Kurta[M]',
                   'Formal Trouser[M]',
                   'Khadi Kurta Pyjama[M]',
                   'Lohi [M]',
                   'Sweater FS [M]',
                   'Pullover Sleevless [M]',
                   'Shirting Fabric[P]',
                   'Jeans[L]',
                   'W-Dress Fabric [L]',
                   'Cotton Saree[L]',
                   'Capri [L]',
                   'Ethnic Suit[L]',
                   'Top[L]',
                   'Pant[L]',
                   'Synthetic Saree[L]',
                   'Dupatta[L]',
                   'Track Pants[L]',
                   'W-Kurti [L]',
                   'Dress[L]',
                   'Skirt [L]',
                   'Kurta[L]',
                   'W-Legging[L]',
                   'Pyjama[L]',
                   'Top FS[L]',
                   'Synthetic Print[L]',
                   'Kurti[L]',
                   'Fusion Palazzo[L]',
                   'Legging[L]',
                   'Gown[L]',
                   'Hijab[L]',
                   'Handloom Saree',
                   'Pullover [L]',
                   'Trouser[L]',
                   'Shawl [L]',
                   'Cardigan [L]',
                   'Shrug[L]',
                   'Knit Churidaar[L]',
                   'Sweatshirt[L]',
                   'Saree B1G1[L]',
                   'W-Blouse [L]',
                   'Shirt[L]',
                   'W-Pyjama[L]',
                   'Shorts[L]',
                   'Tant Saree[L]',
                   'Jegging[L]',
                   'Lehanga Chunni[L]',
                   'T-Shirt[L]',
                   'Salwar Suit[L]',
                   'Jacket Pcs [L]',
                   'Jacket[L]',
                   'W-Stole[L]',
                   'Nighty[L]',
                   'Skirt Top[L]',
                   'Dress Fabric-Cotton[L]',
                   'Suit Fabric[L]',
                   'Fusion Ghagroo[L]',
                   'Blouse[L]',
                   'Burkha[L]',
                   'Kaftan[L]',
                   'Petikot',
                   'Kotti[L]',
                   'Night Suit[L]',
                   'Salwar[L]',
                   'Dress Fabric B1G1[L]',
                   'Saree[L]',
                   'Overcoat[L]',
                   'Sweater [L]',
                   'Lehanga Saree[L]',
                   'Bermuda[M]',
                   'Half Pant[M]',
                   'Jamaican[M]',
                   'Pyjama[M]',
                   'Sando[M]',
                   'Track Suit[M]',
                   'W-Pyjama [M]',
                   'Dress Fabric B1G1[L]',
                   'Dress Fabric-Cotton[L]',
                   'Ethnic Suit[L]',
                   'Gown[L]',
                   'Hijab[L]',
                   'Salwar Suit[L]',
                   'Suit Fabric[L]',
                   'Dupatta[L]',
                   'Kurta[L]',
                   'Kurti[L]',
                   'Salwar[L]',
                   'Blouse[L]',
                   'Cotton Saree[L]',
                   'Handloom Saree',
                   'Lehanga Chunni[L]',
                   'Lehanga Saree[L]',
                   'Petikot',
                   'Saree B1G1[L]',
                   'Saree[L]',
                   'Synthetic Print[L]',
                   'Synthetic Saree[L]',
                   'Tant Saree[L]',
                   'Nighty[L]',
                   'Shawl [L]',
                   'W-Blouse [L]',
                   'W-Kurti [L]',
                   'W-Legging[L]',
                   'W-Stole[L]',
                   'Cardigan [L]',
                   'Khadi Kurta Pyjama[M]',
                   'Khadi Kurta[M]',
                   'Nehru Jacket[M]',
                   'Pant & Shirt Pcs[M]',
                   'Pant Pcs[M]',
                   'Safari Suit[M]',
                   'Sherwani[M]',
                   'Suit2Pc[M]',
                   'Suit3Pc[M]',
                   'Cotton Trouser[M]',
                   'Formal Trouser[M]',
                   'Pant[M]',
                   'Casual Shirt FS[M]',
                   'Casual Shirt[M]',
                   'Formal Shirt FS[M]',
                   'Formal Shirt[M]',
                   'Party Wear Shirt[M]',
                   'Shirt Pcs[M]',
                   'Shirting Fabric[P]',
                   'Coat [M]',
                   'Formal Blazer [M]',
                   'Lohi [M]',
                   'Pullover [M]',
                   'Pullover Sleevless [M]',
                   'Kurta Pyjama[M]',
                   'Jeans B1G1[M]',
                   'Jeans[M]',
                   'Kurta[M]',
                   'Mens-Upper-B1G1[M]',
                   'T-Shirt FS[M]',
                   'T-Shirt[M]',
                   'Casual Blazer [M]',
                   'Casual Jackets [M]',
                   'Ethnic Winter [M]',
                   'Jacket Pcs [M]',
                   'Jacket[M]',
                   'Sweat Shirt [M]',
                   'Sweater [M]',
                   'Sweater FS [M]',
                   'Jacket Pcs [L]',
                   'Jacket[L]',
                   'Overcoat[L]',
                   'Sweater [L]',
                   'Jeans[L]',
                   'W-Dress Fabric [L]',
                   'Capri [L]',
                   'Top[L]',
                   'Pant[L]',
                   'Track Pants[L]',
                   'Dress[L]',
                   'Skirt [L]',
                   'Pyjama[L]',
                   'Top FS[L]',
                   'Fusion Palazzo[L]',
                   'Legging[L]',
                   'Pullover [L]',
                   'Trouser[L]',
                   'Shrug[L]',
                   'Knit Churidaar[L]',
                   'Sweatshirt[L]',
                   'Shirt[L]',
                   'W-Pyjama[L]',
                   'Shorts[L]',
                   'Jegging[L]',
                   'T-Shirt[L]',
                   'Skirt Top[L]',
                   'Fusion Ghagroo[L]',
                   'Burkha[L]',
                   'Kaftan[L]',
                   'Kotti[L]',
                   'Night Suit[L]')

# create new column as YOUTH 
dt_bill$seg_youth <- 0

dt_bill$seg_youth <- ifelse(dt_bill$Department %in% youth_dept_list,1,0)

dt_bill <- dt_bill[!(is.na(dt_bill$seg_youth)), ]

# check mobile number
dt_bill <- mob_check(dt_bill, "Customer_Mobile")

# Removing duplicated records
dt_bill <- dt_bill[!duplicated(Customer_Mobile), ]

#get data where youth flag is setted 1
dt_bill <- dt_bill[dt_bill$seg_youth == 1, ]

dt_bill <- dt_bill[, c("Customer_Mobile", "seg_youth"), with = FALSE]

# set datatype of customer mobile
dt_bill$Customer_Mobile <- as.character(dt_bill$Customer_Mobile)
dt_bill$seg_youth <- as.character(dt_bill$seg_youth)
# total number of customers belongs to youth segment.
nrow(dt_bill)


# get all files on particular location
files <- file.info(list.files(path = out_path, full.names = T), pattern = "*.parquet")
# check file exist or not
file_status <- file.exists(file.path(out_path, basename(file_name_to_save)))
if (!file_status) {
  files <- list.files(file.path(out_path))
  
  # file saved in particular path
  arrow::write_parquet(dt_bill, paste0(out_path, "/", file_name_to_save))
  
  # remove previous dated files save in folder
  if (length(list.files(file.path(out_path))) == 2) {
    file.remove(file.path(out_path, files))
    LOG_INFO(what = paste0("Successful!! Data Saved ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_YOUTH"))
  } else {
    LOG_INFO(what = paste0("Failed!! More than one files in folder ", file_name_to_save), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_YOUTH"))
  }
  LOG_INFO(what = paste0("File ", file_name_to_save, " succesfully copied"), where = file.path(path_proj, "logs"), special_operation = "@Monitor", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_YOUTH")
} else {
  LOG_INFO(what = paste0("File not found. Please Check server"), where = file.path(path_proj, "logs"), special_operation = "@Error", file_name_sub_script = "CLM_EXTRACT_DAILY_CUSTOMER_YOUTH")
}

LOG_INFO(
  what = paste0("File ", file_name_to_save, " succesfully saved"),
  where = file.path(path_proj, "logs"),
  special_operation = "@Monitor",
  file_name_sub_script = paste0("CLM_EXTRACT_DAILY_CUSTOMER_YOUTH")
)

SEND_TO_TELEGRAM(chat_id = ids_to_send, message = "Script Executed: youth_segment.R", type = "text")
