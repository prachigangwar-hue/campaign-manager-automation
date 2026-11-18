rm(list = ls())
source("C:/R - Projects/library.R")
path_proj_19 <- GET_PROJECT_DIR(proj_id = "pid_19")
path_lib <- file.path(path_proj_19, "auto/library")
path_dbase <- GET_PROJECT_DIR(proj_id = "pid_35")
path_date_variable <- "D:/products/mkt_collect_dlr_rm/input/date_variable_last.txt"
path_off_today_7 <- file.path(path_dbase, "rfm/customer/division_group/offline/tagged/td/today_7")
path_off_today <- file.path(path_dbase, "rfm/customer/division_group/offline/tagged/td/today")

source(file.path(path_lib, "read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib, "log_lib.R"))

# Function to load data from the source
load_data <- function() {
  mt_rfm_td7 <- READ_FOLDER(pathFold = path_off_today_7, patternFile = "parquet", selCols = "Customer_Mobile")
  mt_rfm_td <- READ_FOLDER(pathFold = path_off_today, patternFile = "parquet", selCols = "Customer_Mobile")
  mt_rfm <- rbindlist(list(mt_rfm_td,mt_rfm_td7));rm(mt_rfm_td);rm(mt_rfm_td7);gc()
  setDT(mt_rfm)
  mt_rfm <- mt_rfm[!duplicated(mt_rfm)]
  mt_rfm$Customer_Mobile <- as.character(mt_rfm$Customer_Mobile)
  return(mt_rfm)
}

# Initialize data and timestamp once at the start
mt_rfm <- load_data()

# Initialize with the current date
last_refresh_timestamp <- Sys.Date()
last_refresh_timestamp <- as.character(last_refresh_timestamp)
writeLines(last_refresh_timestamp, con = path_date_variable)
print(last_refresh_timestamp)

# Function to check and refresh data if it's a new day
check_and_refresh_data <- function() {
  current_date <- Sys.Date()
  current_date <- as.character(current_date)
  cat("Today's date: ",current_date,"\n")
  last_refresh_timestamp <- readLines(path_date_variable)
  cat("Last refreshed date: ",last_refresh_timestamp,"\n")
  if (current_date != last_refresh_timestamp) {
    # It's a new day, refresh the data
    mt_rfm <- load_data()
    last_refresh_timestamp <- current_date
    last_refresh_timestamp <- as.character(last_refresh_timestamp)
    writeLines(last_refresh_timestamp, con = path_date_variable)
  }
}


#* @get /check_customer_type
function(req,res,mobile){
  
  # # Verify authentication
  # auth_header <- req$HTTP_AUTHORIZATION
  # if (is.null(auth_header) || !startsWith(auth_header, "Basic ")) {
  #   res$status <- 401
  #   return("Unauthorized")
  # }
  # 
  # if (!verify_user(auth_header, basic_auth_creds_misc)) {
  #   res$status <- 401
  #   return("Invalid Credentials")
  # }
  
  check_and_refresh_data()
  
  
  if (!is.na(fmatch(mobile, mt_rfm$Customer_Mobile))){
    return(list("status"= TRUE))
  } else {
    return(list("status"= FALSE))
  }
  
}


