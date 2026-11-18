# Date: 28JAN2023 Time: 12:20 PM
# Author: Abhishek Agnihotri

# --- objective ----
# Use existing segments to generate a campaign master
# and expose it as an API in order to be available for programmatic
# access and manual run

# This is also an experiment with config file in order to store the
# data paths and use them inside another R-Script
# I hope it will work as smooth as butter

# config file will be handles using config package from R

# 1. local functions will be maintained in a different script
# 2. variable and path declaration will be handled in config file

# ---- loading libraries ---- 
# cleaning enviornment

#' @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if(req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}

# functions will be used to create campaign master which can be used 
# in campaign manager in order to execute the communication campaigns
#' segments: which segments to append in the master
#'          ther lower the segments the faster the data fetch in campaign manager
#'          
#* @get /egenmaster
GENERATE_CAMPAIGN_MASTER <- function(dt_segments = NULL, organization = "vmart", date_ason = Sys.Date(), is_include_preold = FALSE){
  
  # sourcing project paths
  source("C:/R - Projects/library.R")
  path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")
  path_lib <- file.path(path_proj, "auto/library")
  path_fltr <- GET_PROJECT_DIR(proj_id = "pid_42")
  
  # sourcing external functions
  source(file.path(path_lib,"read_folder_function(READ_FOLDER).R"))
  # source(file.path(path_lib,"rfm-lib.R"))
  source(file.path(path_lib,"daily-kpi-lib.R"))
  source(file.path(path_lib,"genome-lib.R"))
  source(file.path(path_lib,"log_lib.R"))
  # source(file.path(path_lib,"chatbot-lib.R"))
  
  # loading libraries
  library(config)
  library(data.table)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(dreaweRTools)
  library(jsonlite)
  library(googlesheets4)
  library(httr)
  
  # sourcing config file
  database <- config::get(file = file.path(path_proj, "config.yml"))
  database <- database$database
  
  if (is.null(dt_segments)) {
    googlesheets4::gs4_auth(
      path = file.path("C:/R - Projects/credentials/gsheet/digital/gmail_credentials.json")
    )
    dt_segments <- read_sheet(
      ss = database$gsheets$segment$url,
      sheet = database$gsheets$segment$sheet
    )
    setDT(dt_segments)
  } else {
    cat("segment extracted: ", "\n")
    
    # Handle case where dt_segments might be JSON string
    if (is.character(dt_segments)) {
      dt_segments <- jsonlite::fromJSON(dt_segments)
      setDT(dt_segments)
    } else {
      setDT(dt_segments)  # in case it's a tibble/data.frame
    }
  }
  
  cat("dt_segments as is: ", "\n")
  print(dt_segments)   # show first 10 rows
  cat("Dimensions: ", dim(dt_segments)[1], "rows x", dim(dt_segments)[2], "cols\n")
  cat("organization", "\n")
  print(organization)
  cat("date_ason", "\n")
  print(date_ason)
  
  # filtering dt_segments where tag is TRUE
  dt_segments <- dt_segments[is_include_master == TRUE, ]
  cat("following dt_segments will be combined: ", "\n")
  cat(paste0(dt_segments$segment_name_id, collapse = ", "), "\n")
  
  # reading and standardizing customer master
  mt_cust <- READ_FOLDER(pathFold = database[[organization]]$rfm$customer$division_group$offline$tagged$td$today_7,
                         patternFile = "parquet",
                         selCols = c("Customer_Mobile", "First_Transaction_Date", "Last_Transaction_Date", "Store", "Frequency"));setDT(mt_cust)
  setnames(mt_cust, old = colnames(mt_cust), new = tolower(colnames(mt_cust)))
  setnames(mt_cust, old = "frequency", new = "frequency_td")
  mt_cust$customer_mobile <- as.numeric(mt_cust$customer_mobile)
  
  total_customers <- nrow(mt_cust)
  # removing DND customers
  dt_dnd <- READ_FOLDER(pathFold = database$vmart$segment$seg_dnd, patternFile = "parquet")
  mt_cust <- mt_cust[!customer_mobile %in% unique(dt_dnd$Customer_Mobile)]
  rm(dt_dnd);gc()
  dnd_removed <- nrow(mt_cust)
  
  dt_bir <- READ_FOLDER(database$vmart$clm_comm$birthday,patternFile = "parquet",recursiveFile = "FALSE",selCols = c("Customer_Mobile","Birthday_Month"))
  mt_cust <- mt_cust[!customer_mobile %in% unique(dt_bir$Customer_Mobile[dt_bir$Birthday_Month==month(Sys.Date())])]
  rm(dt_bir);gc()
  birthday_removed <- nrow(mt_cust)
  
  dt_anv <- READ_FOLDER(database$vmart$clm_comm$anniversary,patternFile = "parquet",recursiveFile = "FALSE",selCols = c("Customer_Mobile","Anniversary_Month"))
  mt_cust <- mt_cust[!customer_mobile %in% unique(dt_anv$Customer_Mobile[dt_anv$Anniversary_Month==month(Sys.Date())])]
  rm(dt_anv);gc()
  marriage_anv_removed <- nrow(mt_cust)
  
  dt_removed_customers <- data.table("month"=paste0(month.abb[month(Sys.Date())],"-",year(Sys.Date())),"total_customers"=total_customers,"dnd"=dnd_removed,"birthday"=birthday_removed,"marriage_anv"=marriage_anv_removed)
  fwrite(dt_removed_customers,file.path("D:/digital-transformation/apis/campaign-apis/analysis",paste0(month.abb[month(Sys.Date())],"_",year(Sys.Date()),"_scrubbed_customers.csv")))
  
  mt_cust$first_transaction_date <- as.Date(mt_cust$first_transaction_date,"%d-%b-%y")
  mt_cust$last_transaction_date <- as.Date(mt_cust$last_transaction_date,"%d-%b-%y")
  
  # number of days associated with vmart
  # start date for the data is 01-Apr-2017
  mt_cust$days_association <- as.numeric(mt_cust$last_transaction_date - mt_cust$first_transaction_date)
  mt_cust$days_recency <- as.numeric(date_ason  - mt_cust$last_transaction_date)
  
  # adding recency groups
  #mt_cust$active_group <- cut(mt_cust$days_recency, breaks = c(-Inf, 90, 150, 180, 360, 720, Inf), c("a-mr", "a-r", "a-atl", "l", "o", "p"))
  #mt_cust$customer_type <- cut(mt_cust$days_recency, breaks = c(-Inf, 180, 360, 720, Inf), c("a", "l", "o", "p"))
  
  #mt_cust$active_group[is.na(mt_cust$active_group)] <- "p"
  #mt_cust$customer_type[is.na(mt_cust$customer_type)] <- "p"
  
  # adding communication medium
  mt_comm <- READ_FOLDER(pathFold = database$master$medium, 
                         patternFile = "parquet", selCols = c("Customer_Mobile", "Whatsapp_Status"));setDT(mt_comm)
  setnames(mt_comm, old = colnames(mt_comm), new = tolower(colnames(mt_comm)))
  mt_comm <- mt_comm[!duplicated(customer_mobile),]
  mt_comm <- mt_comm[mt_comm$whatsapp_status %in% "d", ]
  mt_comm$communication_medium <- ifelse(mt_comm$whatsapp_status == "d", "w", "s")
  mt_comm$whatsapp_status <- NULL
  mt_comm$customer_mobile <- as.numeric(mt_comm$customer_mobile)
  
  mt_cust <- left_join(mt_cust, mt_comm, by = "customer_mobile")
  mt_cust$communication_medium[is.na(mt_cust$communication_medium)] <- "s"
  rm(mt_comm);gc()
  
  # adding store hierarchy
  mt_str <- READ_FOLDER(pathFold = database$master$store_auto, 
                        contain = "STORE_MASTER", 
                        selCols = c("store", "zone", "region", "language", "nature"))
  setnames(mt_str, old = colnames(mt_str), new = tolower(colnames(mt_str)))
  
  mt_cust <- left_join(mt_cust, mt_str, by = "store")
  
  # adding store tags to the master@ from Marketing team (Mayank)
  dt_str_tag <- READ_FOLDER(pathFold = database$master$str_tag,patternFile = "csv",contain = "store_tagging")
  mt_cust <- left_join(mt_cust,dt_str_tag,by="store")
  mt_cust[, c("str_eid", "str_wed", "str_hol", "str_lpf","str_rakhi","str_onam","str_fmcg")][is.na(str_eid) | is.na(str_wed) | is.na(str_hol) | is.na(str_lpf) | is.na(str_rakhi) | is.na(str_onam) | is.na(str_fmcg)] <- 0
  mt_cust$str_type[is.na(mt_cust$str_type)] <- "Closed"
  mt_cust <- mt_cust[!grepl("^FO-",store)]
  mt_cust$store_update[is.na(mt_cust$store_update)] <- mt_cust$store[is.na(mt_cust$store_update)]
  mt_cust$store <- NULL
  setnames(mt_cust,old = "store_update",new="store")
  rm(dt_str_tag);gc()
  
  # adding test control column
  mt_cust$test_control <- sample(c("c","t"), nrow(mt_cust), replace = TRUE, prob = c(0.05,0.95))
  mt_cust$customer_mobile <- as.character(mt_cust$customer_mobile)
  
  # adding dt_segments by lopping over all the available dt_segments
  for(seg in dt_segments$segment_name_id){
    
    cat("processing segment: ", seg, "\n")
    if(is.null(database[[organization]]$segment[[seg]])){
      next
    }
    # reading dt_segments
    dt_seg <- tryCatch(list("status" = "success", "value" = READ_FOLDER(pathFold = database[[organization]]$segment[[seg]],
                                                                        selCols = c("Customer_Mobile", seg),
                                                                        patternFile = "parquet")), 
                       error = function(e){return(list("status" = "error", "value" = "error"))})
    if(dt_seg$status == "error"){
      next
    }
    if(nrow(dt_seg$value)==0){
      next
    }
    
    dt_seg <- dt_seg$value
    setnames(dt_seg, old = colnames(dt_seg), new = tolower(colnames(dt_seg)))
    dt_seg <- dt_seg[!duplicated(customer_mobile),]
    dt_seg[["customer_mobile"]] <- as.character(dt_seg[["customer_mobile"]])
    
    mt_cust <- left_join(mt_cust, dt_seg, by = "customer_mobile")
    mt_cust[[seg]][is.na(mt_cust[[seg]])] <- 0
    
    rm(dt_seg);gc();gc();gc()
    
  }
  
  # adding division columns
  dt_div <- READ_FOLDER(pathFold = database$vmart$rfm$customer$division$offline$tagged$td$today_7, 
                        contain = "DIVISION_RFM.parquet", patternFile = "parquet")
  setnames(dt_div, old = colnames(dt_div), new = tolower(colnames(dt_div)))
  dt_div <- dt_div[!duplicated(customer_mobile), ]
  dt_div$first_transaction_date <- NULL
  dt_div$last_transaction_date <- NULL
  dt_div$net_amount_sum_total <- NULL
  
  for(col in colnames(dt_div[,-1])){
    
    dt_div[[col]] <- ifelse(dt_div[[col]] > 0, 1, 0)
    
  }
  
  setnames(dt_div, old = colnames(dt_div), new = gsub(pattern = "net_amount_sum_","", colnames(dt_div)))
  setnames(dt_div, old = colnames(dt_div), new = gsub(pattern = " ","_", colnames(dt_div)))
  colnames(dt_div)[-1] <- paste0("div_", colnames(dt_div)[-1])
  dt_div[["customer_mobile"]] <- as.character(dt_div[["customer_mobile"]])
  
  mt_cust <- left_join(mt_cust, dt_div, by = "customer_mobile")
  
  rm(dt_div);gc()
  # adding department columns
  dt_dept <- READ_FOLDER(pathFold = database$vmart$cpm$customer$department$td$today_7, 
                         contain = "_CPM_TOP10.parquet", patternFile = "parquet")
  setnames(dt_dept, old = colnames(dt_dept), new = tolower(gsub("\\[|\\]|\\-", " ", colnames(dt_dept))))
  setnames(dt_dept, old = colnames(dt_dept), new = tolower(gsub(" ", "_", colnames(dt_dept))))
  setnames(dt_dept, old = colnames(dt_dept), new = gsub(pattern = "bill_qty_sum_","", colnames(dt_dept)))
  dt_dept$net_amount_sum_total <- NULL
  dt_dept <- dt_dept[!duplicated(customer_mobile), ]
  colnames(dt_dept)[-1] <- paste0("dept_", colnames(dt_dept)[-1])
  
  for(col in colnames(dt_dept[,-1])){
    
    dt_dept[[col]] <- ifelse(dt_dept[[col]] > 0, 1, 0)
    
  }
  dt_dept[["customer_mobile"]] <- as.character(dt_dept[["customer_mobile"]])
  
  mt_cust <- left_join(mt_cust, dt_dept, by = "customer_mobile")
  
  for(col in colnames(mt_cust[,-1])){
    
    if(!class(mt_cust[[col]]) %in% c("factor", "Date")){
      mt_cust[[col]][is.na(mt_cust[[col]])] <- 0
    }  
  }
  
  setnames(x = mt_cust, 
           old = c("customer_mobile", "store", "zone", "region", "test_control", "customer_type", "active_group", "communication_medium","segment","frequency"), 
           new = c("Customer_Mobile", "Store", "Zone", "Region", "Test_Control", "Customer_Type", "Active_Group", "Communication_Medium","Segment","Frequency"), 
           skip_absent = TRUE)
  rm(dt_dept);gc()
  
  # adding organization
  mt_cust$organization <- ifelse(mt_cust$Zone == "South", "unlimited", "vmart")
  
  # checking for preold condition and removing if TRUE
  if(is_include_preold == FALSE){
    mt_cust <- mt_cust[Customer_Type != "p", ]
  }
  
  dir.create(file.path(path_proj, "output", "masters_temp"), showWarnings = FALSE)
  files_to_save <- list()
  api_status <- list()
  
  # generating organization wise files
  for(org in unique(mt_cust$organization)){
    file_name_tosave <- file.path(path_proj, "output", "masters_temp", 
                                  paste0(toupper(format(Sys.Date(), "%d%b%Y")), "_",toupper(org), "_CAMPAIGN_MASTER", ".parquet"))
    arrow::write_parquet(mt_cust[organization == org,], file_name_tosave)
    files_to_save[[org]] <- file_name_tosave
  }
  
  rm(mt_cust);gc()
  # adding campaign to filter api
  # http://10.0.0.53:5443/eaddmaster?type=path&organization=unlimited&
  # path_file=D%3A%2Fdigital-transformation%2Fapis%2Fcampaign-apis%2Foutput%2Fmasters_temp%2F16MAR2023_UNLIMITED_CAMPAIGN_MASTER.parquet
  # &author=abhishek&camp_name=y23%2Fmar%2Funlimited%2Fcampaign&event=mar%202023&
  # camp_type=gtm&start_date=01MAR2023&end_date=31MAR2023
  
  for(f in names(files_to_save)){
    
    api_response <- tryCatch(list("status" = "success", "value" = httr::GET("http://10.0.0.93:5443/eaddmaster", 
                                                                            query = list("type" = "path",
                                                                                         "organization" = f,
                                                                                         "path_file" = files_to_save[[f]],
                                                                                         "author" = "abhishek",
                                                                                         "event" = paste0(tolower(month.name[month(Sys.Date())]), format(Sys.Date(), "%y")), 
                                                                                         "camp_name" = paste0("y", format(Sys.Date(), "%y"), "/", tolower( substr(month.name[month(Sys.Date())], start = 1, stop = 3)), "/", f, "/campaign"),
                                                                                         "start_date" = toupper(format(Sys.Date(), "%d%b%Y")),
                                                                                         "end_date" = toupper(format(Sys.Date(), "%d%b%Y"))))),
                             error = function(e){list("status" = "error", "value" = "error")})
    if(api_response$status == "error"){
      next
    }
    
    api_response <- content(api_response$value)
    api_status[[f]] <- data.table("campaign_id" = api_response$f_camp_id[[1]], 
                                  "campaign_name" = api_response$`campaign name`[[1]], 
                                  "is_execution" = TRUE,
                                  "is_filter" = TRUE)
    
  }
  
  api_status <- rbindlist(api_status, use.names = TRUE)  
  
  # adding it to filter api
  dt_fltr <- fread(file.path(path_fltr, "input/active_ids/active_campaign_ids.csv"))
  dt_fltr <- rbindlist(list(dt_fltr, api_status), use.names = TRUE)
  
  fwrite(dt_fltr, file.path(path_fltr, "input/active_ids/active_campaign_ids.csv"))
  
  if(file.exists(file_name_tosave)){
    
    return(list("status" = "success",
                "description" = list(
                  "filepath" = files_to_save,
                  "campaign" = api_status
                )))
    
  }
  
}

