#' Date: 18AUG2021 Time: 05:30PM
#' Author: Abhishek Agnihotri

#' Objective: API for adding master
#' 
source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")

library(jsonlite)
source(file.path(path_proj,"Auto/library/chatbot_lib.R"))
library(aws.s3)

# Set AWS credentials
Sys.setenv("AWS_ACCESS_KEY_ID" = "REDACTED_REQUIRED",
           "AWS_SECRET_ACCESS_KEY" = "REDACTED_REQUIRED",
           "AWS_DEFAULT_REGION" = "ap-south-1")

bucket_name <- "campaign-master-files" 

#' @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}




#* @get eaddmaster
#* 
add_master <- function(type = "path", organization = "vmart", path_file = NULL, upload_file = NULL, f_camp_id = NULL, 
                       author = NULL, camp_name = NULL, event = NULL, camp_type = "gtm", start_date = NULL, end_date = NULL){
  library(jsonlite)
  library(data.table)
  library(stringi)
  library(arrow)
  library(dplyr)
  
  ls_response <- list()
  cat("----------------------------1","\n")
  
  cat("received data from frontend","\n")
  cat("type: ",type,"\n")
  cat("path_file: ",path_file,"\n")
  cat("upload_file: ",upload_file,"\n")
  cat("f_camp_id: ",f_camp_id,"\n")
  cat("author: ",author,"\n")
  cat("camp_name: ",camp_name,"\n")
  cat("event: ", event, "\n")
  cat("camp_type: ", camp_type, "\n")
  cat("start_date", start_date, "\n")
  cat("end_date", end_date, "\n")
  
  cat("----------------------------1","\n")
  
  cat("----------------------------2","\n")
  
  cat("copying file to the master folder","\n")
  if(type == "path"){
    
    cat("inside path if","\n")
    
    cat("path_file extension",tools::file_ext(path_file),"\n")
    f_camp_id <- ifelse(is.null(f_camp_id), toupper(paste0("CAMP",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5))), f_camp_id)
    file_path_copy <- file.path(path_proj,"Output/masters",paste0(toupper(format(Sys.Date(),"%d%b%Y")),"_campaign_master_",f_camp_id,".",tools::file_ext(path_file)))
    
    
    ls_response[['f_camp_id']] <- f_camp_id
    ls_response[['master file location']] <- file_path_copy
    
    cat("file_path_copy: ",file_path_copy,"\n")
    file.copy(from = file.path(path_file),
              to = file.path(file_path_copy))
    
    if(tools::file_ext(file_path_copy) == "csv"){
      
      cat("added file is csv","\n")
      dt_data <- data.table::fread(file_path_copy, integer64 = "double")
      
      ls_response[['records in file']] <- nrow(dt_data)
      
    }
    
    if(tools::file_ext(file_path_copy) == "parquet"){
      
      
      cat("added file is parquet","\n")
      dt_data <- arrow::read_parquet(file_path_copy)
      
      ls_response[['records in file']] <- nrow(dt_data)
      
      # handling linux system files
      
      
    }
    
    # Construct the new file name with the appropriate prefix
    s3_file_name <- paste0(organization, "_",basename(file_path_copy))
    
    # Upload the file to AWS S3 with the new file name
    put_object(file = file_path_copy, object = s3_file_name, bucket = bucket_name)
    
    cat("File uploaded to S3 successfully as: ", s3_file_name, "\n")
    
    
  }
  
  if(type == "file"){
    
    
    
    
  }
  
  cat("----------------------------2","\n")
  
  cat("----------------------------3","\n")
  
  cat("adding metadata to schema master","\n")
  file_name <- basename(file_path_copy)
  source_folder <- dirname(file_path_copy)
  
  columns <- colnames(dt_data)
  columns <- columns[columns != "Customer_Mobile"]
  length(columns)
  schema_list <- list()
  
  ls_response[['added columns']] <- paste0(columns, collapse = "|")
  
  schema_list[["metadata"]][["version"]] <- "0.1"
  schema_list[["metadata"]][["channel"]] <- "offline"
  schema_list[["metadata"]][["date_creation"]] <- Sys.time()
  schema_list[["metadata"]][["author_ip"]] <- gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=T)[grep("IPv4", system("ipconfig", intern = T))])
  schema_list[["metadata"]][["author"]] <- author
  schema_list[["metadata"]][["source_file"]] <- file_name
  schema_list[["metadata"]][["source_folder"]] <- source_folder
  set.seed(as.numeric(Sys.time()))
  schema_list[["metadata"]][["id_global"]] <- f_camp_id
  schema_list[["metadata"]][["display_name"]] <- camp_name
  schema_list[["metadata"]][["organization"]] <- organization
  schema_list[["metadata"]][["event"]] <- event
  schema_list[["metadata"]][["type"]] <- camp_type
  schema_list[["metadata"]][["start_date"]] <- start_date
  schema_list[["metadata"]][["end_date"]] <- end_date
  
  ls_response[['organization']] <- schema_list[["metadata"]][["organization"]]
  ls_response[['campaign name']] <- schema_list[["metadata"]][["display_name"]]
  ls_response[['campaign id_global']] <- schema_list[["metadata"]][["id_global"]]
  
  
  cat("----------------------------3","\n")
  
  cat("----------------------------4","\n")
  
  cat("generating headers and their details","\n")
  for(cn in columns){
    cat("processing column ",cn,"\n")
    if(class(dt_data[[cn]]) == "character" | class(dt_data[[cn]]) == "factor" | cn == "birth_month" | cn == "anniversary_month" | grepl("_visit",cn)){
      
      if(n_distinct(dt_data[[cn]]) > 600){
        
        next
        
      }
      
      schema_list[["schema"]][[cn]][["values"]] <- as.character(unique(dt_data[[cn]]))
      schema_list[["schema"]][[cn]][["class"]] <- "character"
      next
      
    }
    if(class(dt_data[[cn]]) == "integer" | class(dt_data[[cn]]) == "double" | class(dt_data[[cn]]) == "numeric"){
      
      if((length(unique(dt_data[[cn]])) == 2 & length(setdiff(c(0,1),unique(dt_data[[cn]]))) == 0) | length(unique(dt_data[[cn]])) == 1){
        
        schema_list[["schema"]][[cn]][["values"]] <- as.character(unique(dt_data[[cn]]))
        schema_list[["schema"]][[cn]][["class"]] <- "factor"
        
      }else if(class(dt_data[[cn]]) == "integer" | class(dt_data[[cn]]) == "numeric"){
        
        schema_list[["schema"]][[cn]][["values"]] <- c(min(dt_data[[cn]]),max(dt_data[[cn]]))
        schema_list[["schema"]][[cn]][["class"]] <- "numeric"
        
      }else if(class(dt_data[[cn]]) == "double"){
        
        schema_list[["schema"]][[cn]][["values"]] <- c(min(dt_data[[cn]]),max(dt_data[[cn]]))
        schema_list[["schema"]][[cn]][["class"]] <- "numeric"
      }
    }
    
  }
  cat("----------------------------6","\n")
  
  cat("writing schema master","\n")
  
  schema_list <- list(schema_list)
  names(schema_list) <- f_camp_id
  
  write_json(toJSON(schema_list),
             file.path(path_proj, paste0("Output/schema/",toupper(format(Sys.Date(),"%d%b%Y")),"_new_schema_campaign_master_",schema_list[[f_camp_id]][["metadata"]][["id_global"]],".json")))
  
  cat("----------------------------6","\n")
  
  
  return(ls_response)
  
  
}