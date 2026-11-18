#' Date: 12AUG2021 Time: 04:30PM
#' Author: Abhishek Agnihotri

#' Objective: API for filtering data
#' 

source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_42")
path_cmng <- GET_PROJECT_DIR(proj_id = "pid_19")

library(data.table)
library(jsonlite)
library(arrow)
library(googlesheets4)
library(stringr)
library(duckdb)

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

# For data
files <- list.files(file.path(path_cmng, "output", "masters"), pattern = "*.parquet", full.names = TRUE)
active_ids <- data.table::fread(file.path(path_proj, "input", "active_ids", "active_campaign_ids.csv"))[is_filter == TRUE, "campaign_id"]
files <- files[grepl(pattern = paste0(active_ids$campaign_id, collapse = "|"), x = files)]

mt_data <- lapply(files, function(x){arrow::read_parquet(x)})
names(mt_data) <- gsub(".parquet","",sapply(strsplit(basename(files),"_"), function(x){x[4]}))

mt_data <- lapply(mt_data, setDT)
rm(active_ids)
rm(files)

# data.table::fread(file.path(path_proj, "Output", "input_file.csv"))

#* @get /efilter
#* 
filter_api <- function(req,res,query,file_id,to_exclude = NULL, info = TRUE){
  
  cat("query recieved", "\n")
  print(query)
  cat("loading schema","\n")
  
  # For schema
  path_schema <- file.path(path_cmng, "output/schema")
  
  files <- list.files(path_schema,"*.json",full.names = TRUE)
  mt_schema <- lapply(files, function(x){fromJSON(read_json(x)[[1]])[[1]]})
  names(mt_schema) <- gsub(".csv|.json","",sapply(strsplit(basename(files),"_"), function(x){x[6]}))
  
  cat("----------------------------1","\n")
  
  cat("received data from frontend","\n")
  cat("query: ",query,"\n")
  cat("file_id: ",file_id,"\n")
  cat("to_exclude: ",to_exclude,"\n")
  
  cat("----------------------------1","\n")
  
  cat("----------------------------2","\n")
  
  query <- URLdecode(query)
  cat("query after decoding", "\n")
  print(query)
  cat("decoded query",query,"\n")
  
  if(!is.null(to_exclude)){
    
    to_exclude <- URLdecode(to_exclude)
    cat("decoded to_exclude",to_exclude,"\n")
    
  }
  
  cat("----------------------------2","\n")
  #  file_id = "CAMP09SEP20214M4BJ"
  #path_data <- "D:\\digital-transformation\\apis\\filter-api\\Output\\input_file.csv"
  
  #dt_data <- data.table::fread(path_data, integer64 = "double")
  #'{"result_value":["customer","sale"],"operation":[],"filter_data":{"Region":["JHK1","UP7"],"Sale_Value":"500-600","Store":["ADITYAPUR","AGRA","AGRA-SIKANDRA"]}}'
  #'{"result_value":["customer","sale"],
  #'   "operation":[],
  #'   "filter_data":[{"Region":["JHK1","UP7"],"Sale_Value":"500-600","Store":["ADITYAPUR","AGRA","AGRA-SIKANDRA"],"kirana":["1"]}}
  
  cat("----------------------------3","\n")
  
  print(file_id)
  dt_data <- mt_data[[file_id]]
  cat("data dimesnsion","\n")
  print(dim(dt_data))
  
  dt_schema <- mt_schema[[file_id]][["schema"]]
  cat("schema dimension")
  print(str(dt_schema))
  
  # Checking for number of criteria and iterating to return customers in all criteria
  
  file_filters <- jsonlite::fromJSON(query,simplifyDataFrame = FALSE)
  #---
  
  #file_filters[[1]] <- file_filters 
  
  #---
  cat("file_filters","\n")
  print(file_filters)
  
  cat("----------------------------3","\n")
  
  cat("----------------------------4","\n")
  
  if(length(file_filters$operation) == 200){
    names_filter <- names(file_filters$filter_data)
    
    for(nf in names_filter){
      
      print(nf)
      
      if(dt_schema[[nf]]$class == "character" | dt_schema[[nf]]$class == "factor"){
        print(dt_schema[[nf]]$class)
        dt_data <- dt_data[dt_data[[nf]] %in% file_filters$filter_data[[nf]],]
        
      }
      
      if(dt_schema[[nf]]$class == "integer" | dt_schema[[nf]]$class == "double" | dt_schema[[nf]]$class == "numeric"){
        print(dt_schema[[nf]]$class)
        criteria <- as.numeric(strsplit(file_filters$filter_data[[nf]], "-")[[1]])
        print(criteria)
        dt_data <- dt_data[(dt_data[[nf]] >= criteria[1]) & (dt_data[[nf]] <= criteria[2]),]
        
      }
      
      #print(dt_data)
    }
  }else{
    
    out_list <- list()
    
    for(iter in 1:(length(file_filters$operation) + 1)){
      cat("iteration: ","\n")
      print(Sys.time())
      cat(iter,"\n")
      
      dt_data <- mt_data[[file_id]]
      
      names_filter <- names(file_filters$filter_data[[iter]])
      cat("names_filter","\n")
      print(names_filter)
      
      for(nf in names_filter){
        
        print(nf)
        
        if(dt_schema[[nf]]$class == "character" | dt_schema[[nf]]$class == "factor"){
          print(dt_schema[[nf]]$class)
          dt_data <- dt_data[dt_data[[nf]] %in% file_filters$filter_data[[iter]][[nf]],]
          
        }
        
        if(dt_schema[[nf]]$class == "integer" | dt_schema[[nf]]$class == "double" | dt_schema[[nf]]$class == "numeric"){
          print(dt_schema[[nf]]$class)
          criteria <- as.numeric(strsplit(file_filters$filter_data[[iter]][[nf]], "-")[[1]])
          print(criteria)
          dt_data <- dt_data[(dt_data[[nf]] >= criteria[1]) & (dt_data[[nf]] <= criteria[2]),]
          
        }
        
        #print(dt_data)
      }
      
      out_list[[iter]] <- list("query" = file_filters$filter_data[[iter]],
                               "metric" = "customers",
                               "value" = nrow(dt_data),
                               "data" = dt_data)
      rm(dt_data)
      
    }
    
    dt_join <- rbindlist(lapply(out_list, function(x){return(x[["data"]])}),use.names = TRUE)
    
    for(iter in 1:(length(file_filters$operation) + 1)){
      
      out_list[[iter]][["data"]] <- NULL
      
    }
    
    out_list[["metric"]] = "customers"
    out_list[["data"]] = uniqueN(dt_join$Customer_Mobile,na.rm = TRUE)
    
  }
  
  cat("----------------------------4","\n")
  
  cat("----------------------------5","\n")
  
  
  if(!is.null(to_exclude)){
    to_exclude <- jsonlite::fromJSON(to_exclude,simplifyDataFrame = FALSE)
    cat("to_exclude in json","\n")
    print(to_exclude)
    
    cat("inside to_exclude if","\n")
    
    mt_to_exclude <- list()
    cat("----------------------------5.1","\n")
    
    for(te in to_exclude){
      
      cat("inside to_exclude for","\n")
      
      f_camp_id = te[["f_camp_id"]]
      s_camp_id = te[["s_camp_id"]]
      t_seg_id = te[["t_seg_id"]]
      
      filter_details <- 
        list(
          
          criteria = 
            mt_schema[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[t_seg_id]][["data"]][["criteria"]],
          
          master =  file.path(
            mt_schema[[f_camp_id]][["metadata"]][["source_folder"]],
            mt_schema[[f_camp_id]][["metadata"]][["source_file"]]),
          
          campaign_id = 
            f_camp_id,
          
          subcamp_id = 
            s_camp_id,
          
          segment_id = 
            t_seg_id
          
        )
      
      
      cat("recursive filter details","\n")
      print(filter_details)
      
      mt_to_exclude[[paste0(filter_details$campaign_id, "_", filter_details$subcamp_id, "_", filter_details$segment_id)]] <- 
        filter_api(query = filter_details$criteria, file_id = filter_details$campaign_id, to_exclude = NULL, info = FALSE)
      
    }
    
    cat("----------------------------5.1","\n")
    
    
    mt_to_exclude <-  rbindlist(mt_to_exclude, use.names = TRUE, fill = TRUE)
    cat("number of customers in exclude dataset","\n")
    print(dim(mt_to_exclude))
    
    #query,file_id,req,res, path_data
    
    # Excluding customers from 
    dt_join <- dt_join[!dt_join[["Customer_Mobile"]] %in% mt_to_exclude[["Customer_Mobile"]],]
    
    
    out_list[["metric"]] = c("customers", "conversion")
    out_list[["data"]] = c(uniqueN(dt_join$Customer_Mobile,na.rm = TRUE), round(sample((55:99)/100, 1, TRUE), 3))
    
  }
  
  cat("----------------------------5","\n")
  
  cat("----------------------------6","\n")
  
  if(info == TRUE){
    
    response <- jsonlite::toJSON(out_list)
    
    cat("response","\n")
    print(response)
    return(response)
    
  }else if(info == "export"){
    
    file_name_export <- file.path(path_cmng, "output", "exportdata", paste0(as.numeric(round(Sys.time()),0), "_", file_id, ".csv"))
    dt_join <- dt_join[!duplicated(Customer_Mobile)]
    fwrite(dt_join[,"Customer_Mobile"], file_name_export)  
    
    cat("exporting file", file_name_export, "\n")
    
    return(include_file((file_name_export), res, content_type = "text/csv"))
    
  }else{
    
    response <- dt_join
    
    cat("response","\n")
    print(head(response))
    return(response)
    
  }
  cat("----------------------------6","\n")
  
}



