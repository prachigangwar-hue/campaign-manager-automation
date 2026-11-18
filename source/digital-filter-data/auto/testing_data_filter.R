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
library(stringr)
library(duckdb)
library(dplyr)
library(plumber)

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

# file_id <- "CAMP02SEP2024FRDSP"

mt_files <- list.files(file.path(path_cmng, "output", "masters"), pattern = "*.parquet", full.names = TRUE)
active_ids <- data.table::fread(file.path(path_proj, "input", "active_ids", "active_campaign_ids.csv"))[is_filter == TRUE, "campaign_id"]
mt_files <- mt_files[grepl(pattern = paste0(active_ids$campaign_id, collapse = "|"), x = mt_files)]

# mt_data <- lapply(files, function(x){arrow::read_parquet(x)})
# names(mt_data) <- gsub(".parquet","",sapply(strsplit(basename(files),"_"), function(x){x[4]}))

build_and_execute_query <- function(mt_files, file_id, query_json, info = TRUE) {

  # Select the matching file based on the file_id
  mt_file_to_select <- mt_files[grepl(pattern = file_id, x = mt_files)]
  
  cat("Selected file: ", mt_file_to_select, "\n")
  
  # If query_json is empty, return the count of the whole file (not distinct)
  if (is.null(query_json) || query_json == "") { # @kamana : to ask what comes exactly in empty query
    con <- dbConnect(duckdb::duckdb())
    sql_query <- paste0("SELECT COUNT(Customer_Mobile) as total_count FROM '", mt_file_to_select, "'")
    result <- dbGetQuery(con, sql_query)
    dbDisconnect(con)
    cat("Total count of the file: ", result$total_count, "\n")
    return(result$total_count)
  }
  
  # Convert the JSON query string to a list
  query <- fromJSON(query_json, simplifyDataFrame = FALSE)
  
  filter_data <- query$filter_data
  
  query_list <- list()
  
  # Loop through filter_data to dynamically build SQL query
  for (i in seq_along(filter_data)) {
    conditions <- list()
    
    # Loop through each column in the current filter_data element
    for (col_name in names(filter_data[[i]])) {
      col_values <- paste0("'", filter_data[[i]][[col_name]], "'", collapse = ", ")
      conditions <- c(conditions, paste0(col_name, " IN (", col_values, ")"))
    }
    
    # Join conditions with AND to form the WHERE clause for each filter set
    query_list[[i]] <- paste0("(", paste(conditions, collapse = " AND "), ")")
    
    # cat("Generated Query for filter set", i, ":\n")
    # print(query_list[[i]])
  }
  
  # Combine all queries with OR
  final_query <- paste(query_list, collapse = " OR ")
  
  # Final SQL query string
  sql_query <- paste0("SELECT DISTINCT Customer_Mobile FROM '", mt_file_to_select, "' WHERE ", final_query)
  # cat("Final SQL Query: \n", sql_query, "\n")
  
  # Execute the query using DuckDB
  con <- dbConnect(duckdb::duckdb())
  start_time <- Sys.time()
  result <- dbGetQuery(con, sql_query)
  dbDisconnect(con)
  end_time <- Sys.time()
  
  cat("Query execution time: ", end_time - start_time, "\n")
  
  # Convert result to data.table
  result_dt <- as.data.table(result)
  return(result_dt)
}


  # cat("Query result: \n")
  # print(head(result_dt))
  
  
#   if (info == "true") {
#     return(n_distinct(result_dt$Customer_Mobile))
#   } else if (info == "false") {
#     return(result_dt)
#   } else {
#     return("Invalid value for 'info'. Please use TRUE or 'export'.")
#   }
# }


#* @get /efilter
#* 
filter_api <- function(req,res,query,file_id,to_exclude=NULL, info = TRUE){
  
  
  if (is.null(file_id) || file_id == "") {
    res$status <- 400
    return(list(error = "Campaign master_id is not provided"))
  }
  
  query <- URLdecode(query)
  cat("query after decoding", "\n")
  print(query)
  cat("decoded query",query,"\n")
  
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

  
  cat("----------------------------3","\n")
  
  print(file_id)
  cat("data dimesnsion","\n")
  
  cat("info value::::::::::::")
  print(info)
  
  result_dt <- build_and_execute_query(mt_files, file_id, query)
  print(result_dt)
  
  cat("to_excludeeee:::")
  print(to_exclude)
  
  if(!is.null(to_exclude)){
  # to_exclude <- paste0('[{"f_camp_id":"CAMP02SEP2024FRDSP","s_camp_id":"SUBCAMP04SEP2024II0XK","t_seg_id":"SEGMENT04SEP2024BQPRD"},{"f_camp_id":"CAMP02SEP2024FRDSP","s_camp_id":"SUBCAMP04SEP2024II0XK","t_seg_id":"SEGMENT04SEP2024FPKM0"}]')
  to_exclude <- fromJSON(to_exclude)
  print(to_exclude)
  to_exclude <- as.data.table(to_exclude)
  print(typeof(to_exclude))
  
  cat("Excluding Customers: ","\n")
  dt_exclude_customers <- data.table()
  for (i in 1:nrow(to_exclude)) {
    cat("iteration :",i,"\n")
    f_camp_id <- to_exclude$f_camp_id[[i]]
    cat("f_camp_id :",f_camp_id,"\n")
    s_camp_id <- to_exclude$s_camp_id[[i]]
    cat("s_camp_id :",s_camp_id,"\n")
    t_seg_id <- to_exclude$t_seg_id[[i]]
    cat("t_seg_id",t_seg_id,"\n")
    
    query_criteria <- mt_schema[[f_camp_id]]$child[[s_camp_id]]$child[[t_seg_id]]$data$criteria
    query_customers <- build_and_execute_query(mt_files, file_id = f_camp_id, query_json = query_criteria)
    dt_exclude_customers <- rbind(dt_exclude_customers,query_customers, info)
    dt_exclude_customers <- dt_exclude_customers[!duplicated(Customer_Mobile)]
  }
  
  result_dt <- result_dt[!Customer_Mobile %in% dt_exclude_customers$Customer_Mobile]
  }
  
  # return(toJSON(list(Customers=n_distinct(result_dt$Customer_Mobile),Conversion=0),auto_unbox = TRUE))
  
  if (info == TRUE) {
    return(toJSON(list(Customers = n_distinct(result_dt$Customer_Mobile), Conversion = 0), auto_unbox = TRUE))
  } else if (info == "export") {
    file_name_export <- file.path(path_cmng, "output", "exportdata", paste0(as.numeric(round(Sys.time(), 0)), "_", file_id, ".csv"))
    result_dt <- result_dt[!duplicated(Customer_Mobile)]
    fwrite(result_dt[,"Customer_Mobile"], file_name_export)
    
    cat("exporting file", file_name_export, "\n")
    
    return(include_file(file_name_export, res, content_type = "text/csv"))
  } else {
    res$status <- 400
    return(list(error = "Invalid 'info' parameter"))
  }
}

