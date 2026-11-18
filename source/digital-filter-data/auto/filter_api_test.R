library(jsonlite)
library(data.table)
library(duckdb)


dt_json <- read_json("D:/products/digital_filter_data/analysis/fetch_payload.json")
x <- toJSON(dt_json)
x <- URLdecode(x)
x <- fromJSON(x,simplifyDataFrame = FALSE)

length(names(x$filter_data[[1]]))
filter_data<-x$filter_data


# Sample filter_data (list of lists, your provided structure)
# filter_data <- list(
#   list(Test_Control = matrix("t", nrow=1),
#        Store = matrix(c("COOCH BEHAR", "BURDWAN", "JALPAIGURI"), ncol=1),
#        Customer_Type = matrix("a", nrow=1),
#        Frequency = matrix(">4", nrow=1),
#        abv_bucket = matrix(c("(500,1000]", "(1000,1500]", "[0,500]"), ncol=1)),
#   
#   list(Test_Control = matrix("t", nrow=1),
#        Store = matrix(c("COOCH BEHAR", "BURDWAN", "JALPAIGURI"), ncol=1),
#        Customer_Type = matrix("a", nrow=1),
#        Frequency = matrix("1", nrow=1),
#        abv_bucket = matrix(c("(500,1000]", "[0,500]", "(1500,2000]"), ncol=1))
#   # more data can be appended here
# )

# Initialize query components
query_list <- list()

# Loop through filter_data to dynamically build SQL query
for (i in seq_along(filter_data)) {
  conditions <- list()
  
  # Get the names of available columns in the current filter_data element
  available_cols <- names(filter_data[[i]])
  
  # Dynamically add filters based on available columns in the data
  if ("Store" %in% available_cols) {
    store_values <- paste0("'", filter_data[[i]]$Store[, 1], "'", collapse = ", ")
    conditions <- c(conditions, paste0("Store IN (", store_values, ")"))
  }
  
  if ("abv_bucket" %in% available_cols) {
    abv_values <- paste0("'", filter_data[[i]]$abv_bucket[, 1], "'", collapse = ", ")
    conditions <- c(conditions, paste0("abv_bucket IN (", abv_values, ")"))
  }
  
  # Here you can add similar dynamic conditions based on other fields in filter_data
  # Example for dynamic "Frequency"
  if ("Frequency" %in% available_cols) {
    freq_value <- filter_data[[i]]$Frequency[, 1]
    conditions <- c(conditions, paste0("Frequency = '", freq_value, "'"))
  }
  
  # Example: Add Customer_Type if available
  if ("Customer_Type" %in% available_cols) {
    customer_type <- filter_data[[i]]$Customer_Type[, 1]
    conditions <- c(conditions, paste0("Customer_Type = '", customer_type, "'"))
  }
  
  
  # Example: Add Customer_Type if available
  if ("Test_Control" %in% available_cols) {
    Test_Control <- filter_data[[i]]$Test_Control[, 1]
    conditions <- c(conditions, paste0("Test_Control = '", Test_Control, "'"))
  }
  
  
  # Join conditions with AND to form the WHERE clause
  query_list[[i]] <- paste0("(", paste(conditions, collapse = " AND "), ")")
}

# Combine all queries with OR
final_query <- paste(query_list, collapse = " OR ")

# Final SQL query string
sql_query <- paste0("SELECT DISTINCT Customer_Mobile FROM 'D:/digital-transformation/apis/campaign-apis/Output/masters/02SEP2024_campaign_master_CAMP02SEP2024FRDSP.parquet' WHERE ", final_query)
cat(sql_query)

library(duckdb)
con <- dbConnect(duckdb::duckdb())
start_time <- Sys.time()
result <- dbGetQuery(con, sql_query) 
dbDisconnect(con)
end_time <- Sys.time()
print(end_time-start_time)
result_dt <- as.data.table(result)
print(result_dt)


#############################
# Initialize query components
query_list <- list()

# Loop through filter_data to dynamically build SQL query
for (i in seq_along(filter_data)) {
  conditions <- list()
  
  # Dynamically loop through each column in the current filter_data element
  for (col_name in names(filter_data[[i]])) {
    col_values <- paste0("'", filter_data[[i]][[col_name]][, 1], "'", collapse = ", ")
    conditions <- c(conditions, paste0(col_name, " IN (", col_values, ")"))
  }
  
  # Join conditions with AND to form the WHERE clause for each filter set
  query_list[[i]] <- paste0("(", paste(conditions, collapse = " AND "), ")")
}

# Combine all queries with OR
final_query <- paste(query_list, collapse = " OR ")# Final SQL query string
sql_query <- paste0("SELECT count(DISTINCT Customer_Mobile) FROM 'D:/digital-transformation/apis/campaign-apis/Output/masters/02SEP2024_campaign_master_CAMP02SEP2024FRDSP.parquet' WHERE ", final_query)
cat(sql_query)

library(duckdb)
con <- dbConnect(duckdb::duckdb())
start_time <- Sys.time()
result <- dbGetQuery(con, sql_query) 
dbDisconnect(con)
end_time <- Sys.time()
print(end_time-start_time)
result_dt <- as.data.table(result)
print(result_dt)

