library(jsonlite)
library(data.table)
library(stringi)
library(arrow)
library(dplyr)

path_proj <- getwd()
file_id <- toupper(paste0("CAMP",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5)))
org <- "vmart"
mstr_name <- "y22/may/vmart/tshjamai_shashti_savitri_amavasya_rajjo_sankranti"
dt_data <- arrow::read_parquet("D:/digital-transformation/apis/campaign-apis/Output/masters/19MAY2022_campaign_master_CAMP19MAY2022SVDKR.parquet")
file_name <- "19MAY2022_campaign_master_CAMP19MAY2022SVDKR.parquet"

head(dt_data)
columns <- colnames(dt_data)
columns <- columns[columns != "Customer_Mobile"]
length(columns)
schema_list <- list()
#jsonlite::toJSON(dt_data)

schema_list[["metadata"]][["version"]] <- "0.1"
schema_list[["metadata"]][["channel"]] <- "offline"
schema_list[["metadata"]][["date_creation"]] <- Sys.time()
schema_list[["metadata"]][["author_ip"]] <- gsub(".*? ([[:digit:]])", "\\1", system("ipconfig", intern=T)[grep("IPv4", system("ipconfig", intern = T))])
schema_list[["metadata"]][["author"]] <- "abhishek_dt"
schema_list[["metadata"]][["source_file"]] <- file_name
schema_list[["metadata"]][["source_folder"]] <- "D:\\digital-transformation\\apis\\campaign-apis\\Output\\masters\\"#file.path(path_proj,"Output","masters")
set.seed(as.numeric(Sys.time()))

# For campaign id
schema_list[["metadata"]][["id_global"]] <- file_id

#schema_list[["metadata"]][["id_global"]] <- toupper(paste0("CAMP",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5)))
schema_list[["metadata"]][["display_name"]] <- mstr_name
schema_list[["metadata"]][["organization"]] <- org

# Logic to generate schema for columns
for(cn in columns){
  cat("processing column ",cn,"\n")
  if(class(dt_data[[cn]]) == "character" | class(dt_data[[cn]]) == "factor" | cn == "birth_month" | cn == "anniversary_month" | grepl("_visit",cn)){
    
    if(n_distinct(dt_data[[cn]]) > 500){
      
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

schema_list <- list(schema_list)
names(schema_list) <- file_id

write_json(toJSON(schema_list),
           paste0("Output/schema/",toupper(format(Sys.Date(),"%d%b%Y")),"_new_schema_campaign_master_",schema_list[[file_id]][["metadata"]][["id_global"]],".json"))







