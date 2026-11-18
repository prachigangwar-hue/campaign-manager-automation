#' Date: 25AUG2021 Time: 05:30PM
#' Author: Abhishek Agnihotri

#' Objective: API for requesting data to be displayed
#' 

source(file.path(path_proj,"Auto/library/chatbot_lib.R"))
path_var_dict <- file.path(path_proj, "info", "variable_dictionary.json")

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


# f_level: campaign, subcampaign, criteria
# f_camp_id: campaign id
# s_level: metadata,child
# s_camp_id: 
#* @get /evar
#* 
request_data_api <- function(username, password, req, res){
  dict_users <- c("cmng" = "REDACTED_REQUIRED")
  
  if(password != dict_users[[username]]){
    
    response <- c("username or password is incorrect")
    
  }else{
    
    library(data.table)
    library(jsonlite)
    
    dt_json <- read_json(path_var_dict)
    response <- dt_json
    
  }
      
  return(response)
  
}






