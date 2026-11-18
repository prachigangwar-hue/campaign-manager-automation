#' Date: 20OCT2021 Time: 09:01PM
#' Author: Abhishek Agnihotri

#' Objective: Script for iterating campaign evaluation
#' 

source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")

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


#* @get admin
#* 
admin_panel_refresh <- function(res, req){
  
  #---- Extracting Command line arguments ----
  
  #----Cleaning Environment----
  cat("\n\n")
  cat("Cleaning enviornment","\n")
  #rm(list=ls()) 
  gc()
  
  #----Sourcing Libraries and external files----
  # loading libraries
  cat("\n\n")
  cat("Loading libraries","\n")
  library(dreaweRTools)
  library(dplyr)
  library(data.table)
  library(pbapply)
  library(tidyr)
  library(jsonlite)
  
  # sourcing external file
  cat("\n\n")
  cat("Sourcing external files","\n")
  
  path_lib <- file.path(path_proj, "Auto/libraries")
  
  #----Path/variable Declaration----
  
  #---- Local functions ----
  
  #---- Function Calls, Data Reading & Manipulations ----
  
  # creating a data frame with all the columns
  
  cat("reading campaign masters","\n")
  path_schema <- file.path(path_proj, "Output", "schema")
  files <- list.files(path_schema,"*.json",full.names = TRUE)
  mt_schema <- lapply(files, function(x){fromJSON(read_json(x)[[1]])[[1]]})
  names(mt_schema) <- gsub(".csv|.json","",sapply(strsplit(basename(files),"_"), function(x){x[6]}))
  
  campaigns <- names(mt_schema)
  mt_campaigns <- list()
  
  for(f_camp_id in campaigns){
    
    subcampaign_id <- names(mt_schema[[f_camp_id]][["child"]])
    subcampaign_id <- subcampaign_id[subcampaign_id!="null"]
    
    value <- unlist(lapply(subcampaign_id, 
                           function(x){
                             return(mt_schema[[f_camp_id]][["child"]][[x]][["metadata"]][["display_name"]])
                           }))
    
    if(!is.null(value)){
      subcampaign_df <- 
        data.frame(
          
          subcampaign_id = subcampaign_id,
          subcampaign_name = value
          
        )
      subcampaign_df[["campaign_name"]] <- unlist(lapply(f_camp_id, 
                                                         function(x){
                                                           return(mt_schema[[f_camp_id]][["metadata"]][["display_name"]])
                                                         }))
      
      mt_campaigns[[f_camp_id]] <- subcampaign_df
    }
    
  }
  mt_campaigns <- rbindlist(mt_campaigns, use.names = TRUE, idcol = "campaign_id")
  campaigns <- names(mt_schema)
  mt_subcampaigns <- list()
  
  for(r in 1:nrow(mt_campaigns)){
    
    value <- unlist(lapply(mt_campaigns$subcampaign_id[r], 
                           function(x){
                             return(names(mt_schema[[mt_campaigns$campaign_id[r]]][["child"]][[x]][["child"]]))
                           }))
    if(!is.null(value)){
      segment_df <- 
        data.frame(
          
          campaign_id = mt_campaigns$campaign_id[r],
          subcampaign_id = mt_campaigns$subcampaign_id[r],
          subcampaign_name = mt_campaigns$subcampaign_name[r],
          segment_id = value
          
        )
      
      segment_df$campaign_name <- mt_campaigns$campaign_name[r]
      mt_subcampaigns[[mt_campaigns$subcampaign_id[r]]]  <- segment_df  
      
    }
    
  }
  mt_campaigns <- rbindlist(mt_subcampaigns, use.names = TRUE)
  
  # adding display name
  for(r in 1:nrow(mt_campaigns)){
    
    value <- mt_schema[[mt_campaigns$campaign_id[r]]][["child"]][[mt_campaigns$subcampaign_id[r]]][["child"]][[mt_campaigns$segment_id[r]]][["metadata"]][["display_name"]]
    value <- ifelse(is.null(value),"",value)
    mt_campaigns[["filter_name"]][r] <- value
    
  }
  
  mt_campaigns <- mt_campaigns[,c("campaign_id","subcampaign_id","segment_id","campaign_name","subcampaign_name","filter_name")]
  
  #return(list("admin_panel_rules" = mt_campaigns))
  
  
  # reading admin panel files
  dt_ap_rule <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1MEI0oeWz9QyA2O83CfpcVrjwOloQwWulyUrqHJmR1Po/edit?usp=sharing")
  dt_ap_camp <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1XjYHHqSI1spWmETwuk0hUGJgL92ywAM6GfCQKSngKVQ/edit?usp=sharing")

  mt_campaigns <- mt_campaigns[!campaign_id %in% "CAMP16SEP2021BGAHO",]

  setDT(dt_ap_camp);setDT(dt_ap_rule)

  # for rules
  # joining and updating admin panel
  # rows to add in google sheet
  dt_ap_rule_add <- mt_campaigns[!mt_campaigns$segment_id %in% dt_ap_rule$segment_id,]

  # finding rules to refresh count
  dt_ap_rule_refresh <- dt_ap_rule[refresh_count == TRUE,]

  dt_ap_rule_refresh <- rbindlist(list(dt_ap_rule_add[,c("campaign_id","subcampaign_id","segment_id","campaign_name","subcampaign_name","filter_name")],
                                       dt_ap_rule_refresh[,c("campaign_id","subcampaign_id","segment_id","campaign_name","subcampaign_name","filter_name")]),use.names = TRUE)



  # calling filter_api to return customer count for newly added rules
  if(nrow(dt_ap_rule_refresh) > 0){

    for(r in 1:nrow(dt_ap_rule_refresh)){
    cat("fetching customers for rule",dt_ap_rule_refresh$segment_id[r],"\n")

    f_camp_id = dt_ap_rule_refresh$campaign_id[r]; s_camp_id = dt_ap_rule_refresh$subcampaign_id[r]; t_seg_id = dt_ap_rule_refresh$segment_id[r]
    query <- mt_schema[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[t_seg_id]][["data"]][["criteria"]]
    to_exclude <- fromJSON(toJSON(mt_schema[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[t_seg_id]][["data"]][["to_exclude"]]), simplifyDataFrame = FALSE)
    file_id <- f_camp_id

    # api_call
    library(httr)
    api_url <- paste0("http://10.0.0.53:4434/efilter?", "query=",
                      URLencode(query,reserved = TRUE),
                      "&to_exclude=",
                      ifelse(length(to_exclude) != 0,URLencode(toJSON(to_exclude),reserved = TRUE),"%5B%5D"),
                      "&file_id=",
                      URLencode(file_id,reserved = TRUE))
    api_response <- GET(url = api_url,timeout = 500)
    if(!is.null(names(content(api_response)))){next}
    
    api_response <- content(api_response)
    
    
    api_response <- fromJSON(api_response[[1]])

    dt_ap_rule_refresh$customer_count[r] <- api_response[["data"]]


    rm(f_camp_id);rm(s_camp_id);rm(t_seg_id);rm(api_url);rm(api_response);rm(query);rm(to_exclude);rm(file_id)

    }

  }
  # appending rows to update in the admin panel rule sheet
  dt_ap_rule <- rbindlist(list(dt_ap_rule, dt_ap_rule_refresh), use.names = TRUE,fill = TRUE)

  if(nrow(dt_ap_rule_refresh) > 0){

    for(r in 1:nrow(dt_ap_rule_refresh)){

      dt_ap_rule[segment_id == dt_ap_rule_refresh$segment_id[r],"customer_count"] <- dt_ap_rule_refresh$customer_count[r]
      dt_ap_rule[segment_id == dt_ap_rule_refresh$segment_id[r],"refresh_count"] <- FALSE

    }

  }

  # for campaigns
  dt_ap_camp_add <- mt_campaigns[!mt_campaigns$campaign_id %in% dt_ap_camp$campaign_id,]
  dt_ap_camp_add <- unique(dt_ap_camp_add[,c("campaign_id","campaign_name")])

  # appending rows to update in the admin panel rule sheet
  dt_ap_camp <- rbindlist(list(dt_ap_camp, dt_ap_camp_add), use.names = TRUE,fill = TRUE)

  return(list("admin_panel_rules" = dt_ap_rule, "admin_panel_campaigns" = dt_ap_camp))
  
    
}  





