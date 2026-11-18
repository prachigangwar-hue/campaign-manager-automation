#' Date: 25AUG2021 Time: 05:30PM
#' Author: Abhishek Agnihotri

#' Objective: API forrequesting data to be displayed
#' 

source(file.path(path_proj,"Auto/library/chatbot_lib.R"))

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
#* @get /edata
#* 
request_data_api <- function(f_level = "campaign", f_camp_id = NULL, s_level = "metadata", s_camp_id = NULL, req, res){
  library(data.table)
  library(jsonlite)
  cat("f_level:", f_level,"\n")
  cat("s_level:", s_level,"\n")
  cat("f_camp_id:", f_camp_id,"\n")
  cat("s_camp_id:", s_camp_id,"\n")
  
  path_data <- file.path(path_proj,"Output","schema")
  files <- list.files(path_data, "*.json",full.names = TRUE)
  dt <- lapply(files, function(x) fromJSON(read_json(x)[[1]]))
  
  if(f_level == "campaign"){
    
    if(s_level == "metadata"){
      
      response <- lapply(1:length(dt), function(x) dt[[x]][[1]][["metadata"]]) 
      return(response)
      
    }
    
    if(s_level == "schema"){
      
      id_list <- lapply(1:length(dt), function(x) dt[[x]][[1]][["metadata"]][["id_global"]]) 
      id_index <- which(id_list == f_camp_id)
      response <- dt[[id_index]][[f_camp_id]][["schema"]]
      return(response)
      
    }
    
    if(s_level == "child"){
      
      id_list <- lapply(1:length(dt), function(x) dt[[x]][[1]][["metadata"]][["id_global"]]) 
      id_index <- which(id_list == f_camp_id)
      response <- dt[[id_index]][[f_camp_id]][["child"]]
      return(response)
      
    }
  }
  
  if(f_level == "subcampaign"){
    
    if(s_level == "metadata"){
      
      id_list <- lapply(1:length(dt), function(x) dt[[x]][[1]][["metadata"]][["id_global"]]) 
      id_index <- which(id_list == f_camp_id)
      response <- dt[[id_index]][[f_camp_id]][["child"]][[s_camp_id]][["metadata"]]
      
    }
    
    if(s_level == "child"){
      
      id_list <- lapply(1:length(dt), function(x) dt[[x]][[1]][["metadata"]][["id_global"]]) 
      id_index <- which(id_list == f_camp_id)
      response <- dt[[id_index]][[f_camp_id]][["child"]][[s_camp_id]][["child"]]
      
    }
  }
  
  if(f_level == "all"){
    
    
    path_schema <- file.path(path_proj,"Output","schema")
    files <- list.files(path_schema,"*.json",full.names = TRUE)
    mt_schema <- lapply(files, function(x){fromJSON(read_json(x)[[1]])[[1]]})
    names(mt_schema) <- gsub(".csv|.json","",sapply(strsplit(basename(files),"_"), function(x){x[6]}))
    
    campaigns <- names(mt_schema)
    mt_campaigns <- list()
    
    for(f_camp_id in campaigns){
      
      subcampaign_id <- names(mt_schema[[f_camp_id]][["child"]])
      subcampaign_id <- subcampaign_id[subcampaign_id != "null"]
      
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
    
    response <- list("admin_panel_rules" = mt_campaigns)
    
  }
  
  return(response)
  
}



#* @post /esavedata
save_data_api <- function(f_level = "campaign", 
                          s_level = "subcampaign", 
                          t_level = "segment",
                          f_camp_id = NULL, 
                          s_camp_id = NULL, 
                          t_camp_id = NULL,
                          metadata= NULL,
                          data = NULL,
                          req, 
                          res){
  require(jsonlite)
  cat("f_level:", f_level,"\n")
  cat("s_level:", s_level,"\n")
  cat("t_level:", t_level,"\n")
  cat("f_camp_id:", f_camp_id,"\n")
  cat("s_camp_id:", s_camp_id,"\n")
  cat("t_camp_id:", t_camp_id,"\n")
  
  
  path_data <- file.path(path_proj,"Output","schema")
  files <- list.files(path_data, "*.json",full.names = TRUE)
  dt <- lapply(files, function(x) fromJSON(read_json(x)[[1]]))
  names_dt <- unlist(lapply(dt, function(x){ names(x)}))
  dt <- lapply(dt, function(x){ x[[1]]})
  names(dt) <- names_dt
  
  
  if(f_level == "campaign"){
    
    if(s_level == "subcampaign"){
      
      if(t_level == "segment"){
        
        if(is.null(t_camp_id)){
          
          segment_id <- toupper(paste0("SEGMENT",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5)))
          
          dt[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[segment_id]][["metadata"]] <- toJSON(fromJSON(metadata))
          dt[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[segment_id]][["metadata"]][["id_global"]] <- segment_id
          
          dt[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[segment_id]][["data"]] <- toJSON(fromJSON(data))
          
          
        }else{
          
          dt[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[segment_id]][["metadata"]] <- toJSON(fromJSON(metadata))
          
          dt[[f_camp_id]][["child"]][[s_camp_id]][["child"]][[segment_id]][["data"]] <- toJSON(fromJSON(data))
          
        }
        
        
      }
      
    }
    
  }
  
  write_json(dt, files[grep(f_camp_id, files)])
  
}



