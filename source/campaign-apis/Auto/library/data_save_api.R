library(jsonlite)
library(data.table)
library(stringi)
library(arrow)

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

# add_level: sub campaign or segment or communication
# if add_level is communication then one or more multiple segment ids can be specified
# communication details

# is_executed, comm_text = NULL, comm_template_id = NULL, schedule_time = NULL
#* @parser multi
#* @post /esavedata
save_data_api <- function(add_level = NULL,
                          f_camp_id = NULL, 
                          s_camp_id = NULL, 
                          t_seg_id = NULL,
                          data =  NULL,
                          req, 
                          res){
  require(jsonlite)
  
  
  request_details <- list(formContents = Rook::Multipart$parse(req))
  cat("request details","\n")
  
  #print(request_details)
  f_camp_id <- request_details$formContents$f_camp_id
  s_camp_id <- request_details$formContents$s_camp_id
  add_level <- request_details$formContents$add_level
  t_seg_id <- request_details$formContents$t_seg_id
  data <- URLdecode(request_details$formContents$data)
  comm_medium <- request_details$formContents$comm_medium
  is_external <- ifelse(request_details$formContents$is_external == "true",TRUE,FALSE)
  

  if(is.null(request_details$formContents$is_uni)){
    
    is_unicode <- FALSE
    
  }else{
    
    is_unicode <- ifelse(request_details$formContents$is_uni == "true",TRUE,FALSE)
    
  }
  
  if(!is.null(comm_medium)){
    if(comm_medium == "w"){
      
      articles_files <- c()
      for(f in names(request_details$formContents)[grepl("comm_articles",names(request_details$formContents))]){
        
        
        tmpfile <- request_details$formContents[[f]]$tempfile
        cat("processing: ",tmpfile,"\n")
        
        
        ## Copy the file to a new folder, with its original name
        fn <- file.path(path_proj, 'Output/temp', paste0(f_camp_id,"_",s_camp_id,"_",t_seg_id,"_",toupper(format(Sys.Date(),"%d%b%Y")),request_details$formContents[[f]]$filename))
        articles_files <- c(articles_files, fn)
        file.copy(tmpfile, fn)
        
        
      }
      
    }
  }
  
  # Decoding data
  
  cat("f_camp_id:", f_camp_id,"\n")
  print(is.null(f_camp_id))
  
  cat("add_level:", add_level,"\n")
  print(is.null(add_level))
  
  cat("s_camp_id:", s_camp_id,"\n")
  print(is.null(s_camp_id))
  
  cat("t_seg_id:", t_seg_id,"\n")
  print(is.null(t_seg_id))
  
  cat("data:",data,"\n")
  
  cat("is unicde",is_unicode,"\n")
  print(is.null(is_unicode))
  
  cat("is external", is_external, "\n")
  print(is_external)
  
  if(TRUE){
    
    #cat("s_camp_id:", s_camp_id,"\n")
    #cat("t_camp_id:", t_camp_id,"\n")
    display_name <- "sample"
    
    path_data <- file.path(path_proj,"Output","schema")
    files <- list.files(path_data, "*.json",full.names = TRUE)
    files <- files[grepl(f_camp_id,files)]
    cat("reading following file: ", files,"\n")
    dt <- lapply(files, function(x) jsonlite::fromJSON(jsonlite::read_json(x)[[1]]))
    names_dt <- unlist(lapply(dt, function(x){ names(x)}))
    dt <- lapply(dt, function(x){ x[[1]]})
    names(dt) <- names_dt
    
    
    if(add_level == "subcampaign"){
      
      cat("adding new subcampaign : segment id na","\n")
      
      subcamp_id <- ifelse(is.null(s_camp_id), toupper(paste0("SUBCAMP",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5))), s_camp_id)
      
      display_name <-  toupper(format(as.Date(URLdecode(jsonlite::fromJSON(data)[["metadata"]][["display_name"]]),"%Y-%m-%d"),"%d%b%Y"))
      
      if(is.null(s_camp_id)){
        cat("subcampaign: ", subcamp_id,"\n")
        dt[[f_camp_id]][["child"]][[subcamp_id]][["metadata"]][["id_global"]] <- subcamp_id
        dt[[f_camp_id]][["child"]][[subcamp_id]][["metadata"]][["display_name"]] <- display_name
        dt[[f_camp_id]][["child"]][[subcamp_id]][["data"]] <- list()
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]] <- list()
        
        
      }
      
      response <- list(campaign_id = f_camp_id,
                       sub_campaign_id = ifelse(is.null(s_camp_id),subcamp_id,s_camp_id),
                       display_name = display_name
      )
      
      
    }
    
    
    if(add_level == "segment"){
      
      subcamp_id <- s_camp_id
      segment_id <- ifelse(is.null(t_seg_id), toupper(paste0("SEGMENT",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5))), t_seg_id)
      
      if(is.null(t_seg_id)){
        cat("adding new segment: segment id na","\n")
        cat("subcampaign: ", subcamp_id,"\n")
        cat("segment: ", segment_id,"\n")
        
        print(jsonlite::fromJSON(data))
        display_name <-  URLdecode(jsonlite::fromJSON(data)[["metadata"]][["display_name"]])
        
        # decoding data
        
        data <- jsonlite::fromJSON(data)
        data[["metadata"]][["display_name"]] <- URLdecode(data[["metadata"]][["display_name"]])
        data[["data"]][["criteria"]] <- URLdecode(data[["data"]][["criteria"]])
        
        
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]] <- data
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- FALSE
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- FALSE
        
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["id_global"]] <- segment_id
        
        
      }else{
        cat("adding new subcampaign segment: segment id ","\n")
        cat("subcampaign: ", subcamp_id,"\n")
        cat("segment: ", segment_id,"\n")
        
        print(jsonlite::fromJSON(data))
        display_name <-  URLdecode(jsonlite::fromJSON(data)[["metadata"]][["display_name"]])
        
        # decoding data
        
        data <- jsonlite::fromJSON(data)
        data[["metadata"]][["display_name"]] <- URLdecode(data[["metadata"]][["display_name"]])
        data[["data"]][["criteria"]] <- URLdecode(data[["data"]][["criteria"]])
        
        
        
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]] <- data
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- FALSE
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- FALSE
        
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["id_global"]] <- segment_id
        
      }
      
      response <- list(campaign_id = f_camp_id,
                       sub_campaign_id = ifelse(is.null(s_camp_id),subcamp_id,s_camp_id),
                       segment_id = segment_id,
                       display_name = display_name
      )

    }
    
    
    if(add_level == "communication"){
      # For sending communication
      
      
      # For SMS & Whatsapp
      if(TRUE){
        
        cat("adding communication details","\n")
        
        subcamp_id <- s_camp_id
        segment_id <- ifelse(is.null(t_seg_id), toupper(paste0("SEGMENT",toupper(format(Sys.Date(),"%d%b%Y")),stri_rand_strings(n = 1,length = 5))), t_seg_id)
        
        cat("parsing json","\n")
        to_add <- jsonlite::fromJSON(data,simplifyDataFrame = FALSE)
        to_add <- simplify2array(to_add)
        
        if(is_unicode == FALSE){
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["message"]] <- URLdecode(to_add[["message"]])
        
        }else{
          
          temp_msg <- URLdecode(to_add[["message"]])
          Encoding(temp_msg) <- "UTF-8"
          to_add[["message"]] <- temp_msg
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["message"]] <- to_add[["message"]]

          
        }
          
        if(comm_medium == "s"){
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["id_template"]] <- URLdecode(to_add[["template_id"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["vendor_sms"]] <- URLdecode(to_add[["vendor_sms"]])
          
        }
        # Scheduler details
        
        if(URLdecode(to_add[["scheduler_details"]][["schedule_time"]]) == "instant"){
          cat("communication time: ","instant","\n")
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["schedule_time"]] <- URLdecode(to_add[["scheduler_details"]][["schedule_time"]])
          
        }
        
        if(URLdecode(to_add[["scheduler_details"]][["schedule_time"]]) == "once"){
          cat("communication time: ","once","\n")
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["schedule_time"]] <- URLdecode(to_add[["scheduler_details"]][["schedule_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_date"]] <- URLdecode(to_add[["scheduler_details"]][["start_date"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_time"]] <- URLdecode(to_add[["scheduler_details"]][["start_time"]])
        }
        
        if(URLdecode(to_add[["scheduler_details"]][["schedule_time"]]) == "daily"){
          cat("communication time: ","daily","\n")
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["schedule_time"]] <- URLdecode(to_add[["scheduler_details"]][["schedule_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_date"]] <- URLdecode(to_add[["scheduler_details"]][["start_date"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_time"]] <- URLdecode(to_add[["scheduler_details"]][["start_time"]])
        }
        
        if(URLdecode(to_add[["scheduler_details"]][["schedule_time"]]) == "weekly"){
          cat("communication time: ","weekly","\n")
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_external"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["schedule_time"]] <- URLdecode(to_add[["scheduler_details"]][["schedule_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_date"]] <- URLdecode(to_add[["scheduler_details"]][["start_date"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_time"]] <- URLdecode(to_add[["scheduler_details"]][["start_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["day"]] <- URLdecode(to_add[["scheduler_details"]][["day"]])
        }
        
        if(URLdecode(to_add[["scheduler_details"]][["schedule_time"]]) == "monthly"){
          cat("communication time: ","monthly","\n")
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["metadata"]][["is_executed"]] <- is_external
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["schedule_time"]] <- URLdecode(to_add[["scheduler_details"]][["schedule_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_date"]] <- URLdecode(to_add[["scheduler_details"]][["start_date"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["start_time"]] <- URLdecode(to_add[["scheduler_details"]][["start_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["end_date"]] <- URLdecode(to_add[["scheduler_details"]][["end_date"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["end_time"]] <- URLdecode(to_add[["scheduler_details"]][["end_time"]])
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["days"]] <- as.vector(sapply(to_add[["scheduler_details"]][["days"]], function(x){URLdecode(x)}))
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["scheduler_details"]][["months"]] <- as.vector(sapply(to_add[["scheduler_details"]][["months"]], function(x){URLdecode(x)}))
          
        }
        
        if(comm_medium == "w"){
          
          dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["articles"]] <- articles_files
          
        }
        
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["communication_medium"]] <- comm_medium
        dt[[f_camp_id]][["child"]][[subcamp_id]][["child"]][[segment_id]][["data"]][["is_unicode"]] <- is_unicode
        
      }

      response <- list(campaign_id = f_camp_id,
                       sub_campaign_id = ifelse(is.null(s_camp_id),subcamp_id,s_camp_id),
                       segment_id = segment_id,
                       display_name = display_name
      )
      
      
    }
    
    cat("saving json","\n")
    cat("name: ",files,"\n")
    jsonlite::write_json(toJSON(dt), files)
    
    return(response)
  }
}

