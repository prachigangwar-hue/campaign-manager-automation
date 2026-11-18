#' Function to update log for a given date
#' By default it is today i.e System Date


LOG_INFO <- function(what = NULL, where = NULL, when = NULL, file_name_sub_script = NULL, header_info = NULL, special_operation = NULL){
  
  if(is.null(where)){
    where <- "D:/digital-transformation/bot/daily-bot/logs"
  }
  if(is.null(when)){
    when <- toupper(format(Sys.Date(),"%d%b%Y"))
  }
  if(is.null(file_name_sub_script)){
    file_name_sub_script = "LOG"
  }
  if(is.null(header_info)){
    header_info = ""
  }
  
  file_name <- paste0(when,"_",file_name_sub_script,".ptr")
  if(file.exists(file.path(where,file_name))){}else{
    file.create(file.path(where,file_name),showWarnings = FALSE)
  }
  
  
  if(is.null(special_operation)){
    cat(paste0(Sys.time()," ", what,"\n"), file = file.path(where,file_name), append = TRUE)
  }else{
    cat(paste0(Sys.time()," ",special_operation," | ", what,"\n"), file = file.path(where,file_name), append = TRUE)
  }

  
}


# Function to check if file for a complete period is available in a given folder or not?

CHECK_PERIODS <- function(path_fold = NULL, period_start_date = NULL, period_end_date = NULL,single_file = FALSE){
  
  if(is.null(path_fold)){
    path_fold <- getwd()
  }
  if(is.null(period_start_date)){
    period_start_date <- "01APR2017"
  }
  if(is.null(period_end_date)){
    period_end_date <- toupper(format(Sys.Date(),"%d%b%Y"))
  }
  
  # Date sequence between start & end date
  date_seq <- toupper(format(
                seq.Date(as.Date(period_start_date,"%d%b%Y"),
                       as.Date(period_end_date,"%d%b%Y"),
                       by = 1
                       ),
                "%d%b%Y"))
  # Looking for files inside the given folder
  # File naming convention:
  # 01MAR2020-02MAR2020_<___>.csv
  
  files <- list.files(path_fold)
  if(single_file == FALSE){
    available_periods <- unique(substr(files,start = 1, stop = 9))
  }else{
    available_periods <- toupper(format(
      seq.Date(as.Date(period_start_date,"%d%b%Y"),
               as.Date(substr(files,start = 11, stop = 19),"%d%b%Y"),
               by = 1
      ),
      "%d%b%Y"))
  }
  
  # Filtering for which dates data is not available
  not_available_periods <- setdiff(date_seq,available_periods)
  
  return(not_available_periods)
}

