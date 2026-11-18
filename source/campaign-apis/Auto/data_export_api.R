#' Date: 25AUG2021 Time: 05:30PM
#' Author: Abhishek Agnihotri

#' Objective: API forrequesting data to be displayed
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



# If the URL gets called the browser will automatically download the file.
#' @get /exportdata
rest_rds = function(req,res) {
  
  #tfile = tempfile(fileext = "csv")
  #write.csv(iris, file = tfile)
  tfile = file.path(path_proj, "output", "CampaignPrepration", "SMS", "non_saral_oth_west_sms", "11JUN2022_1654927314_CAMP08JUN2022K02WW_SUBCAMP10JUN2022HASJA_SEGMENT10JUN2022K9Q30.csv")
  #read.csv(tfile)
  #readBin(tfile, "raw", n = file.info(tfile)$size)
  include_file(tfile, res, "text/csv")
  
}



