#' Date: 20OCT2021 Time: 09:01PM
#' Author: Abhishek Agnihotri

#' Objective: Script for iterating campaign evaluation
#' 

#---- Extracting Command line arguments ----
library(httr)
library(jsonlite)

source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_05")

path_gsheet_cred <- file.path(path_proj, "credentials", "gsheet")

dt_admin_panel <- GET("http://10.0.0.53:4474/admin")

if(!is.null(dt_admin_panel)){
  
  cat("------")
  
  dt_admin_panel_camp <- fromJSON(toJSON(content(dt_admin_panel)[["admin_panel_campaigns"]]))
  dt_admin_panel_rule <- fromJSON(toJSON(content(dt_admin_panel)[["admin_panel_rules"]]))
  
  googlesheets4::gs4_auth(email = "pmo@vmartretail.com", cache = file.path("C:/R - Projects", "credentials", "gmail"))
  
  # writin in google sheet
  googlesheets4::sheet_write(dt_admin_panel_camp, 
                             ss = "https://docs.google.com/spreadsheets/d/1XjYHHqSI1spWmETwuk0hUGJgL92ywAM6GfCQKSngKVQ/edit?usp=sharing",
                             sheet = "camp_master")
  
  googlesheets4::sheet_write(dt_admin_panel_rule, ss = "https://docs.google.com/spreadsheets/d/1MEI0oeWz9QyA2O83CfpcVrjwOloQwWulyUrqHJmR1Po/edit?usp=sharing",sheet = "rule_master")

}



