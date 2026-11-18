#' Date: 09JUL2020 Time: 01:30PM
#' Author: Dreawe Innovation Labs (Abhishek Agnihotri)

#' Objective: To create a script to combine visit_rfm profile(need to be edited)
#' 



#----Cleaning Enviornment----
cat("\n\n")
cat("Cleaning enviornment","\n")
rm(list=ls()) 
gc()

#----Sourcing Libraries and external files----
# loading libraries
cat("\n\n")
cat("Loading libraries","\n")
library(dreaweRTools)
library(dplyr)
library(data.table)
library(pbapply)

# sourcing external file
cat("\n\n")
cat("Sourcing external files","\n")

source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id  = "pid_19")

path_lib <- file.path(path_proj, "Auto/library")

source(file.path(path_lib,"read_folder_function(READ_FOLDER).R"))
source(file.path(path_lib,"log_lib.R"))
source(file.path(path_lib,"daily-kpi-lib.R"))

#----Path/variable Declaration----
# Declaring path of data files
path_from_where <- file.path(path_proj, "Output", "schema")
path_to_where <- file.path(path_proj, "Output", "schema", "backup")

# Declaring global variables

#---- Local Functions ----

#---- Function calls, data reading & manipulations ----

BACKUP_FILES(fromDir = path_from_where, 
             toDir = file.path(path_to_where, paste0(toupper(format(Sys.Date(), "%d%b%Y")), "_schema")), 
             patternFiles = ".json")


