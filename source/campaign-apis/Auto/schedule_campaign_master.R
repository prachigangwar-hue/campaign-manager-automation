# sourcing project paths
source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")
path_lib <- file.path(path_proj, "auto/library")

# loading libraries
library(config)
library(data.table)
library(arrow)
library(dplyr)
library(tidyr)
library(dreaweRTools)
library(googlesheets4)
library(jsonlite)
library(httr)

# declaring which segments are needed
# sourcing config file
database <- config::get(file = file.path(path_proj, "config.yml"))
database <- database$database

# reading file
googlesheets4::gs4_auth(path = file.path("C:/R - Projects/credentials/gsheet/digital/gmail_credentials.json"))

dt_segments <- read_sheet(ss = database$gsheets$segment$url, sheet = database$gsheets$segment$sheet);setDT(dt_segments)
dt_segments <- dt_segments[is_include_master == TRUE, ]
dt_segments <- toJSON(dt_segments, dataframe = "rows")

#segments <- jsonlite::fromJSON('{"family":[true],"discount_seeker":[false],"fmcg":[true],"footmart":[true],"high_value_one_timer":[true]}')

# calling campaign master api to generate campaign master
api_response <- httr::GET("http://10.0.0.93:5441/egenmaster", query = list("dt_segments" = dt_segments, "is_include_preold" = TRUE))