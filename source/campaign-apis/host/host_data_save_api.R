# getting project directory
source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")

# loading libraries
library(plumber)

# hosting api
pr(file.path(path_proj, "Auto/data_save_api.R")) %>%
  pr_run(port = 4438, host = "0.0.0.0")
