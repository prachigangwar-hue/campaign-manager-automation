# getting project directory
source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")

# loading libraries
library(plumber)

# hosting api
pr(file.path(path_proj, "Auto/admin_panel_api.R")) %>%
  pr_run(port = 4474, host = "0.0.0.0")
