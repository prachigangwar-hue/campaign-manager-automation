library(plumber)

# Hosting API on port 8182

#options(plumber.apiURL = "https://apps.vmart.co.in:8443/nca1/")

#options(plumber.trailingSlash = TRUE)

path_proj <- "D:/digital-transformation/apis/filter-api/"

pr(file.path(path_proj,"Auto/filter_api.R")) %>%
  pr_run(port = 4444,host = "0.0.0.0")

#curl -H "Content-Type: application/json" --data "{\"authentication_key\":\"UEXGA1219R\",\"mobile_number\":\"7897895502\",\"date_time\":\"2021-03-19 17:26:44\",\"circle_name\":\"lucknow\",\"task_name\":\"task\",\"dtmf_press_number\":\"1\"}" http://localhost:8000/collect_data