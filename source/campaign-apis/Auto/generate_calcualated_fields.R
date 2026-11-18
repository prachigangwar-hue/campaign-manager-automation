library(jsonlite)

source("C:/R - Projects/library.R")
path_proj <- GET_PROJECT_DIR(proj_id = "pid_19")

mt_json <- fromJSON(read_json(file.path(path_proj, "Output/schema/11JUN2022_new_schema_campaign_master_CAMP11JUN2022ZAEKY.json"))[[1]])

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_calculated_mrp"]] <-
  list(
    "values" = c(0, 1000000),
    "class" = "numeric",
    "formula" =
      list(
        "fields" =
          list(
            "net_amount_sum",
            "promo_amount_sum",
            "discount_amount_sum"
          ),
        "operations" =
          list(
            "add",
            "add"
          )
      )
  )

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_promo_discount"]] <-
  list(
    "values" = c(0, 1000000),
    "class" = "numeric",
    "formula" =
      list(
        "fields" =
          list(
            "promo_amount_sum",
            "discount_amount_sum"
          ),
        "operations" =
          list(
            "add"
          )
      )
  )

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_promo_discount_percent"]] <-
  list(
    "values" = c(0, 100),
    "class" = "numeric",
    "formula" =
      list(
        "fields" =
          list(
            "cf_promo_discount",
            "cf_calculated_mrp"
          ),
        "operations" =
          list(
            "divide"
          )
      )
  )

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_average_customer_value"]] <-
  list(
    "values" = c(0, 1000000),
    "class" = "numeric",
    "formula" =
      list(
        "fields" =
          list(
            "net_amount_sum"
          ),
        "operations" =
          list(
            "sum"
          )
      )
  )

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_average_bill_value"]] <-
  list(
    "values" = c(0, 100000),
    "class" = "numeric",
    "formula" =
      list(
        "fields" =
          list(
            "net_amount_sum",
            "bill_no"
          ),
        "operations" =
          list(
            "divide"
          )
      )
  )

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_unique_division_count"]] <-
  list(
    "values" = c(1:15),
    "class" = "factor",
    "formula" =
      list(
        "fields" =
          list(
            "division"
          ),
        "operations" =
          list(
            "unique_count"
          )
      )
  )

mt_json$CAMP11JUN2022ZAEKY$schema[["cf_recency"]] <-
  list(
    "values" = c(1, 5000),
    "class" = "numeric",
    "formula" =
      list(
        "fields" =
          list(
            "<sys_date>",
            "last_transaction_date"
          ),
        "operations" =
          list(
            "subtract"
          )
      )
  )


write_json(toJSON(mt_json), file.path(path_proj, "Output/schema/11JUN2022_new_schema_campaign_master_CAMP11JUN2022ZAEKY.json"))
