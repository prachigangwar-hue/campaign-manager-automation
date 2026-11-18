#' Function to update log for a given date
#' By default it is today i.e System Date


mob_check <- function (data, var = "CUSTOMER_MOBILE", replace = FALSE, 
    stopNumbers = c(9999999999, 9999999990, 9999999993)) 
{
    if (var %in% colnames(data)) {
        if (replace == F) {
            data <- data[grepl("^[6-9]\\d{9}$", data[[var]]), 
                ]
            data <- data[!(data[[var]] %in% stopNumbers), ]
            print("Sucees!! Deleted non Indian or fraud numbers")
        }
        if (replace == T) {
            data[!(grepl("^[6-9]\\d{9}$", data[[var]])), 
                var] <- NA
            data[(data[[var]] %in% stopNumbers), var] <- NA
            print("Sucess!! Replaced non Indian or fraud numbers with NA")
        }
    }
    else {
        warning("Column not found in data")
		 }
    return(data)
}
		
		