REQUIRED_PERIODS <- function(start_period = "01JAN2019", end_period = "31DEC2019") {
  tmp <- toupper(format(
    seq.Date(as.Date(start_period, "%d%b%Y"),
             as.Date(end_period, "%d%b%Y"),
             by = 1
    ), "%d%b%Y"
  ))
  cat("number of days selected: ", length(tmp), "\n\n")
  return(paste0(tmp, collapse = "|"))
}