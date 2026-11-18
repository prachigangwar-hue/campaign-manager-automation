# Date: 200120 Time: 01:00PM
# Author: Dreawe Innovation Labs(Abhishek Agnihotri)

# Objective: Create a function which takes path, pattern and cols to read

library(pbapply)

#' Function to validate uniqueness of a column
#' Input: data, variable name
#' Output: Information whether column is unique or not?
VALIDATE_UNIQ_COL <- function(data,var = "CUSTOMER_MOBILE",retType = "info",delRec = TRUE){
  if (var %in% colnames(data)) {
    if(nrow(data) == length(unique(data[[var]]))){
      cat("Go ahead !!",var," is unique across the dataframe","\n")
      cat("Number of total records: ",nrow(data),"\n")
      cat("Number of duplicate records: ",length(which(duplicated(data[[var]]))),"\n")
    }else{
      cat("Recheck !!",var," is not unique across the dataframe","\n")
      cat("Number of total records: ",nrow(data),"\n")
      cat("Number of duplicate records: ",length(which(duplicated(data[[var]]))),"\n")
       # if(readline("Want to delete duplicate records \n 1: Yes \n 2: No \n") == "Yes"){
        if(delRec == TRUE){
          data <- data[! duplicated(data[[var]]),]
          VALIDATE_UNIQ_COL(data,var)
        }
    }
  }else{
    warning("Column not found in data")
  }
  if(retType == "data"){
    return(data)
  }
}


READ_FOLDER <- function(pathFold = getwd(), patternFile = "csv",recursiveFile = FALSE,all = TRUE, contain = NULL,selCols = NULL,numRows = Inf,outObj = "data.frame",skipRows = 0){
  require(data.table)
  require(readxl)
  require(pbapply)
  if("csv" == tolower(patternFile)){
    cat("Reading csv files","\n")
    tmpListFiles <- list.files(path = pathFold,pattern = "*.csv|*.txt|*.CSV",full.names = TRUE,recursive = recursiveFile)
    cat("Number of total Files:",length(tmpListFiles),"\n")
    if(! is.null(contain)){
      tmpListFiles <- tmpListFiles[grep(pattern = contain,tmpListFiles)]
      cat("Number of selected files:",length(tmpListFiles),"\n")
      cat("Name of selected files: ",paste0(tmpListFiles,collapse ="\n"),"\n")
    }
    cat("Data reading started","\n")
	if(! is.null(selCols)){
		tmpData <- pblapply(tmpListFiles,fread,stringsAsFactors = FALSE,verbose=FALSE,integer64 ="double",select = selCols,nrows = numRows,skip = skipRows)
    }else{
		tmpData <- pblapply(tmpListFiles,fread,stringsAsFactors = FALSE,verbose=FALSE,integer64 ="double",nrows = numRows,skip = skipRows)
	}
	cat("Data reading finished","\n")
  }
  if("txt" == tolower(patternFile)){
    cat("Reading csv files","\n")
    tmpListFiles <- list.files(path = pathFold,pattern = "*.txt|*.TXT",full.names = TRUE,recursive = recursiveFile)
    cat("Number of total Files:",length(tmpListFiles),"\n")
    if(! is.null(contain)){
      tmpListFiles <- tmpListFiles[grep(pattern = contain,tmpListFiles)]
      cat("Number of selected files:",length(tmpListFiles),"\n")
      cat("Name of selected files: ",paste0(tmpListFiles,collapse ="\n"),"\n")
    }
    cat("Data reading started","\n")
    if(! is.null(selCols)){
      tmpData <- pblapply(tmpListFiles,fread,stringsAsFactors = FALSE,verbose=FALSE,integer64 ="double",select = selCols,nrows = numRows,skip = skipRows)
    }else{
      tmpData <- pblapply(tmpListFiles,fread,stringsAsFactors = FALSE,verbose=FALSE,integer64 ="double",nrows = numRows,skip = skipRows)
    }
    cat("Data reading finished","\n")
  }
  
  if("parquet" == tolower(patternFile)){
    cat("Reading parquet files","\n")
    tmpListFiles <- list.files(path = pathFold,pattern = "*.parquet|*.PARQUET",full.names = TRUE,recursive = recursiveFile)
    cat("Number of total Files:",length(tmpListFiles),"\n")
    if(! is.null(contain)){
      tmpListFiles <- tmpListFiles[grep(pattern = contain,tmpListFiles)]
      cat("Number of selected files:",length(tmpListFiles),"\n")
      cat("Name of selected files: ",paste0(tmpListFiles,collapse ="\n"),"\n")
    }
    cat("Data reading started","\n")
    if(! is.null(selCols)){
      tmpData <- pblapply(tmpListFiles,arrow::read_parquet,col_select = selCols)
    }else{
      tmpData <- pblapply(tmpListFiles,arrow::read_parquet)
    }
    cat("Data reading finished","\n")
    
  }
    
  if("xlsx" == tolower(patternFile)){
  }
  if(outObj == "data.frame"){
  cat("Reducing data to a single dataframe","\n")
  tmpData <-  rbindlist(tmpData,fill=TRUE)#
  return(tmpData)
  }else{
  return(tmpData)
  }
}

#------------------Funcrion to read all excel sheets-------------------#

READ_EXCEL_ALL <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  x <- Reduce(rbind,x)
  return(x)
}

ROW_SUM_TAB <- function(data){
  tmp  <- table(data)
  cols <- colnames(tmp);rows <- rownames(tmp)
  tmp  <- as.matrix.data.frame(tmp)
  cols -> colnames(tmp);rows -> rownames(tmp)
  tmp <- cbind(tmp,Total = rowSums(tmp))
  tmp <- cbind(tmp,HitRate_perc = 100 * (tmp[,cols[1]]/tmp[,"Total"]))
  tmp <- tmp[order(-tmp[,"HitRate_perc"]),]
  return(tmp)
}
