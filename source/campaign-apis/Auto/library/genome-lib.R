# Date: 06FEB2020 Time: 11:00AM
# Author: Dreawe Innovation Labs(Abhishek Agnihotri)

# Objective: To create a library of generator and combine functions to be used in generating 
# tags for a customer based on transaction details
# 
# What is tag?
# A tag can be seen as a binary information about customer
# for e.g. family customer, apparel buyer etc..
# 

# Use source(file = "path of this file") function from R-base library to source this script

# Two type of functions:
# Private function: which will not be accessed directly by user, 
# Public function: which will be accessed directly by user, these function may be dependent on primary function


#---- generate tags ----
# Need to generate 2 tags for early-adopter campaign for each season
# Inventory season tag SS[period]
# This should be a generic function which can be used to generate tags 
# for other variables
# Name: GENERATE_TAGS()


#---- Information function ----
#' Function with important information about the file 
#'
#' @return
#' @export
#'
#' @examples
INFO_GENEOME <- function(){
  cat("1. Author: Dreawe Innovation Labs (Abhishek Agnihotri)","\n")
  cat("2. Date: 06FEB2020, Time: 11:00 AM","\n")
  cat("3. Functions for genome-engine","\n")
  cat("4. Name of functions:", "MAP_ROOT, GENERATE_TAGS","GENERATE_PROD_SEASON_TAGS","GENERATE_INVT_SEASON_TAGS","GET_INFO","GET_FILE_INFO","COMBINE_PERIODS","\n")
}



#---- Local functions----
#---- Function 1:    (Public)  MAP_ROOT ----
#' Function to do customer wise root department mapping
#' This function is copied from eoss campaign and may need some imporovements
#' 
#' @param pathDataSale # folder
#' @param pathDataASP  # folder
#' @param pathRFMData  # folder
#' @param pathLangMap  # folder
#' @param rfmCustIdentifier 
#' @param rfmStoreIdentifier 
#' @param langStoreIdentifier 
#' @param saleCustIdentifier 
#' @param rootColumnDataSale 
#' @param saleStoreIdentifier 
#' @param aspColumnName 
#' @param rootColumnASP 
#'
#' @return
#' @export
#'
#' @examples
MAP_ROOT <- function(pathDataSale = choose.dir(),pathDataASP = choose.dir(),pathRFMData = choose.dir(),pathLangMap = choose.dir(),
                        rfmCustIdentifier = c("Customer Mobile"),rfmStoreIdentifier = "enrolledstore",langStoreIdentifier = "Store Name",
                        saleCustIdentifier = c("CUSTOMER_MOBILE"),rootColumnDataSale = c("DEPARTMENT"),saleStoreIdentifier = c("SHORT_NAME"),
                        aspColumnName = c("ASP"),rootColumnASP = c("DEPARTMENT")){
  require(data.table)
  require(dreaweRTools)
  require(dplyr)
  # Working on ASP data
  dataASP <- fread(list.files(pathDataASP,full.names = T),fill = TRUE,stringsAsFactors = FALSE,integer64 = "double")
  # Info about ASP data
  cat("ASP data details","\n")
  print(str(dataASP))
  
  # Selecting unique department from ASP data
  filterDepts <- unique(dataASP$DEPARTMENT)
  cat("Total",length(filterDepts),"selected","\n")
  
  # Reading RFM data
  cat("Reading RFM data","\n")
  RFMfiles <- list.files(pattern = "*.csv",path = pathRFMData,full.names = T,recursive = TRUE)
  dataRFM <- Reduce(rbind,lapply(RFMfiles,fread,integer64 = "double",stringsAsFactors=F))
  print(str(dataRFM))
  cat("RFM data reading finished","\n")
  # Data customer Type
  dataRFMType  <- dataRFM[,c(rfmCustIdentifier,"Customer_Type"),with=FALSE]
  
  # Seprating Store data to merge store set
  dataStoreClus  <- unique(dataRFM[,c(rfmStoreIdentifier,"Cluster"),with=FALSE])
  dataStoreClus[[rfmStoreIdentifier]] <- toupper(dataStoreClus[[rfmStoreIdentifier]])
  
  # Reading language map data
  dataLangMap <- fread(list.files(pathLangMap,full.names = T),fill = TRUE,stringsAsFactors = FALSE,integer64 = "double")
  dataLangMap[[langStoreIdentifier]] <- toupper(dataLangMap[[langStoreIdentifier]])
  cat("Language map reading finished")
  
  # We will use for loop iterating over each file and filter the required departments
  # and required columnss
  # Also, we will use mob_check() function to validate the mobile numbers
  # it will reduce the data size so that we can read as much data as possible
  
  
  # Working on sale data
  # columns to be selected from sale data
  saleDataCols <- c(saleCustIdentifier,rootColumnDataSale,saleStoreIdentifier)
  # listing sale files
  saleFiles <- list.files(pathDataSale,full.name=T,pattern = "*.csv")
  # Initializing empty list to store for loop data
  dataSale <- list()
  fileIndex <- 1
  # Reading files
  for(filePath in saleFiles){
    cat("Processing file: ", fileIndex,".)",filePath,"\n")
    # Eeading files
    temp <- fread(filePath,select = saleDataCols,fill = TRUE,stringsAsFactors = FALSE,integer64 = "double",sep=",")
    cat("File read finished","\n")
    # Converting store column to upper case
    temp[[saleStoreIdentifier]] <- toupper(temp[[saleStoreIdentifier]])
    cat("Store column coverted to upper case","\n")
    # Department filter
    temp <- temp[which(temp[[rootColumnDataSale]] %in% filterDepts),]   
    cat("Department filter done","\n")
    # Applying mob_check funcition
    temp <- mob_check(temp,var=saleCustIdentifier,replace = F)
    cat("Mobile check function utilized","\n")
    # Attching 91 to each customer columns
    temp[[saleCustIdentifier]] <- as.numeric(paste0(91,temp[[saleCustIdentifier]]))
    cat("Attached 91 to each mobile","\n")
    cat("File:details","\n")
    print(str(temp))
    # Merging  with ASP
    temp <- merge(temp,dataASP,by.x = rootColumnDataSale, by.y = rootColumnASP,all.x =TRUE)
    cat("ASP data merged","\n")
    # Sorting by customer mobile and ASP column
    temp <- temp[order(temp[[saleCustIdentifier]],-temp[[aspColumnName]]),]
    cat("Sorting by ASP done","\n")
    # Selecting Highest ASP department
    temp <- temp[!duplicated(temp[[saleCustIdentifier]]),]
    cat("Highest ASP department selected","\n")
	  cat("Number of customers: ",nrow(temp),"\n")
    # Now merging  with RFM data to add Cust Type
    temp <- merge(temp,dataRFMType,by.x=saleCustIdentifier,by.y=rfmCustIdentifier,all.x=TRUE)
    cat("RFM data merge finish","\n")
    # Now merging Store cluster data
    temp <- merge(temp,dataStoreClus,by.x = saleStoreIdentifier,by.y=rfmStoreIdentifier,all.x=TRUE)
    cat("Store CLuster merge finish","\n")
    # Now merging with Language Map data
    temp <- merge(temp,dataLangMap,by.x = saleStoreIdentifier,by.y = langStoreIdentifier,all.x=TRUE)
    cat("Language map merged","\n")
    dataSale[[fileIndex]] <- temp
    cat("Prcessed file: ",fileIndex,"\n")
    fileIndex = fileIndex + 1
    rm(temp)
  }
  cat("All files processed","\n")
  # Reduciing list of sale data to single data frame
  dataSale <- Reduce(rbind,dataSale)
  cat("Reduce to single dataframe","\n")
  # Info about sale data
  cat("Sale data details","\n")
  print(str(dataSale))  
  # Sorting by customer mobile and ASP column
  dataSale <- dataSale[order(dataSale[[saleCustIdentifier]],-dataSale[[aspColumnName]]),]
  cat("Sorted whole data frame according to ASP","\n")
  # Selecting Highest ASP department
  dataSale <- dataSale[!duplicated(dataSale[[saleCustIdentifier]]),]
  cat("Removed duplicate numbers")
  dataSale$ASP <- NULL
  cat("Dimension of dataSale",dim(dataSale),"\n")
  return(dataSale)
}




#---- Function 2:    (Private) GENERATE_TAGS  ----
#' Function to assign a tag to customer based on the the criteria
#' Default new column name is NEW_TAG
#' Output will be binary a tag column containing 1(tagged) 0(not tagged)
#' A row can be classified into a category based on two type of logics
#' 1. rows for which a particular column value include a given value (for categorical variable)
#'    - included
#'    for e.g. if "Attribute 2" contains "SUMMER" then NEW_TAG = 1 else NEW_TAG = 0
#' 2. rows for which a particular column value lie within a set of given values (for continuous variable)
#'    - comparison
#'    for e.g. if "Net Amount SUM" greater than 1000 then NEW_TAG = 1(Above ABV) else NEW_TAG = 0(Not Above ABV)
#'
#' @param data 
#' @param logic 
#' @param colToGrp 
#' @param colToLogic 
#' @param cmp_boundaries 
#' @param cmp_categories 
#' @param con_values 
#' @param tagname 
#'
#' @return
#' @export
#'
#' @examples
GENERATE_TAGS <- function(data, logic = "included", 
                          colToGrp = c(), colToLogic = c() , 
                          cmp_boundaries  = c(), cmp_categories = c(0,1), 
                          con_values = c(),
                          tagname = "NEW_TAG"){
  if(logic == "included"){
    cat("Generating categories","\n")
    data[[tagname]] <- ifelse(data[[colToLogic]] %in% con_values, 1, 0)
    cat("Selecting single record for each value of ", colToGrp, "\n")
    data <- data[order(data[[tagname]],decreasing = TRUE)]
    data <- data[! duplicated(data[[colToGrp]])]
    cat("Number of records per category:","\n")
    print(table(data[[tagname]]))
    cols <- c(colToGrp,tagname)
    data <- data[,..cols] 
  }
  if(logic == "comparison"){
    cmp_boundaries <- c(min(data[[colToLogic]])-1,cmp_boundaries,max(data[[colToLogic]])+1)
    cat("Generating",length(cmp_categories),"categories with following final boundaries: \n",cmp_boundaries,"\n")
    data[[tagname]] <- as.character(
                        cut(data[[colToLogic]],
                          breaks=cmp_boundaries,
                            labels = cmp_categories))
    cat("Selecting single record for each value of ", colToGrp, "\n")
    data <- data[order(data[[tagname]],decreasing = TRUE)]
    data <- data[! duplicated(data[[colToGrp]])]
    cat("Number of records per category:","\n")
    print(table(data[[tagname]]))
    cols <- c(colToGrp,tagname)
    data <- data[,..cols]
  }
  return(data)
}





#---- Function 2.1:  (Public)  GENERATE_PROD_SEASON_TAG ----
# Product season tag SUMMER based on Attribute2 column
# If a customer transaction contains "SUMMER" then tag 1 else 0
# 
# Function will try to fetch information from 
#   - period: filename (FEB to JUL: SUMMER, AUG to JAN: WINTER)
#   -   if period information is missing from the file name, it will search for the 'Bill Date'
#   -   column inside the data and automatically decide the period based on the first 10 rows
#   - tag: it will decide tag based on the period
#   - 


GENERATE_PROD_SEASON_TAGS <- function(data,info=list()){
  require(dreaweRTools)
  mob_check(GENERATE_TAGS(data = data,
                          logic = "included", colToGrp = "Customer Mobile",
                          colToLogic = "Attribute2",con_values = toupper(info$prod_season),
                          tagname = toupper(info$prod_season)),var="Customer Mobile")
}
#---- Function 2.2:  (Public)  GENERATE_INVT_SEASON_TAG ----
GENERATE_INVT_SEASON_TAGS <- function(data,info=list()){
  require(dreaweRTools)
  mob_check(GENERATE_TAGS(data = data,
                          logic = "included", colToGrp = "Customer Mobile",
                          colToLogic = "Desc2",con_values = paste0(toupper(info$invt_season),substr(info$year,3,4)),
                          tagname = "SAME_SEASON"),var="Customer Mobile")
}


  
  

#---- Function 3:    (Private) GET_INFO ----

GET_INFO <- function(path_file ="E:\\vmartSaleDB\\Sale19/1.01JAN2019-31JAN2019_BILLS.csv"){
  require(data.table)
  amonth <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  summer <- amonth[02:07]
  winter <- amonth[c(08:12,01)]
  # Getting period info
  # Try 1: from file name
  tmp_month <- strsplit(path_file,split = "[/]")[[1]]
  year <- substr(tmp_month[length(tmp_month)],start = nchar(tmp_month)-13,stop = nchar(tmp_month)-10)
  tmp_month <- strsplit(tmp_month[length(tmp_month)],split = "[-_]")[[1]]
  tmp_month  <- substr(tmp_month[length(tmp_month)-1],start = 3,stop = 5)
  return(list("prod_season" = ifelse(tmp_month %in% summer,"SUMMER","WINTER"),
              "invt_season" = ifelse(tmp_month %in% summer,"SS","AW"),
              "year" = year,
              "month" = tmp_month))
}

GET_DATA_PERIOD <- function(pathFold = getwd(),patternFile = "parquet",recursive = TRUE, contain = ""){
  tmpListFiles <- list.files(path = pathFold,pattern = "*.parquet",full.names = TRUE,recursive = recursiveFile)
  tmpListFiles <- sort(as.Date(unique(unlist(sapply(substr(basename(tmpListFiles[grep(pattern = contain,tmpListFiles)]),start = 1, stop = 19),strsplit,"_"))),"%d%b%Y"))
  period <- list(min_p = min(tmpListFiles,na.rm = TRUE), max_p = max(tmpListFiles,na.rm = TRUE))
  return(period)
}




#path <-list.files("E:\\vmartSaleDB\\Sale19",full.names = TRUE)
#lapply(path,GET_FILE_INFO)

#---- Function 4:    (Public)  COMBINE_PERIODS(genome) ----
# Renamed: COMBINE_PERIODS ---> COMBINE_PERIODS_TEMP
COMBINE_PERIODS_TEMP <- function(list_data = list()){
  require(dplyr)
  list_data <- bind_rows(list_data)
  cat("Total rows consolidated: ",nrow(list_data),"\n")
  # Aggregating for customers with adding values
  list_data <- list_data %>% group_by(`Customer Mobile`) %>% summarise_all(sum,na.rm = TRUE)
  # Replacing greater than 1 by 1
  list_data[,-1][which(list_data[,-1]>1,arr.ind=T)] <- 1
  cat("Total rows removing duplicates: ",nrow(list_data),"\n")
  cat("Customers in each category","\n")
  print(t(apply(list_data[-1],2,table)))
  return(list_data)
}

#---- Function 5:    (Public)  COMBINE_PERIODS_DATES ----
COMBINE_PERIODS_DATES <- function(list_data = list(),varId = "Customer Mobile",varDate = "Bill Date",selDate = "First"){
  require(dplyr)
  require(data.table)
  # selecting columns and binding rows
  list_data <- lapply(list_data, function(x){ x %>% select_at(.vars = c(varId,varDate))})
  list_data <- bind_rows(list_data)
  cat("Total rows consolidated: ",nrow(list_data),"\n")
  
  # Breaking in single and multiple customers
  setDT(list_data)
  list_data[, Count := .N, by=get(varId)]
  dt_single <- list_data[Count == 1]
  dt_single[["Count"]] <- NULL
  list_data <- list_data[Count > 1]
  list_data[["Count"]] <- NULL
  
  #dt_single <- list_data %>% group_by_at(.vars = varId) %>% filter(n() == 1) %>% data.table()
  #list_data <- list_data %>% group_by_at(.vars = varId) %>% filter(n() > 1) %>% data.table()
  
  list_data[[varDate]] <- as.Date(list_data[[varDate]],"%d-%b-%y") 
  # Aggregating for customers with adding values
  # Selecting according to first date
  if(selDate == "First"){
    list_data <- list_data[(order(list_data[[varDate]])),]
    list_data[[varDate]] <- format(list_data[[varDate]],"%d-%b-%y")
  }
  # Selecting according to last date
  if(selDate == "Last"){
    list_data <- list_data[rev(order(list_data[[varDate]])),]
    list_data[[varDate]] <- format(list_data[[varDate]],"%d-%b-%y")    
  }
  # Selcting record based on the selDate variable
  list_data <- list_data %>% filter_at(vars(varId), ~ ! duplicated(.))
  cat("Total rows removing duplicates: ",nrow(list_data),"\n")
  cat("Customers in each category","\n")
  # binding with single id's
  list_data <- data.table(rbind(list_data,dt_single))
  return(list_data)
}

#---- Function 6:    (Public)  COMBINE_PERIODS_AGG ----
COMBINE_PERIODS_AGG <- function(list_data = list(),varId = "Customer_Mobile",varAgg = "Net_Amount_SUM_Total"){
  require(dplyr)
  list_data <- lapply(list_data, function(x){ x %>% select_at(.vars = c(varId,varAgg))})
  list_data <- bind_rows(list_data)
  cat("Total rows consolidated: ",nrow(list_data),"\n")
  
  # Breaking in single and multiple customers
  setDT(list_data)
  list_data[, Count := .N, by=get(varId)]
  dt_single <- list_data[Count == 1]
  dt_single[["Count"]] <- NULL
  list_data <- list_data[Count > 1]
  list_data[["Count"]] <- NULL
  
  # dt_single <- list_data %>% group_by_at(.vars = varId) %>% filter(n() == 1) %>% data.table()
  # list_data <- list_data %>% group_by_at(.vars = varId) %>% filter(n() > 1) %>% data.table()
   
  list_data <- list_data %>% group_by_at(.vars = c(varId)) %>% summarise_at(vars(varAgg),sum,na.rm = TRUE)
  # Aggregating on customer level
  cat("Total rows removing duplicates: ",nrow(list_data),"\n")

  # binding with single id's
  list_data <- data.table(rbind(list_data,dt_single))
  return(list_data)
}


#---- Function 7:    (Public)  COMBINE_PERIODS_FIRST ----
COMBINE_PERIODS_FIRST <- function(list_data,varId = "Customer_Mobile",varStr = "Store",varDate = "Bill_Date",mobCheck = FALSE){
  require(dplyr)
  list_data <- lapply(list_data, function(x){ x %>% select_at(.vars = c(varId,varStr,varDate))})
  list_data <- bind_rows(list_data)

  # Breaking in single and multiple customers
  setDT(list_data)
  list_data[, Count := .N, by=get(varId)]
  dt_single <- list_data[Count == 1]
  dt_single[["Count"]] <- NULL
  list_data <- list_data[Count > 1]
  list_data[["Count"]] <- NULL
  
  # dt_single <- list_data %>% group_by_at(.vars = varId) %>% filter(n() == 1) %>% data.table()
  # list_data <- list_data %>% group_by_at(.vars = varId) %>% filter(n() > 1)  %>% data.table()
  
  list_data[[varDate]] <- as.Date(list_data[[varDate]],tryFormat = c("%d-%b-%y"))
  list_data <- list_data[(order(list_data[[varDate]])),]
  list_data[[varDate]] <- format(list_data[[varDate]],"%d-%b-%y")
  list_data <- list_data %>% filter(! duplicated(get(varId))) %>% select(varId,varStr)
  cat("Total unique records for ID variable: ", n_distinct(list_data[[varId]]),"\n")
  
  # binding with single id's
  list_data <- data.table(rbind(list_data,dt_single %>% select(varId,varStr)))
  return(list_data)
}




#---- Function 8:    (Public)  COMBINE_PERIODS ----
COMBINE_PERIODS <- function(data,varId = "Customer_Mobile",varStr = "Store", varFirstDate = "First_Transaction_Date", 
                            varLastDate = "Last_Transaction_Date", varFreq = "Frequency"){
  # Combining and selecting first transaction dates
  dt_first_date <- COMBINE_PERIODS_DATES(list_data = data, varId = varId,varDate = varFirstDate,selDate = "First")
  # Combining and selecting last transaction dates
  dt_last_date  <- COMBINE_PERIODS_DATES(list_data = data, varId = varId,varDate = varLastDate,selDate = "Last")
  # Combining and adding frequency
  dt_freq  <- COMBINE_PERIODS_AGG(list_data = data, varId = varId, varAgg = varFreq)
  # Updating store according to the date
  dt_str  <- COMBINE_PERIODS_FIRST(list_data = data, varId = varId, varStr = varStr,varDate = varFirstDate, mobCheck = FALSE)
  
  # Joining tales
  cat("Joining generated tables","\n")
  dt <- left_join(dt_first_date, dt_last_date,  by = varId) %>%
                  left_join(., dt_freq,         by = varId) %>%
                  left_join(., dt_str,          by = varId)
  rm(dt_first_date);rm(dt_last_date);rm(dt_freq)
  return(dt)
}


#---- Function 9:    (Public)  GENERATE_DATA_PERIOD ----

# It will show the minimum and maximum dates as the period,
# It will check if any dates are missing betweem the periods and display these dates
# format  dreawe-date: %d-%b-%y, dreawe-file: %d%B%Y 
GENERATE_DATA_PERIOD <- function(data,var = "Bill Date",formatDate = "dreawe-date"){
  uniq_p <- unique(data[[var]])
  period <- as.Date(uniq_p,"%d-%b-%y")
  min_p <- min(period)
  max_p <- max(period)
  diff_p <- as.numeric(max_p - min_p + 1)
  if(diff_p != length(uniq_p) & diff_p > 1){
    tmp_seq <- seq.Date(min_p,max_p,by = 1)
    miss_p  <- tmp_seq[! tmp_seq %in% uniq_p]
    cat("\n\n")
    cat("Following dates are missing from the period: ", paste0(miss_p,collapse = ", "),"\n")
  }
  retPeriod <- as.Date(c(min_p,max_p),"%Y-%m-%d")
  cat("\n")
  retPeriod <- format(retPeriod,"%d-%b-%y")
  cat("Data period: From ",paste0(retPeriod[1],""), " to ", paste0(retPeriod[2],""),"\n")
  if(formatDate =="dreawe-file"){
    retPeriod <- as.Date(c(min_p,max_p),"%Y-%m-%d")
    retPeriod <- toupper(format(retPeriod,"%d%b%Y"))
  }
  return(retPeriod)
}




#---- Function 10:    (Public)  GENERATE_DEPT_INFO ----

# It will generate frequency & quantity count for the customers for the list of departments provided

GENERATE_DEPT_INFO <- function(data,listCat){
  dt_qty <- GENERATE_SALE_QTY(data = data,varAgg = "Bill Qty SUM",varCat = "Department",fun = "sum",varAggFun = "Add",varAct = "bought",
                         varQualifier = "quantity of",wide = TRUE,mobCheck = TRUE,writeIndex = FALSE,filterCat = TRUE,listCat = listCat)
  data[["Department_Frequency"]] <- paste0(data[["Bill Date"]],"-",data[["Department"]])
  dt_freq <- GENERATE_AGG(data = data,varAgg = "Department_Frequency",varCat = "Department", filterCat = TRUE,listCat = listCat,varAggFun = "Add",varAct = "shopped",
                          varQualifier = "number of times in",fun = "ucount",wide=TRUE,mobCheck = TRUE,writeIndex = FALSE)
  dt <- left_join(dt_qty,dt_freq, by ="Customer Mobile")
  return(dt)
}


#---- Function 11    (Public) COMBINE_PERIODS_CPM ----
# combine customer product master

COMBINE_PERIODS_CPM <- function(data){
  # Combining and selecting first transaction dates
  dt_first_date <- COMBINE_PERIODS_DATES(list_data = data, varId = "Customer Mobile",varDate = "First Transaction Date",selDate = "First")
  # Updating store according to the date
  dt_str  <- COMBINE_PERIODS_FIRST(list_data = data,varId = "Customer Mobile",varStr = "Source Short Name",varDate = "First Transaction Date",mobCheck = FALSE)
  # Combining and adding frequency
  dt_sq  <- COMBINE_PERIODS_AGG(list_data = data, varAgg = colnames(data[[1]])[-c(1:4)])
  
  # Joining tales
  cat("Joining generated tables","\n")
  dt <- left_join(dt_str,dt_first_date, by = 'Customer Mobile') %>%
    left_join(., dt_sq,           by = 'Customer Mobile')
  
  rm(dt_first_date);rm(dt_str)
  return(dt)
}


# Update: 24DEC2020
#---- Function 12:    (Public) GENERATE_EACH_BILL  ----
# This function is used to generate tags which needs a criteria to fullfill on all bills of a customer in a given period
# Input:
#' @data: data.frame
#' @varType: count or sum
#' @tagName: name for new tag
#' @varCriteria: less, equal, greater, between
#' @valCriteria: value(s) to define the criteria
#' @varId: Id column
#' @varGrp: columns used for grouping
#' @varAgg: 
#' Output:
#' data with two columns


GENERATE_EACH_BILL <- function(data,type = "sum",varId = "Customer Mobile",varGrp = c("Bill No","Division"),varAgg = "Bill Qty SUM",mobCheck = TRUE,valCriteria = 9,
                               tagName = "Bill_Units_10"){
  require(dplyr)
  require(data.table)
  cat("Selecting columns","\n")
  # selecting columns and binding rows
  data <- data %>% select_at(.vars = all_of(c(varId,varGrp,varAgg)))
  if(mobCheck == TRUE){
    data <- mob_check(data,var = varId)
  }
  if(type == "sum"){
    cat("Selecting customer based on sum operation","\n")
    data <- data %>% group_by_at(.vars = all_of(c(varId,varGrp))) %>% summarise_at(vars(varAgg),sum,na.rm = TRUE)
    data[["ConditionCol"]] <- (data[[varAgg]] > valCriteria)
    data <- data %>% group_by_at(.vars = all_of(c(varId))) %>% summarise("ConditionCol" = prod(ConditionCol,na.rm = TRUE),
                                                                         "Bills" = n_distinct(`Bill No`,na.rm = TRUE))
    #data <- data %>% filter(ConditionCol > 0)
    colnames(data)[2] <- tagName
    return(data)
  }
  if(type == "ucount"){
    cat("Selecting customer based on unique count operation","\n")
    data <- data %>% group_by_at(.vars = all_of(c(varId,varGrp))) %>% summarise_at(vars(varAgg),n_distinct,na.rm = TRUE)
    data[["ConditionCol"]] <- (data[[varAgg]] > valCriteria)
    data <- data %>% group_by_at(.vars = all_of(c(varId))) %>% summarise("ConditionCol" = prod(ConditionCol,na.rm = TRUE),
                                                                         "Bills" = n_distinct(`Bill No`,na.rm = TRUE))
    #data <- data %>% filter(ConditionCol > 0)
    colnames(data)[2] <- tagName
    return(data)
  }
}



