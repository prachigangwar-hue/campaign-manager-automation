#' Function to break and save data into multiple files based on rows in data
#' Input:
#' Output:
BREAK_AND_SAVE <- function(data,numPart = "DEFAULT",numRow = 500000,fileExt = "csv",path = getwd(),shareFileName = "Partition"){
  require(data.table)
  require(writexl)
  if(numPart == "DEFAULT"){
    rowPart <- unique(c(seq(1,nrow(data),by = numRow),nrow(data)))
    cat("data will be partitioned on: ",rowPart,"\n")
    for(i in 1:(length(rowPart)-1)){
      tmpData <- data[rowPart[i]:rowPart[i+1],]
      cat(i,"Part Rows",nrow(tmpData),"\n")
      switch(toupper(fileExt),
             CSV = fwrite(tmpData,file.path(path,paste0(shareFileName,"_Part-",i,".csv")),row.names = F),
             XLSX = write_xlsx(tmpData,file.path(path,paste0(shareFileName,"_Part-",i,".xlsx"))))
    }
  }else{
    rowPart <- round(seq(1,nrow(data),length = numPart),0)
    print(rowPart)
    for(i in 1:(length(rowPart)-1)){
      tmpData <- data[rowPart[i]:rowPart[i+1],]
      cat(i,"Part Rows",nrow(tmpData),"\n")
      switch(toupper(fileExt),
             CSV = fwrite(tmpData,file.path(path,paste0(shareFileName,"_Part-",i,".csv")),row.names = F),
             XLSX = write_xlsx(tmpData,file.path(path,paste0(shareFileName,"_Part-",i,".xlsx"))))
    }
  }
}

#' Function to break and save data into multiple files based on given column and its categories
#' Input:
#' Output:
BREAK_AND_SAVE_COL <- function(data,var = "Root_Department",shareFileName = "Partition",fileExt = "CSV",path=getwd(),selCol = "All"){
  require(dplyr)
  uniqCat <- unique(data[[var]])
  cat("Generating ",length(uniqCat),"files","\n")
  fileIndex <- 1
  for(i in uniqCat){
    if(selCol == "All"){
      tmpData <- data %>% filter_at(var,~ . == i)
    }else{
      tmpData <- data %>% filter_at(var,~ . == i) %>% select_at(vars(selCol))
    }
    cat("Generating ",fileIndex,"file for Category ",i,"\n")
    if(is.null(shareFileName)){
      switch(toupper(fileExt),
             CSV = fwrite(tmpData,file.path(path,paste0(shareFileName,i,gsub("/","_",i),"Part-",fileIndex,".csv")),row.names = F),
             XLSX = write_xlsx(tmpData,file.path(path,paste0(shareFileName,i,gsub("/","_",i),"Part-",fielIndex,".xlsx"))))
    }else{
      switch(toupper(fileExt),
             CSV = fwrite(tmpData,file.path(path,paste0(shareFileName,"_",i,"_Part-",fileIndex,".csv")),row.names = F),
             XLSX = write_xlsx(tmpData,file.path(path,paste0(shareFileName,"_",i,"_Part-",fielIndex,".xlsx"))))
    }

    fileIndex <- fileIndex + 1
  }
}
