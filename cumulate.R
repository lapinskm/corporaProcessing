#!/usr/bin/env Rscript

# ---- wczytanie pakietów ----
if (!(any(installed.packages()[ , "Package"]=="optparse"))) {
   install.packages("optparse")
}
suppressPackageStartupMessages( library(optparse) )

suppressPackageStartupMessages( library(Matrix) )

# ---- obsługa argumentów ----

option_list <- list(
  make_option(c("-o", "--output"),
              default="sum_out.csv",
              help="Specify output csv file name [default is is sum_out.csv]")
)
usage <- "%prog [options] INPUT_CSV_FILES ..."
opt <- parse_args(OptionParser(option_list = option_list,
                               usage = usage),
                  positional_arguments = TRUE,
                  print_help_and_exit = TRUE)

fileNames <- opt$args
outFileName <- unlist(opt$o)[1]
stopifnot( length(fileNames) > 0 )


# ---- wczytanie nazw kolumn i rzędów z plików ----

unionColNames <- unionRowNames <- vector(length = 0)

for(fileName in fileNames) {
  csvContent <- read.csv(fileName, sep=",", row.names=1)
  subMatrix <- as.matrix(csvContent)
  unionRowNames <-union( unionRowNames, rownames(subMatrix))
  unionColNames <-union(unionColNames, colnames(subMatrix))
}

unionRows <-1:length(unionRowNames)
names(unionRows) <- unionRowNames

unionCols <- 1:length(unionColNames)
names (unionCols) <- unionColNames

# ---- pomowne wczytanie i processowanie danych  ----

unionMatrix <- Matrix(data = 0,
                      ncol = length(unionColNames),
                      nrow = length(unionRowNames),
                      dimnames = list(unionRowNames, unionColNames),
                      sparse = TRUE)

sumValues <- function(idxPair, modEnv){
  newDataCol <- idxPair[1];
  newDataRow <- idxPair[2];
  unionDataCol<- newDataColsToUnionMap[newDataCol];
  unionDataRow<- newDataRowsToUnionMap[newDataRow];
  modEnv$unionMatrix[unionDataRow, unionDataCol] <- modEnv$unionMatrix[unionDataRow, unionDataCol] +  modEnv$newData[newDataRow, newDataCol];
  eval(modEnv);
}

for(fileName in fileNames) {
   csvContent <- read.csv(fileName, sep=",", row.names=1)
   newData <- as.matrix(csvContent)
   
   newDataColsToUnionMap <- unionCols[colnames(newData)]
   names(newDataColsToUnionMap)=NULL
   newDataColNames <- colnames(newData);
   newDataCols <- 1:length(newDataColNames);
   
   newDataRowsToUnionMap <- unionRows[rownames(newData)]
   names(newDataRowsToUnionMap)=NULL 
   newDataRowNames <- rownames(newData);
   newDataRows <- 1:length(newDataRowNames);
   
   idxs=expand.grid(newDataCols, newDataRows)
   
   apply(idxs, FUN = sumValues ,MARGIN =1,modEnv=globalenv())
}

# ---- zapisanie macierzy do pliku. ----
write.csv(as.matrix(unionMatrix), outFileName)
