#!/usr/bin/env Rscript

# ---- wczytanie pakietów ----
if (!(any(installed.packages()[ , "Package"]=="optparse"))){
  install.packages("optparse")
}
suppressPackageStartupMessages( library(optparse) )

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

# ---- wczytanie i processowanie plików  ----
cumulatedMatirx=matrix(nrow = 0, ncol = 0);
for(fileName in fileNames) {
   csvContent <- read.csv(fileName, sep=",", row.names=1)
   subMatrix <- as.matrix(csvContent)
   
   collumnSpan <- union(colnames(subMatrix), colnames(cumulatedMatirx))
   rowSpan <- union(rownames(subMatrix), rownames(cumulatedMatirx))
   
   temp1 <- temp2 <- matrix(0, ncol=length(collumnSpan), nrow=length(rowSpan), dimnames=list(rowSpan, collumnSpan))
   
   cumuIndx <- outer(rowSpan, collumnSpan, FUN=paste) %in% outer(rownames(cumulatedMatirx), colnames(cumulatedMatirx), FUN=paste) 
   submIndx <- outer(rowSpan, collumnSpan, FUN=paste) %in% outer(rownames(subMatrix), colnames(subMatrix), FUN=paste)
   temp1[cumuIndx] <- cumulatedMatirx
   temp2[submIndx] <- subMatrix
   
   cumulatedMatirx <- temp1 + temp2
}

# ---- zapisanie macierzy do pliku. ----

write.csv(cumulatedMatirx, outFileName)