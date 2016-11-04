#!/usr/bin/env Rscript

# ---- obsługa argumentów ----

fileNames = commandArgs(trailingOnly=TRUE)
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

dateString<-format(Sys.time(), "%M:%H:%S_%d_%b_%Y")
write.csv(cumulatedMatirx, paste(dateString, ".cooc.csv", sep=""))