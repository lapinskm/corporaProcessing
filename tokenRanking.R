#!/usr/bin/env Rscript

# ---- load necessary packages ----
if (!(any(installed.packages()[ , "Package"]=="tokenizers"))){
  install.packages("tokenizers")
}
suppressPackageStartupMessages(library(tokenizers))

if (!(any(installed.packages()[ , "Package"]=="optparse"))){
  install.packages("optparse")
}
suppressPackageStartupMessages( library(optparse) )


# ---- handle command line args ----
option_list <- list(
  make_option(c("-o", "--output"), default = "",
              help="Specify output file name [default is is dict.{csv|rds}]"),
  make_option(c("-b" ,"--bin"),action = "store_true", default=FALSE,
              help="Save result as binary file (.rds) [default is disabled]")
)
usage <- "%prog [options] INPUT_FILE[S].txt"
opt <- parse_args(OptionParser(option_list = option_list,
                               usage = usage),
                  positional_arguments = TRUE,
                  print_help_and_exit = TRUE)

if (opt$options$o != "") {
  outFileName <- opt$options$o
} else if (opt$options$b) {
  outFileName <- "dict.out.rds"
} else {
  outFileName <- "dict.out.csv"
}


fileNames <- opt$args
stopifnot( length(fileNames) > 0 )

uniqueWords = c();
cumulatedRanking=c();

for(fileName in fileNames) {
# ---- Read file content ----
  fileContent <- readChar(fileName, file.info(fileName)$size)
  words <- unlist(tokenize_words(fileContent, lowercase = TRUE))
  remove(fileContent)
  #remove numbers
  words <- words[! grepl("\\d", words)]

  uniqueWords <- union(uniqueWords, words)
  fileRanking <- sort(table(words));# not only sorting but also counts occurences what is handy
  newRanking <- structure(rep(0,length(uniqueWords)), names=uniqueWords);
  newRanking[names(cumulatedRanking)] <- cumulatedRanking;
  newRanking[names(fileRanking)] <- newRanking[names(fileRanking)] + fileRanking;
  cumulatedRanking <- newRanking
}
cumulatedRanking <- sort(cumulatedRanking, decreasing=TRUE)

# ---- Save ranking to file ----
if (opt$options$b) {
  saveRDS(cumulatedRanking, outFileName)
} else {
  write.csv(as.matrix(cumulatedRanking), outFileName)
}

