#!/usr/bin/env Rscript

# ---- wczytanie niezbędnych pakietów ----
if (!(any(installed.packages()[ , "Package"]=="zoo"))){
  install.packages("zoo")
}
suppressPackageStartupMessages(library(zoo)) #rollapply

if (!(any(installed.packages()[ , "Package"]=="tokenizers"))){
  install.packages("tokenizers")
}
suppressPackageStartupMessages(library(tokenizers))

if (!(any(installed.packages()[ , "Package"]=="optparse"))){
  install.packages("optparse")
}
suppressPackageStartupMessages( library(optparse) )

# ---- obsługa argumentów ----
option_list <- list(
  make_option(c("-o", "--output"), default = "",
              help="Specify output file name [default is is cooc.out.{csv|rds}]"),
  make_option(c("-b" ,"--bin"),action = "store_true", default=FALSE,
              help="Save result as binary file (.rds) [default is disabled]")
  )
usage <- "%prog [options] INPUT_FILE.txt"
opt <- parse_args(OptionParser(option_list = option_list,
                               usage = usage),
                  positional_arguments = TRUE,
                  print_help_and_exit = TRUE)

if (opt$options$o != "") {
  outFileName <- opt$options$o
} else if (opt$options$b) {
  outFileName <- "cooc.out.rds"
} else {
  outFileName <- "cooc.out.csv"
}

inputFile <- opt$args
stopifnot( length(inputFile) == 1 )

# ---- Odczyt pliku ----
fileContent <- readChar(inputFile, file.info(inputFile)$size)

words <- unlist(tokenize_words(fileContent,
                               lowercase = TRUE))
#remove numbers
words <- words[! grepl("\\d", words)]

remove(fileContent)

# ---- Parametry macierzy współwystąpień ----
featureVectorSize <- 100 #współwystąpienia będą liczone ze 100-ma najczęstszymi słowami
#parametry okna dla którego będzie liczona macierz współwystąpień
leftWindow <- 2;
rightWindow <- 2;
windowSize <- leftWindow+1+rightWindow;

sampledTokenIndex <- leftWindow+1;

# ---- stworzenie słownika z mapowaniem słów na identyfikatory ----
# trochę magii optymalizacyjnej-robimy macierz na liczbach zamiast słów - będzie lżej zarówno dla pamięci jak i procesora.
dictNames <- names(sort(table(words),
                        decreasing=TRUE))# zrobienie listy słów występującej korpusie - posortowana wg popularności
dictSize <- length(dictNames)
dictIds <- 1 : dictSize # zrobienie listy id-ków

# ---- mapowanie słów na identyfikatory liczbowe w tekscie ----
names(dictIds) <- dictNames #magiczny myk- zrobienie mapowania id-słowo
mappedWords <- c(rep(NA,floor(leftWindow)),
                 dictIds[words],
                 rep(NA,floor(rightWindow))) # zamiana słów na zmapowane numery

# ---- najpopularniejsze słowa z którymi będzie mierzone współwystępowanie ----
featureVector <- 1 : featureVectorSize; #współwystępowanie z tymi słowami będzie naszym wekorem cech

# sprzątanie zbędnych danych
remove(words)# nie potrzebujemy już tej macierzy w pamięci.
names(mappedWords) <- NULL # nie potrzebujemy również tego (zrobiło się przy okazji mapowania)

# ---- stworzenie macierzy współwystąpień ----
coocMatrix <- matrix(0, dictSize, featureVectorSize ) #optymalizacja - możemy prealokować macierz bo znamy jej wymiary

processPair <- function(pair, modEnv) # funkcja inkrementująca macierz współwystąpień dla podanych słów
{
  modEnv$coocMatrix[pair[1],pair[2]] <- modEnv$coocMatrix[pair[1],pair[2]] + 1;
  eval(modEnv)
  NULL
}

processWindow <- function(window, modEnv) # funkcja zliczająca pary współwystąpień dla okna
{
  sampledToken <- window[modEnv$sampledTokenIndex]
  window <- window[window %in%  modEnv$featureVector] #liczymy współwystąpienia tylko ze słowami z wektora cech
  pairs <- matrix( c(rep(sampledToken,length(window)),window),
                   nrow = length(window), ncol=2 )
  apply(pairs,  FUN=modEnv$processPair, MARGIN = 1, modEnv = modEnv)
  NULL
}

# przetworzenie danych podzielonych na okna
invisible(rollapply(mappedWords,
                    windowSize,
                    FUN = processWindow,
                    modEnv=globalenv()))

# ---- opisanie kolumn i rzędów ---- 
# wiersze odpowiadają wektorom współwystąpień
colnames(coocMatrix) <- dictNames[featureVector]
rownames(coocMatrix) <- dictNames

# ---- zapisanie macierzy współwystąpień do pliku ----
if (opt$options$b) {
  saveRDS(coocMatrix, outFileName)
} else {
  write.csv(coocMatrix, outFileName)
}

