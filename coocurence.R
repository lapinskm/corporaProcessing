#!/usr/bin/env Rscript

# ---- wyczyszczenie work-space-a ----
remove(list = ls())

# ---- wczytanie niezbędnych pakietów ----
if (!(any(installed.packages()[ , "Package"]=="zoo"))){
  install.packages("zoo")
}
library(zoo) #rollapply

if (!(any(installed.packages()[ , "Package"]=="tokenizers"))){
  install.packages("tokenizers")
}
library(tokenizers)

# ---- obsługa argumentów ----
args = commandArgs(trailingOnly=TRUE)

if ( length(args) <= 0 ) {
  #domyślna wartość
  fileName <- "const.txt"
  setwd(getSrcDirectory(function(x) {x}))
  getSrcDirectory(function(x) {x})
} else {
  fileName <- args[1]
}

# ---- Odczyt pliku ----
fileContent <- readChar(fileName, file.info(fileName)$size)

words <- unlist(tokenize_words(fileContent,lowercase = TRUE))
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
dictNames <- names(sort(table(words),decreasing=TRUE))# zrobienie listy słów występującej korpusie - posortowana wg popularności
dictSize <- length(dictNames)
dictIds <- 1 : dictSize # zrobienie listy id-ków

# ---- mapowanie słów na identyfikatory liczbowe w tekscie ----
names(dictIds) <- dictNames #magiczny myk- zrobienie mapowania id-słowo
mappedWords <- c(rep(NA,floor(leftWindow)), dictIds[words],rep(NA,floor(rightWindow))) # zamiana słów na zmapowane numery

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
  sampledToken=window[modEnv$sampledTokenIndex]
  window <- window[window %in%  modEnv$featureVector] #liczymy współwystąpienia tylko ze słowami z wektora cech
  pairs <- matrix( c(rep(sampledToken,length(window)),window),nrow = length(window), ncol=2 )
  apply(pairs,  FUN=modEnv$processPair, MARGIN = 1, modEnv = modEnv)
  NULL
}

rollapply(mappedWords, windowSize,FUN=processWindow, modEnv=globalenv()) # przetworzenie danych podzielonych na okna

# ---- opisanie kolumn i rzędów ---- 
# wiersze odpowiadają wektorom współwystąpień
colnames(coocMatrix)=dictNames[featureVector]
rownames(coocMatrix)=dictNames

# ---- zapisanie macierzy współwystąpień do pliku ----
write.csv(coocMatrix,paste(fileName,".cooc.csv"))


