#!/bin/bash

FILE_NO=0;
mkdir tmp
for FILE in `ls data/*.txt`; do
  ./coocurence.R -o "./tmp/cooc_${FILE_NO}.csv"  ${FILE} &
  FILE_NO=$((${FILE_NO} + 1))
done
wait


SUB_CSV=`ls ./tmp/cooc_*.csv`

./cumulate.R ${SUB_CSV}
