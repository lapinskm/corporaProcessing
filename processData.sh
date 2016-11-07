#!/bin/bash

FILE_NO=0;
mkdir tmp
for FILE in `ls data/*.txt`; do
  ./coocurence.R -b  -o "./tmp/cooc_${FILE_NO}.rds"  ${FILE} &
  FILE_NO=$((${FILE_NO} + 1))
done
wait


SUB_CSV=`ls ./tmp/cooc_*.rds`
echo $SUB_CSV

./cumulate.R ${SUB_CSV}
