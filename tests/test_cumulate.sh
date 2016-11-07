#!/bin/bash
echo testing cumulate.R ...
../cumulate.R -o out.csv a.csv b.csv
cmp --silent ab.csv out.csv || echo "FAIL"

echo "PASS"

rm out.csv
