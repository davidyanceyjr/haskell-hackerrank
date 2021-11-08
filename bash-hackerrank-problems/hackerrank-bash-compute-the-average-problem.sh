#!/bin/bash
## for some reason this code outputs 4.000 under the hackerrank IDE
##
## however, if run from terminal with the input:
##    4
##    1
##    2
##    9
##    8
## the output is 5.000 which is correct. 

## read loop to array from stdin
input=()

while IFS= read -r l;  do
    input+=("$l")
done

inputSize=${#input[@]}
tailInput=${input[@]:1:inputSize}
let divideBy="inputSize - 1"

for i in ${tailInput[@]}; do
    let sum+=$i
done

printf "%.3f\n" "$(scale=3; echo "$sum / $divideBy" | bc -l)"
#printf "inputSize: %s\n" "$inputSize" 
#printf "tailInput: %s\n" "$tailInput" 
#printf "divideBy: %s\n" "$divideBy" 
#printf "total: %s\n" "$sum" 
#printf "average: %s\n" "$average" 
