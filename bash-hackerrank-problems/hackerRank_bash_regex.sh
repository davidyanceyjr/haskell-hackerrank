#!/bin/bash
## collect user input into array...

# regex
regexp="[A-Za-z]\+"

readarray -t input

for i in ${input[@]}; do
    if [[ $i =~ $regexp ]]
    then
        printf "%s " "$(echo $i | sed -e 's/[A-Za-z]\+//')"
    else
        :
    fi

done


#positonalPrint=${input[@]:3:5}
#testseq="[a,A]"
#inputSize=${#input[@]}

#printf "%s" "${input[@]}"
#printf "%s" "${positonalPrint[@]}"
#printf "inputSize: %s\n" "$inputSize" 
#printf "tailInput: %s\n" "$tailInput" 
#printf "divideBy: %s\n" "$divideBy" 
#printf "total: %s\n" "$sum" 
#printf "average: %s\n" "$average" 

## old way to read from input into array....
##while IFS= read -r l;  do
##input+=("$l")
##done
