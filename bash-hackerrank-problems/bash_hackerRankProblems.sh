#!/bin/bash
# SCALENE triangles - all three sides are different
# Equilateral triangles - all three sides are the same
# Isosceles triangles - two sides are the same.


## collect user input into array...
readarray -t input

if [ ${input[0]} -eq ${input[1]} ]
then
    if [ ${input[0]} -eq ${input[2]} ]
    then
        echo "EQUILATERAL"
    else 
            echo "ISOSCELES"
    fi
else
    if [ ${input[0]} -eq ${input[2]} ]
    then
        echo "ISOSCELES"
    else
        if [ ${input[1]} -eq ${input[2]} ]
        then
            echo "ISOSCELES"
        else
            echo "SCALENE"
        fi
    fi
fi

#for i in ${input[@]}; do
#    if [[ $i =~ $regexp ]]
#    then
#        printf "%s " "$(echo $i | sed -e 's/^[A-Z]/\./')"
#    else
#        printf "%s " "$i"
#    fi

#done
#
#
# regex
#regexp="^[A-Z]"

##positonalPrint=${input[@]:3:5}
##testseq="[a,A]"
##inputSize=${#input[@]}

#printf "%s" "${input[@]}"
#printf "%s" "${positonalPrint[@]}"
#printf "inputSize: %s\n" "$inputSize" 
#printf "tailInput: %s\n" "$tailInput" 
#printf "divideBy: %s\n" "$divideBy" 
#printf "total: %s\n" "$sum" 
#printf "average: %s\n" "$average" 

## old way to read from input into array....
#while IFS= read -r l;  do
#    input+=("$l")
#done
