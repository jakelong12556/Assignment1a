#!/bin/bash

diff="diff -iad" 

./test_bst > out.txt
$diff out.txt sol.txt

e_code=$?
if [ $e_code != 0 ]; then
    printf "TEST FAIL : %d\n" "$e_code"
else
    printf "TEST OK!\n"
fi

