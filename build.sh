#!/bin/bash

compile="gcc -std=c99 src/*.c -o lang -Wall -Wpedantic -Wno-switch -Wno-unused-function -g -ldl -lpthread -O0"
run="./lang demos/imports.lang"

echo -e "\nCompiling:"
echo -e "\t$compile\n"

eval $compile

if [ $? != 0 ]; then
    echo -e "\nCompilation failed!"
else
    echo -e "Success!\n"

    if [ "$1" = "-r" ]; then
        eval $run
    fi
fi
