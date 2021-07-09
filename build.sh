#!/bin/bash

compile="g++ -fmax-errors=15 -std=c++11 src/*.cc -o lang -Wall -Wpedantic -Wno-write-strings -Wno-switch -Wno-unused-function -g -ldl -lpthread -O0"
run="./lang demos/hello.lang"

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
