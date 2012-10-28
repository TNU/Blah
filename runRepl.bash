#!/bin/bash

testDir='./tests/'
testCaseDir='./tests/repl/'

ghc --make main.hs

if [ $? -ne 0 ]; then
    echo '#Compilation Failed#'
    exit 1
fi

failed=""
for inputFile in "$testCaseDir"*".in"; do
    testCase="${inputFile%.in}"
    wantedFile="$testCase.wnt"
    outputFile="$testCase.out"

    ./main < "$inputFile" > "$outputFile"
    diffLines="$(diff "$wantedFile" "$outputFile")"

    if [ $? -eq 1 ]; then
        echo "Failed $testCase"
        failed="True"

        if [ $# -ge 1 ]; then
            echo "$diffLines"
            echo
        fi
    fi
done

for badFile in "$testCaseDir"*".bad"; do
    testCase="${badFile%.bad}"
    errorsFile="$testCase.err"
    outputFile="$testCase.out"

    failedThisTest=""
    ./main < "$badFile" > "$outputFile"

    exec 3<"$outputFile"
    exec 4<"$errorsFile"

    oldIFS="$IFS"
    while IFS= read -r output <&3
          IFS= read -r errors <&4; do

        grep "$errors" <<< "$output" > /dev/null

        if [ $? -eq 1 ]; then
            if [ -z "$failedThisTest" ]; then
                echo "Failed $badFile"
                failedThisTest="True"
                failed="True"
            fi

            if [ $# -ge 1 ]; then
                echo "$errors : $output"
            fi
        fi
    done
    IFS="$oldIFS"

    outputLines="$(wc -l < "$outputFile")"
    errorsLines="$(wc -l < "$errorsFile")"

    if [ $outputLines -ne $errorsLines ]; then
        if [ -z "$failedThisTest" ]; then
            echo "Failed $badFile"
            failedThisTest="True"
            failed="True"
        fi

        if [ $# -ge 1 ]; then
            echo "Expeced $errorsLines lines but got $outputLines"
        fi
    fi

done

if [ -z "$failed" ]; then
    echo '[Passed All Tests]'
else
    echo '#Failed Some Tests#'
fi
