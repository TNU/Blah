#!/bin/bash

testDir='./tests/'
replTestDir='./tests/repl/'
scriptTestDir='./tests/script/'

if [ $# -ge 1 ]; then
    showDiff="TRUE"
else
    showDiff="FALSE"
fi

exact_match() {
    local testCase="$1"
    local wantedFile="$2"
    local outputFile="$3"

    local diffLines;
    diffLines="$(diff "$wantedFile" "$outputFile")"

    if [ $? -eq 1 ]; then
        echo "Failed $testCase"
        if [ "$showDiff" == 'TRUE' ]; then
            echo "$diffLines"
            echo
        fi

        return 1
    else
        return 0
    fi
}

grep_match() {
    local testCase="$1"
    local errorsFile="$2"
    local outputFile="$3"

    local failed="FALSE"
    ./main < "$badFile" > "$outputFile"

    exec 3<"$outputFile"
    exec 4<"$errorsFile"

    oldIFS="$IFS"
    while IFS= read -r output <&3
          IFS= read -r errors <&4; do

        grep "$errors" <<< "$output" > /dev/null

        if [ $? -eq 1 ]; then
            if [ "$failed" == 'FALSE' ]; then
                echo "Failed $badFile"
                failed="TRUE"
            fi

            if [ "$showDiff" == 'TRUE' ]; then
                echo "$errors : $output"
            fi
        fi
    done
    IFS="$oldIFS"

    outputLines="$(wc -l < "$outputFile")"
    errorsLines="$(wc -l < "$errorsFile")"

    if [ $outputLines -ne $errorsLines ]; then
        if [ "$failed" == 'FALSE' ]; then
            echo "Failed $badFile"
            failed="TRUE"
        fi

        if [ "$showDiff" == 'TRUE' ]; then
            echo "Expeced $errorsLines lines but got $outputLines"
        fi
    fi

    if [ "$failed" == 'TRUE' ]; then
        return 1
    else
        return 0
    fi
}

ghc --make main.hs

if [ $? -ne 0 ]; then
    echo '#Compilation Failed#'
    exit 1
fi

failed='FALSE'

for inputFile in "$replTestDir"*".in"; do
    testCase="${inputFile%.in}"
    wantedFile="$testCase.wnt"
    outputFile="$testCase.out"

    ./main < "$inputFile" > "$outputFile"

    if ! exact_match "$testCase" "$wantedFile" "$outputFile"; then
        failed="TRUE"
    fi
done

for badFile in "$replTestDir"*".bad"; do
    testCase="${badFile%.bad}"
    errorFile="$testCase.err"
    outputFile="$testCase.out"

    ./main < "$badFile" > "$outputFile"

    if ! grep_match "$testCase" "$errorFile" "$outputFile"; then
        failed="TRUE"
    fi
done

for inputFile in "$scriptTestDir"*".in"; do
    testCase="${inputFile%.in}"
    wantedFile="$testCase.wnt"
    outputFile="$testCase.out"

    ./main "$inputFile" > "$outputFile"

    if ! exact_match "$testCase" "$wantedFile" "$outputFile"; then
        failed="TRUE"
    fi
done

for badFile in "$scriptTestDir"*".bad"; do
    testCase="${badFile%.bad}"
    errorFile="$testCase.err"
    outputFile="$testCase.out"

    ./main "$badFile" > "$outputFile"

    if ! grep_match "$testCase" "$errorFile" "$outputFile"; then
        failed="TRUE"
    fi
done

if [ "$failed" == 'FALSE' ]; then
    echo '[Passed All Tests]'
else
    echo '#Failed Some Tests#'
fi
