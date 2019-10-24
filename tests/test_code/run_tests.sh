#!/bin/bash

# Makes test files, runs all tests

#To do: add cleaning (remove *out, *~ files before each run)

WORKDIR=`pwd -P`

mapfile -t DIRECTORIES < <( ls -d $WORKDIR/*/ )
#note the obtuse spacing for the redirect

printf "Will loop over the following directories:\n"
printf "%s\n" "${DIRECTORIES[@]}"
printf "\n"

# Make test executables
printf "Making all available test suites:\n\n"
for DIR in "${DIRECTORIES[@]}"; do

    cd $DIR
    printf "In directory %s:\n" $DIR
    mapfile -t MAKEFILES < <(ls make*)
    printf "%s\n" "${MAKEFILES[@]}"
    printf "\n"

    for TARGET in "${MAKEFILES[@]}"; do
	printf "Making %s.\n" $TARGET
	make -f $TARGET
    done
    printf "\n"
done

#run test executables
printf "Running all available test suites:\n\n"
for DIR in "${DIRECTORIES[@]}"; do

    cd $DIR
    printf "In directory %s:\n" $DIR
    mapfile -t EXECUTABLES < <(ls Test*.x)
    printf "%s\n" "${EXECUTABLES[@]}"
    printf "\n"

    for EXEC in "${EXECUTABLES[@]}"; do
	printf "Running %s.\n" $EXEC
	./$EXEC > ${EXEC}-run.out
    done
    printf "\n"
done

# Write summary file
cd $WORKDIR  
grep -H -A1 "SUMMARY" */*run.out > test_summary.out

printf "List successful tests:\n"
grep "passed" test_summary.out
printf "\n"

printf "List failed tests (please check test output file for more info):\n"
grep "failed" test_summary.out
printf "\n"

printf "DONE\n"

