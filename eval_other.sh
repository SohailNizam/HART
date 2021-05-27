#! /bin/bash

#Run the non_HAL.R script, pass two arguments
#1 = the dataset name (e.g. fev)
#2 = the algo name (e.g. rpart)

Rscript non_HAL.R "$1" "$2"
