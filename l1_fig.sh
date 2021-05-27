#! /bin/bash

#A shell script to call l1_performance_plot.R and get the l1 performance plot
#Must have already generated results using eval_hart.sh to run l1_experiments.R
#1 = the dataset name


Rscript l1_performance.R "$1"
