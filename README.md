#  Highly Adaptive Regression Trees

Highly Adaptive Regression Trees (HART) is a method for mapping Highly Adaptive Lasso (HAL) model fits to decision trees. The files in this repository allow for reproduction of results shown in our paper submitted to NeurIPS 2021. Code was written in R version 4.0.3 (distributed under the terms of the GNU General Public License version 3) and Python version 3.7 (distributed under the terms of the PSF license agreement). There are 3 main things to explore/reproduce in this code. Below are descriptions of these tasks and how to perform them using the code and data stored in this repository. Before beginning, download the entire repository, preserve the file structure, and set this as your working directory. Please note, the code will be very time consuming to run, espcially when analyzing the cpu dataset.


# 1. HART Size and Performance (Table 1 from paper submission)

- Goal: Evaluate the size (in terms of terminal node and unique terminal node counts) and performance (in terms of cross-validated R^2) of HARTs mapped from HAL model fits built using many random seeds on a user-chosen dataset.

- Packages/libraries required: hal9001 (R Library), anytree (Python Library), pandas (Python Library), random (Python Library), sys (Python Library). See requirements.txt for instructions on installing these libraries.

- Instructions: To reproduce the HART results from the Table 1 of the paper submission, run the following commands at the command line from the current directory:

```
$./eval_hart.sh 0 cpu 10

$./eval_hart.sh 0 fev 100

$./eval_hart.sh 0 mussels 100
```

The result will be 3 csv files named cpu_hart_eval.csv, fev_hart_eval.csv, and mussels_hart_eval.csv, each with 3 columns: R^2, Terminal Node Count, Unique Terminal Node count. The number of rows will correspond to the third argument in each command above (equal to the number of random seeds used to fit HAL for each dataset). Seeds are automatically read in from the corresponding csv file in the ./seeds/ directory. The seeds in those files are the seeds we used to generate the results in the paper submission. The final output files can be analyzed for mean and sd of terminal node count, unique terminal node count, and R^2 value (as in the paper). 

# 2. HART Size and Performance With Heavy L1 Penalization

- Goal: Evaluate the relationship between HART size and cv-R^2 as we change the l1 penalization term during fitting of HAL.

- Packages/libraries required: hal9001 (R Library), ggplot2 (R Library), dplyr (R Library), anytree (Python Library), pandas (Python Library), random (Python Library), sys (Python Library). See requirements.txt for instructions on installing these libraries.

- Instructions: To reproduce results from the Figure 7 of the paper submission, run the following commands at the command line from the current directory:

```
$./eval_hart.sh 1 cpu 85
$./l1_fig.sh cpu

$./eval_hart.sh 1 fev 85
$./l1_fig.sh fev

$./eval_hart.sh 1 mussels 85
$./l1_fig.sh mussels
```
The first result will be 3 csv files named cpu_hart_eval.csv, fev_hart_eval.csv, and mussels_hart_eval.csv, each with 3 columns: R^2, Terminal Node Count, Unique Terminal Node count. The number of rows will correspond to the third argument in each command above (we restrict to models built with 85 different penalty terms).
The second result will be 3 png files containing plots relating cv_R^2 to terminal node count for models built under the different penalty terms. 

# 3. Performance of Other Algorithms (randomForest and CART/rpart)

- Goal: Evaluate the performance of randomForest and CART/rpart on a user-chosen dataset using many different random seeds.

- Packages/libraries required: hal9001 (R Library), nnls (R Library), SuperLearner (R Library), randomForest (R Library), rpart (R Library).

- Instructions: To reproduce the non-HART results from the Table 1 of the paper submission, run the following commands at the command line from the current directory:

```
$./eval_other.sh cpu rpart
$./eval_other.sh cpu randomForest

$./eval_other.sh fev rpart
$./eval_other.sh fev randomForest

$./eval_other.sh mussels rpart
$./eval_other.sh mussels randomForest
```

The result will be 6 csv files: cpu_rpart_r2s.csv, cpu_randomForest_r2s.csv, fev_rpart_r2s.csv, and so on. Each will contain cv-R^2 values corresponding to each seed the given model was built under. Seeds are automatically read in from the corresponding csv file in the ./seeds/ directory. The seeds in those files are the seeds we used to generate the results in the paper submission. The final output can be analyzed for mean and sd R^2 (as in the paper).
