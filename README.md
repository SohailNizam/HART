#  Highly Adaptive Regression Trees

Highly Adaptive Regression Trees (HART) is a method for mapping Highly Adaptive Lasso (HAL) model fits to decision trees. The files in this repository allow for reproduction of results shown in our paper submitted to NeurIPS 2021 as well as general experimentation with HAL and HART. Code was written in R version 4.0.3 (distributed under the terms of the GNU General Public License version 3) and Python version 3.7 (distributed under the terms of the PSF license agreement). There are 3 main things to explore/reproduce in this code. Below are descriptions of these tasks and how to perform them using the code and data stored in this repository. Before beginning, download the entire repository, preserve the file structure, and set this as your working directory.

# 0. Code description

- HAL.R takes the name of a dataset (cpu, fev, or mussels), builds HAL models for that dataset under many random seeds, calculates the cv-R^2 value for each model, and writes csv files to the ./data/ directory containing the information that HART.py needs to build HARTs for each HAL model. HAL.R automatically uses the 


# 1. HART Size and Performance (Table 1 from paper submission)

- Goal: Evaluate the size (in terms of terminal node and unique terminal node counts) and performance (in terms of cross-validated R^2) of HARTs mapped from HAL model fits built using many random seeds on a user-chosen dataset.

- Packages/libraries required: hal9001 (R Library), anytree (Python Library), pandas (Python Library), random (Python Library), sys (Python Library). See requirements.txt for instructions on installing these libraries.

- Instructions: To reproduce results from the Table 1 of the paper submission, run the following commands at the command line from the current directory:

$./eval_hart.sh 0 cpu 10
$./eval_hart.sh 0 fev 100
$./eval_hart.sh 0 mussels 100

The result will be 3 csv files, each with 3 columns: R^2, Terminal Node Count, Unique Terminal Node count. The number of rows will correspond to the third argument in each command above (equal to the number of random seeds used to fit HAL for each dataset). These files can be analyzed for mean and sd of terminal node count,  unique terminal node count, and R^2 value. 

# 2. HART Size and Performance With Heavy L1 Penalization

- Goal:
- Packages/libraries required:
- Instructions:

# 3. Performance of Other Algorithms (randomForest and CART/rpart)

- Goal:
- Packages/libraries required:
- Instructions:

