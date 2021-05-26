##########################################
## A script to laod data, fit HAL,      ##
## and prepare needed files for HART.Py ##
##########################################


#load packages
library(hal9001)
library(data.table)
library(nnls)
library(SuperLearner)
library(randomForest)
library(rpart)
library(glmnet)
library(ggplot2)
library(dplyr)
library(reshape2)

#the same as above, but uses the hal fits for every l1 penalty tried
for_py_lambda <- function(hal_fit, df, deg, i){
  
  #hal_fit = existing hal_fit object
  #df = dataframe used to fit hal
  #deg = max degree hal was fit with
  #l_vec = vector of lambdas used as input to hal
  #i = index of the lambda youre interested in in l_vec
  
  
  #get the basis mat
  basis_mat <- Reduce(rbind, hal_fit$basis_list)[as.numeric(names(hal_fit$copy_map)),]
  
  #get the lambdas used by hal
  l_vec <- hal_fit$glmnet_lasso$lambda
  
  #make fit matrix
  fit_mat <- cbind(basis_mat, hal_fit$glmnet_lasso$beta[,i])
  
  print("made fit_mat")
  
  #cast to data frame
  fit_df <- data.frame(fit_mat)
  
  print("made fit_df")
  
  
  #fcn to add a column with original variable names
  get_names <- function(row){
    return(names(df[,-1])[unlist(row[1])])
  }
  
  #add var names as column
  fit_df$var_name <- apply(fit_df, MARGIN = 1, FUN = get_names)
  
  # print("added names col")
  
  #cast to character
  fit_df <- data.frame(apply(X = fit_df, MARGIN = 2, FUN = as.character))
  
  # print("got it to chr")
  #rename the last col to "coeffs"
  # names(fit_df)[3] <- "coeffs"
  
  print("changed the names")
  #get the name of the dataframe as a string
  df_name <- deparse(substitute(df))
  
  
  #get the names of the output files
  outfile1 = paste0(path, df_name, "_l", i,".csv")
  #outfile2 = paste("/Users/sohailnizam/Desktop/", df_name, "_features.csv", sep = "")
  
  print("got the file name")
  print(paste0(path, df_name, "_l", i,".csv"))
  
  
  #get that boiiii ova to python
  write.csv(x = fit_df, file = outfile1)
  
  #also write the feature set to csv for python
  #write.csv(x = df[,-1], file = outfile2)
  print(paste("lambda =", hal_fit$glmnet_lasso$lambda[i]))
  print("done")
}
