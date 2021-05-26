##########################################
## A script to laod data, fit HAL,      ##
## and prepare needed files for HART.Py ##
##########################################


#install/load hal
install.packages("hal9001")
install.packages("data.table")
install.packages("nnls")
install.packages("SuperLearner")
install.packages("randomForest")
install.packages("rpart")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("dplyr")
library(data.table)
library(hal9001)
library(rpart)
library(glmnet)
library(ggplot2)
library(dplyr)
library(reshape2)
library(SuperLearner)
library(randomForest)



#a function to create files needed for HART in python
for_py <- function(hal_fit, df){
  # make basis list a two column matrix
  basis_mat <- Reduce(rbind, hal_fit$basis_list)[as.numeric(names(hal_fit$copy_map)),]
  
  #make fit matrix
  fit_mat <- cbind(basis_mat, (hal_fit$coefs)[-1,])
  
  #cast to data frame
  fit_df <- data.frame(fit_mat)
  
  #print number of nonzero coeffs
  #print(paste(sum(fit_df$V3 != 0), "nonzero coeffs"))
  
  #add a column with original variable names
  get_names <- function(row){
    return(names(df[,-1])[unlist(row[1])])
  }
  
  fit_df$var_name <- apply(fit_df, MARGIN = 1, FUN = get_names)
  
  #cast to character
  fit_df <- data.frame(apply(X = fit_df, MARGIN = 2, FUN = as.character))
  
  #rename the last col to "coeffs"
  names(fit_df)[3] <- "coeffs"
  
  return(fit_df)
}


#the same as above, but uses the hal fits for every l1 penalty tried
#MOVE TO NEW SCRIPT
#for_py_lambda <- function(hal_fit, df, deg, i){
  
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


#a fcn that writes the necessary files to csv
write_files <- function(fit_df, df, name1, name2){
  
  #get that boiiii ova to python
  write.csv(x = fit_df, file = paste('./', name1,'.csv', sep=''))
  
  #also write the feature set to csv for python
  write.csv(x = df[,-1], file = paste('./', name2,'.csv', sep=''))
}

### import data, fit hal, get cvr2, create objects for py, write to csv ###

evaluate_hal <- function(df, df_name, num_fits){
  
  r2_vec <- vector(length = num_fits)
  
  for(i in 1:num_fits){
    set.seed(i)
    hal_fit <- fit_hal(Y = df[,1], X = df[,-1],
                           fit_type = "glmnet",
                           n_folds = 5,  
                           yolo = FALSE, 
                           max_degree = ncol(df) - 1) 
    
    r2 <- 1 - (min(hal_fit$hal_lasso$cvm)/var(df[,1]))
    r2_vec[i] <- r2
    
    fit_df <- for_py(hal_fit, df)
    write_files(fit_df, df, paste0(df_name, '_fit_', i), paste0(df_name, '_features'))
    
  }
  
  return(r2_vec)
} 

#cpu
cpu <- read.csv(paste0(path,"cpu.csv"))
cpu_r2_vec <- evaluate_hal(cpu, "cpu")
cpu_hal_r2_mean <- mean(cpu_r2_vec)
cpu_hal_r2_sd <- sd(cpu_r2_vec)

#mussels
mussels <- read.csv(paste0(path,"mussels.csv"))
mussels_r2_vec <- evaluate_hal(mussels, "mussels")
mussels_hal_r2_mean <- mean(mussels_r2_vec)
mussels_hal_r2_sd <- sd(mussels_r2_vec)

#fev
fev <- read.csv(paste0(path,"fev.csv"))
fev_r2_vec <- evaluate_hal(fev, "fev")
fev_hal_r2_mean <- mean(fev_r2_vec)
fev_hal_r2_sd <- sd(fev_r2_vec)



