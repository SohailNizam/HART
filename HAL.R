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


### import data, fit hal, get cvr2, create objects for py, write to csv ###

write_hal_files <- function(df, df_name, num_fits){
  
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
    write.csv(x = fit_df, 
              file = paste('./', paste0(df_name, '_fit_', i),'.csv', sep=''))
    
  }
  
  write.csv(x = r2_vec, file = paste0('./', df_name, '_r2s.csv'))
  return()
} 


df <- read.csv(paste0('./',df_name,'.csv'))
write.csv(x = df[,-1], file = paste('./', df_name,'_features.csv', sep=''))
write_hal_files(df = df, df_name = df_name, num_fits = num_fits)


