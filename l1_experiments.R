###########################################
## A script to loadd data, fit HAL,      ##
## and prepare needed files for HART.Py  ##
## for each penalization term in the fit ##
##                                       ##
## REQUIRES: Assignment of df_name       ##
###########################################

#load packages
library(hal9001, quietly = TRUE)

#a function to create files needed for HART in python
#for every l1 penalty term in a hal fit
for_py_l1 <- function(hal_fit, df, df_name, i){
  
  '
  This function takes a hal fit object, the dataframe it was built with,
  and an index corresponding to the desired l1 penalty. HAL automatically tries
  100 l1 penalty terms, but we restrict to the first 85 model fits.


  Nothing is returned. The dataframe containing the information of the specified
  hal fit that can be used to build a HART in python is written to csv. The file
  format is df_name_l1_i.csv

  SEE ALL CAPS COMMENTS BELOW FOR INSTRUCTIONS IF RUNNING THIS SCRIPT MANUALLY
  '
  
  #set the degree for hal
  deg <- ncol(df) - 1
  
  #get the basis mat
  basis_mat <- Reduce(rbind, hal_fit$basis_list)[as.numeric(names(hal_fit$copy_map)),]
  
  #get the lambdas used by hal
  l_vec <- hal_fit$glmnet_lasso$lambda
  
  #make fit matrix
  fit_mat <- cbind(basis_mat, hal_fit$glmnet_lasso$beta[,i])
  
  
  #cast to data frame
  fit_df <- data.frame(fit_mat)

  
  #fcn to add a column with original variable names
  get_names <- function(row){
    return(names(df[,-1])[unlist(row[1])])
  }
  
  #add var names as column
  fit_df$var_name <- apply(fit_df, MARGIN = 1, FUN = get_names)
  
  #cast to character
  fit_df <- data.frame(apply(X = fit_df, MARGIN = 2, FUN = as.character))
  
  #rename the last col to "coeffs"
  names(fit_df)[3] <- "coeffs"
  
  #get the names of the output files
  outfile = paste0('./data/', df_name, "_l1_", i,".csv")

  #write the final df to csv
  write.csv(x = fit_df, file = outfile)
  
  
}

#fit hal and write the files for each l1 penalty term
write_hal_files_all_l1 <- function(df, df_name, seed){
  
  #set the random seed
  set.seed(seed) #seed <- 123 in paper
  
  #fit hal
  hal_fit <- fit_hal(Y = df[,1], X = df[,-1],
                     fit_type = "glmnet",
                     n_folds = 5,  
                     yolo = FALSE, 
                     max_degree = ncol(df) - 1)
  
  
  #write the cv-r2 values to csv
  r2_vec <- 1 - (hal_fit$hal_lasso$cvm[1:3] / var(df[,1]))
  r2 <- data.frame(r2_vec)
  names(r2) <- c('r2')
  write.csv(x = r2, file =paste0('./data/', df_name, '_l1_r2s.csv'))
  
  #call the for_py_l1 function 85 times
  ## IF YOU WISH TO CHANGE THE NUMBER OF FITS, DO SO IN THE RANGE BELOW ##
  ## THEN YOU MUST NOTE THE NUMBER OF FITS AND PASS IT TO THE num_fits VARIABLE ##
  ## IN HART.PY ##
  for(i in 1:85){
    for_py_l1(hal_fit = hal_fit, df = df, df_name = df_name, i)
  }
  
}


#get the dataset name from the command line
df_name <- commandArgs(trailingOnly = TRUE)
### IF RUNNING THIS SCRIPT MANUALLY, COMMENT LINE ABOVE AND UNCOMMENT ONE LINE BELOW ###
#df_name <- 'cpu'
#df_name <- 'mussels'
#df_name <- 'fev'

#import the seed
seed_df <- read.csv(paste0('./seeds/l1_seed.csv'))
seed <- seed_df$seeds[1]
### IF YOU WISH TO SPECIFY YOUR OWN SEEDS, COMMENT 2 LINES ABOVE AND ASSIGN seed YOURSELF ##


#import the data
df <- read.csv(paste0('./data/',df_name,'.csv'))
#call the function to get all the files written to csv
write_hal_files_all_l1(df = df, df_name = df_name, seed = seed)
#also write the feature set as a separate csv file
write.csv(x = df[,-1], file = paste('./data/', df_name,'_features.csv', sep=''))

