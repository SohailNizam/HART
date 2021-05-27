##########################################
## A script to laod data, fit HAL,      ##
## and prepare needed files for HART.Py ##
##########################################

#load packages
library(hal9001)

#a function to create files needed for HART in python
for_py <- function(hal_fit, df){
  
  '
  This function takes in a hal fit object and the dataframe
  used to build it, and it creates a dataframe containing all
  of the info needed to construct the corresponding HART in 
  python.
  '
  
  # make basis list a two column matrix
  basis_mat <- Reduce(rbind, hal_fit$basis_list)[as.numeric(names(hal_fit$copy_map)),]
  
  #make fit matrix
  fit_mat <- cbind(basis_mat, (hal_fit$coefs)[-1,])
  
  #cast to data frame
  fit_df <- data.frame(fit_mat)
  
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


### import data, fit hal, get cvr2, create objects for py, write all to csv ###
write_hal_files <- function(df, df_name, seed_vec){
  
  '
  This function takes in a dataframe, the name of the dataframe as a string,
  a vector of seeds with which you would like to fit HAL, and a boolean set to TRUE
  if you would like printed updates each time HAL is fit successfully.

  Dataframe must be in the following format: first column is output variable/labels
                                             remaining columns are features/preduictors

  There is no output. Files are written to the current directory. Those files are:
  1. csv file containing CV-R2 values for each HAL fit. If df_name = df_name 
  The file format will be df_name_hal_r2s.csv.
  2. csv files containing data ready for building HARTs in Python 
  (one file for each seed). If seed = s and df_name = df_name, 
  the file format will be df_name_fit_s.csv

  seed_vec defaults to 1:100 if unspecified (100 HALs will be fit)
  '
  
  #initialize vector for cv r2 values
  r2_vec <- vector(length = length(seed_vec))
  #keep track of which number fit we're on
  num <- 1
  #for each seed specified
  for(i in seed_vec){
    set.seed(i)
    #fit hal
    hal_fit <- fit_hal(Y = df[,1], X = df[,-1],
                           fit_type = "glmnet",
                           n_folds = 5,  
                           yolo = FALSE, 
                           max_degree = ncol(df) - 1) 
    
    #get cv r2
    r2 <- 1 - (min(hal_fit$hal_lasso$cvm)/var(df[,1]))
    #add it to running vector
    r2_vec[i] <- r2
    
    #create python-ready file
    fit_df <- for_py(hal_fit, df)
    #write the file to csv
    write.csv(x = fit_df, file = paste0('./data/', df_name, '_hal_', num,'.csv'))
    
    #increment the fit counter
    num <- num + 1

  }
  
  r2_vec <- data.frame(r2_vec)
  names(r2_vec) <- c('r2')
  #write the r2 vector to csv
  write.csv(x = r2_vec, file = paste0('./data/', df_name, '_hal_r2s.csv'))

} 

#import the seeds, store in vector
seed_df <- read.csv(paste0('./seeds/',df_name,'_seeds.csv'))
seed_vec <- seed_df$seed
#import the data
df <- read.csv(paste0('./data/',df_name,'.csv'))
#write just the features to a separate csv file
write.csv(x = df[,-1], file = paste('./data/', df_name,'_features.csv', sep=''))
#write the csv needed to build HARTs in Python
write_hal_files(df = df, df_name = df_name, seed_vec = seed_vec)


