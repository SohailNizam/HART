##########################################
## A script to laod data, evaluate,     ##
## other algorithms (e.g. CART and RF)  ##
##                                      ##
## REQUIRES: Assignment of df_name      ##
##########################################

#load packages
library(hal9001, quietly = TRUE)
library(nnls, quietly = TRUE)
library(SuperLearner, quietly = TRUE)
library(randomForest, quietly = TRUE)
library(rpart, quietly = TRUE)


#libraries for any learners beyond randomForest and rpart must be loaded
#if you wish to evaluate them. They must also be available in 
#the SuperLearner package


#a function to evaluate a chosen algorithm
#in the paper, we use randomForest and rpart
evaluate_algo <- function(algo_name, df, seed_vec){
  
  '
  This function takes in the name of an algorithm, a dataframe to be analyzed,
  a vector of random seeds to be used, and a boolean (TRUE if you want printed updates).
  The algorithm will be fit and evaluated with each seed specified. The default vector is 1:100.

  If using rpart, the output is a two column dataframe. First column contains cv-r2 values 
  for each seed, and second columns contains terminal node counts for each seed. If using anything
  else (e.g. randomForest), the output is a vector containing cv-r2 values for each seed.

  *If you wish to use any algorithms besides rpart and randomForest, they must be available in
   the SuperLearner library, and the appropriate packages must be loaded.
  '
  
  #initialize vector to hold cv-r2 values 
  r2_vec <- vector(length = length(seed_vec))
  #initialize terminal node count vector in case using rpart
  node_count_vec <- vector(length = length(seed_vec))
  
  #for each seed
  for(i in seed_vec){
    set.seed(i)
    
    #build the specified model using the SuperLearner packaage
    model <- SuperLearner(Y = df[,1], X = df[,-1],
                          SL.library = paste0("SL.", algo_name), 
                          family = gaussian(), cvControl = list(V=5))
    
    #get the cv-R2 value
    r2 <- 1 - (model$cvRisk / var(df[,1]))
    #add value to the running vector
    r2_vec[i] <- r2
    
    #if using rpart, add the terminal node count to the running vector
    if(algo_name == "rpart"){
      
      tn_count <- length(table(model$fitLibrary$SL.rpart_All$object$where))
      node_count_vec[i] <- tn_count
    }
    
  }
  
  #if using rpart, write both r2 and nod_count vectors in a df
  if(algo_name == "rpart"){
    
    #create df out of node_counts and r2_vec
    r2 <- data.frame(node_count_vec, r2_vec)
    names(r2) <- c("tn_counts", "r2")
  
  }
  #if using anything else, write just the r2 vector
  else{
    
    #create df out of just r2_results
    r2 <- data.frame(r2_vec)
    names(r2) <- c("r2")
    
   }
  
  
  #write the results to csv
  write.csv(x = r2, 
            file = paste0('./results/', df_name, '_', algo_name, '_r2s.csv'))
  return(r2)
  
}

#get the dataset name from the command line
df_name <- commandArgs(trailingOnly = TRUE)[1]
#get the algo name from the command line
algo_name <- commandArgs(trailingOnly = TRUE)[2]
seed_vec <- 1:2
#import the seeds, store in vector
#seed_df <- read.csv(paste0('./seeds/',df_name,'_seeds.csv'))
#seed_vec <- seed_df$seed
#import the data
df <- read.csv(paste0('./data/', df_name, '.csv'))
#evaluate the algorithm and write the results to csv
rslts <- evaluate_algo(algo_name, df, seed_vec = seed_vec)


