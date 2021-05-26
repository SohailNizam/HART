#install/load hal
install.packages("hal9001")
install.packages("data.table")
#install.packages("L0Learn")
#install.packages("caret")
install.packages("nnls")
install.packages("SuperLearner")
install.packages("randomForest")
install.packages("rpart.plot")
library(data.table)
library(hal9001)
#library(rpart)
library(rpart.plot)
library(glmnet)
#library(L0Learn)
#library(caret)
library(ggplot2)
library(dplyr)
library(reshape2)
library(SuperLearner)
library(randomForest)


path = '/Users/sohailnizam/Documents/HAL_9001/HAL_data/'

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


#the same as above, but uses the hal fits for every lambda parameter tried
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


#a fcn that writes the necessary files to csv
write_files <- function(fit_df, df, name1, name2){
  
  #get that boiiii ova to python
  write.csv(x = fit_df, file = paste(path, name1,'.csv', sep=''))
  
  #also write the feature set to csv for python
  write.csv(x = df[,-1], file = paste(path, name2,'.csv', sep=''))
}

### import data, fit hal, get cvr2, create objects for py, write to csv ###

evaluate_hal <- function(df, df_name){
  
  r2_vec <- vector(length = 100)
  
  for(i in 1:100){
    set.seed(i)
    hal_fit <- fit_hal(Y = df[,1], X = df[,-1],
                           fit_type = "glmnet",
                           n_folds = 5,  
                           #standardize = FALSE, 
                           yolo = FALSE, 
                           max_degree = ncol(df) - 1) 
    
    r2 <- 1 - (min(hal_fit$hal_lasso$cvm)/var(df[,1]))
    r2_vec[i] <- r2
    
    fit_df <- for_py(hal_fit, df)
    write_files(fit_df, df, paste0(df_name, '_fit_', i), paste0(df_name, '_features'))
    
    print(i)
    
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


#cpu
cpu <- read.csv(paste0(path,"cpu.csv"))
set.seed(123)
hal_fit_cpu <- fit_hal(Y = cpu[,1], X = cpu[,-1],
                       fit_type = "glmnet",
                       n_folds = 5,  
                       #standardize = FALSE, 
                       yolo = FALSE, 
                       max_degree = 6)
cpu_r2 <- 1 - (min(hal_fit_cpu$hal_lasso$cvm)/var(cpu$prp)) #.90
cpu_fit <- for_py(hal_fit_cpu, cpu) #59 nonzero coeffs
write_files(cpu_fit, cpu, 'cpu_fit', 'cpu_features')
for(i in 1:100){
  for_py_lambda(hal_fit_cpu, df = cpu, deg = 6, i)
}
cpu_r2_vec <- 1 - (hal_fit_cpu$hal_lasso$cvm[1:85] / var(cpu$prp))


#mussels
mussels<- read.csv(paste0(path, "mussels.csv"))
set.seed(123)
hal_fit_mussels <- fit_hal(Y = mussels[,1], X = mussels[,-1],
                           fit_type = "glmnet",
                           n_folds = 5,  
                           #standardize = FALSE, 
                           yolo = FALSE, 
                           max_degree = 4)
mussels_r2 <- 1 - (min(hal_fit_mussels$hal_lasso$cvm)/var(mussels$M)) #.79
mussels_fit <- for_py(hal_fit_mussels, mussels) #35 nonzero coeffs
write_files(mussels_fit, mussels, 'mussels_fit', 'mussels_features')
for(i in 1:100){
  for_py_lambda(hal_fit_mussels, df = cpu, deg = 4, i)
}
mussels_r2_vec <- 1 - (hal_fit_mussels$hal_lasso$cvm[1:85] / var(mussels$M))


#fev
fev <- read.csv(paste0(path, "fev.csv"))
set.seed(123)
hal_fit_fev <- fit_hal(Y = fev[,1], X = fev[,-1],
                       fit_type = "glmnet",
                       n_folds = 5,  
                       #standardize = FALSE, 
                       yolo = FALSE, 
                       max_degree = 4)
fev_r2 <- 1 - (min(hal_fit_fev$hal_lasso$cvm)/var(fev$fev)) #.79
fev_fit <- for_py(hal_fit_fev, fev) #51 nonzero coeffs
write_files(fev_fit, fev, 'fev_fit', 'fev_features')
for(i in 1:100){
  for_py_lambda(hal_fit_fev, df = cpu, deg = 4, i)
}
fev_r2_vec <- 1 - (hal_fit_fev$hal_lasso$cvm[1:85] / var(fev$fev))

## Create r2 vs node count figure ##

#import the node counts
node_counts <- read.csv(paste0(path,"node_counts.csv"))
#create df containing node counts and r2 values
node_counts$cpu_r2 <- cpu_r2_vec
node_counts$mussels_r2 <- mussels_r2_vec
node_counts$fev_r2 <- fev_r2_vec
View(node_counts)

#create the figures
font_size <- 15
line_size <- .75

node_counts %>%
  ggplot(aes(cpu,cpu_r2)) + 
  geom_line() + 
  labs(title = "CPU", x = "Node Count", y = expression(paste("CV-", R^2))) +
  coord_cartesian(xlim = c(0, 45000), ylim = c(0, .9)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = font_size),
        text = element_text(size = font_size),
        axis.text = element_text(size = font_size)) + 
  geom_hline(yintercept = .84, linetype = "dotted", color = "blue", size = line_size) + 
  geom_hline(yintercept = .58, linetype = "dashed", color = "green", size = line_size) + 
  geom_hline(yintercept = .87, linetype = "solid", color = "red", size = line_size)

node_counts %>%
  ggplot(aes(mussels,mussels_r2)) + 
  geom_line() + 
  labs(title = "Mussels", x = "Node Count", y = expression(paste("CV-", R^2))) +
  coord_cartesian(xlim = c(0, 7500), ylim = c(0, .85)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = font_size),
        text = element_text(size = font_size),
        axis.text = element_text(size = font_size)) + 
  geom_hline(yintercept = .84, linetype = "dotted", color = "blue", size = line_size) + 
  geom_hline(yintercept = .75, linetype = "dashed", color = "green", size = line_size) + 
  geom_hline(yintercept = .80, linetype = "solid", color = "red", size = line_size)

node_counts %>%
  ggplot(aes(fev,fev_r2)) + 
  geom_line() + 
  labs(title = "FEV", x = "Node Count", y = expression(paste("CV-", R^2))) +
  coord_cartesian(xlim = c(0, 750), ylim = c(0, .80)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = font_size),
        text = element_text(size = font_size),
        axis.text = element_text(size = font_size)) + 
  geom_hline(yintercept = .71, linetype = "dotted", color = "blue", size = line_size) + 
  geom_hline(yintercept = .73, linetype = "dashed", color = "green", size = line_size) + 
  geom_hline(yintercept = .78, linetype = "solid", color = "red", size = line_size)




### Get CART and RF results for each dataset ###

evaluate_algo <- function(algo_name, df){
  
  r2_vec <- vector(length=100)
  node_count_vec <- vector(length = 100)
  
  for(i in 1:100){
    set.seed(i)
    model <- SuperLearner(Y = df[,1], X = df[,-1],
                          SL.library = paste0("SL.", algo_name), 
                          family = gaussian(), cvControl = list(V=5))
    
    r2 <- 1 - (model$cvRisk / var(df[,1]))
    r2_vec[i] <- r2
    
    if(algo_name == "rpart"){

      tn_count <- length(table(model$fitLibrary$SL.rpart_All$object$where))
      node_count_vec[i] <- tn_count
    }
    
    print(i)
    
  }
  
  if(length(node_count_vec) == length(r2_vec)){
    return(data.frame(node_count_vec, r2_vec))
  }
  else{return(r2_vec)}
  
}

#cpu
cpu_rf_r2_vec <- evaluate_algo("randomForest", cpu)
cpu_rf_mean <- mean(cpu_rf_r2_vec) #.84
cpu_rf_sd <- sd(cpu_rf_r2_vec) #.05

cpu_rpart_rslts <- evaluate_algo("rpart", cpu)
cpu_rpart_r2_mean <- mean(cpu_rpart_rslts$r2_vec) #.58
cpu_rpart_r2_sd <-  sd(cpu_rpart_rslts$r2_vec) #.06
cpu_rpart_nc_mean <- mean(cpu_rpart_rslts$node_count_vec) #5
cpu_rpart_nc_sd <- sd(cpu_rpart_rslts$node_count_vec) #0

#mussels
mussels_rf_r2_vec <- evaluate_algo("randomForest", mussels)
mussels_rf_mean <- mean(mussels_rf_r2_vec) #.84
mussels_rf_sd <- sd(mussels_rf_r2_vec) #.01

mussels_rpart_rslts <- evaluate_algo("rpart", mussels)
mussels_rpart_r2_mean <- mean(mussels_rpart_rslts$r2_vec) #.75
mussels_rpart_r2_sd <- sd(mussels_rpart_rslts$r2_vec) #.03
mussels_rpart_nc_mean <- mean(mussels_rpart_rslts$node_count_vec) #5
mussels_rpart_nc_sd <- sd(mussels_rpart_rslts$node_count_vec) #0


#fev
fev_rf_r2_vec <- evaluate_algo("randomForest", fev)
fev_rf_mean <- mean(fev_rf_r2_vec) #.71
fev_rf_sd <- sd(fev_rf_r2_vec) #.004

fev_rpart_rslts <- evaluate_algo("rpart", fev)
fev_rpart_r2_mean <- mean(fev_rpart_rslts$r2_vec) #.73
fev_rpart_r2_sd <-  sd(fev_rpart_rslts$r2_vec) #.01
fev_rpart_nc_mean <- mean(fev_rpart_rslts$node_count_vec) #7
fev_rpart_nc_sd <- sd(fev_rpart_rslts$node_count_vec) #0

