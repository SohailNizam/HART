##########################################
## A script to laod data, evaluate,     ##
## other algorithms (e.g. CART and RF)  ##
##########################################

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

