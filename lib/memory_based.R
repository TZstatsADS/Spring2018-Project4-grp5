###############################################################################
############################### Data Processing ###############################
###############################################################################

data_process <- function(df) {
  df <- data.frame(df[,-c(1,2)],row.names = df[,2])
  return(as.matrix(df)) 
}

###############################################################################
############################## Similarity Weight ##############################
###############################################################################

sim_weight <- function(mat, method) {
  ### Similarity Weight Matrix
  ### Input: mat (processed training data)
  ###        method ("pearson" or "spearman")
  ### Output: Similarity matrix by each method
  
  if(method == "pearson") {
    return(cor(t(mat), use = "pairwise.complete.obs", method = "pearson")) 
  } else {
    w <- matrix(NA,nrow=nrow(mat),ncol=nrow(mat))
    for(i in 1:nrow(mat)){
      w[i,] <- apply(mat,1,function(row){
        index <- (!is.na(row))&(!is.na(mat[i,]))
        if(sum(index)==0){
          return(0)
        }else{
          return(cor(mat[i,index],row[index],method='spearman'))
        }
      })
    }
    return(w)
  }
}

###############################################################################
########################### Selecting Neighborhoods ###########################
###############################################################################

select_neighbors <- function(weight, thresholding = 0.5){
  ### Weight Thresholding
  ### Input: weight (calculated weighted matrix)
  ###        thresholding (min-abs-corr)
  ### Output: A list of neighbors's index for each user
  
  list_neighbors <- list()
  n <- nrow(weight)
  for(i in 1:n){
    eachrow <- weight[i,]
    # select index of correlation values which are greater than the default thresholding
    index <- which((abs(eachrow) > thresholding)&(eachrow != 1)) 
    list_neighbors[[i]] <- index
  }
  return(list_neighbors)
}

###############################################################################
########################### Producing a Prediction ############################
###############################################################################

prediction_1 <- function(testset = test_1, trainset = train_1, weight, list_neighbors){
  ### Rating Normalization: Deviation from mean
  ### Input: testset (MS processed test dataset)
  ###        trainset (MS processed train dataset)
  ###        weight (calculated weighted matrix)
  ###        list_neighbors (A list of neighbors's index for each user)
  ### Output: A prediction matrix
  
  n <- nrow(trainset)
  p <- ncol(trainset)
  pred_mat <- matrix(0, ncol = p, nrow = n)
  avg_rate <- apply(trainset, 1, mean, na.rm = T)
  for(i in 1:n){
    neighbor_weight <- weight[i,list_neighbors[[i]]]
    neighbor_rate <- trainset[list_neighbors[[i]], ]
    l <- length(list_neighbors[[i]])
    if(l < 2){
      if(l < 1){
        pred_mat[i,] <- avg_rate[i]
      }
      else{pred_mat[i,] <- avg_rate[i]+
        ((neighbor_rate-avg_rate[list_neighbors[[i]]])*neighbor_weight)/sum(neighbor_weight, na.rm = T)}
    }
    else{
      pred_mat[i,] <- avg_rate[i] + 
        apply((neighbor_rate-avg_rate[list_neighbors[[i]]])*neighbor_weight, 2, sum, na.rm=T) / 
        sum(neighbor_weight, na.rm = T)
    }
    
  }
  colnames(pred_mat) <- colnames(trainset)
  rownames(pred_mat) <- rownames(trainset)
  return(pred_mat[rownames(testset),colnames(testset)])
}

prediction_2 <- function(testset = test_2, trainset = train_2, weight, list_neighbors){
  ### Rating Normalization: Deviation from mean
  ### Input: testset (Movie processed test dataset)
  ###        trainset (Movie processed train dataset)
  ###        weight (calculated weighted matrix)
  ###        list_neighbors (A list of neighbors's index for each user)
  ### Output: A prediction matrix
  
  test_logic <- is.na(testset)
  n <- nrow(testset)
  p <- ncol(testset)
  pred_mat <- matrix(0, ncol = p, nrow = n)
  avg_rate <- apply(trainset, 1, mean,na.rm = T)
  for(i in 1:nrow(trainset)){
    colnames_not_na <- colnames(testset)[which(!is.na(testset[i,]))] 
    neighbor_weight <- weight[i,list_neighbors[[i]]]
    neighbor_rate <- trainset[list_neighbors[[i]],colnames_not_na]
    l <- length(list_neighbors[[i]])
    if( l < 2){
      if(l < 1){
        pred_mat[i,!test_logic[i,]] <- round(avg_rate[i],0)
      }
      else{
        pred_mat[i,!test_logic[i,]] <- round(avg_rate[i]+
                                               (neighbor_rate-avg_rate[list_neighbors[[i]]])*neighbor_weight/
                                               sum(neighbor_weight, na.rm = T),0)
      }
    }
    else{
      pred_mat[i,!test_logic[i,]] <- round(avg_rate[i]+
                                             apply((neighbor_rate-avg_rate[list_neighbors[[i]]])*neighbor_weight, 2, sum, na.rm=T)/
                                             sum(neighbor_weight, na.rm = T),0)
    }
    
  }
  return(pred_mat)
}
