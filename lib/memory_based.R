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
  ### Input: mat (training data)
  ###        method ("pearson" or "spearman")
  ### Output: Similarity matrix by each method
  if(method == "pearson") {
    return(cor(t(mat), method = "pearson")) 
  } else {
    return(cor(t(mat), method = "spearman"))
  }
}

###############################################################################
############################## Variance Weighting #############################
###############################################################################

variance_weight <- function(matrix,method){
  
  matrix[is.na(matrix)] = 0
  matrix.t = t(matrix)
  var = apply(matrix,1,var)
  min=min(var)
  max=max(var)
  weight<-function(vector){
    vector * (var(vector)-min/max)
  }
  matrix=scale(matrix)
  w = cor(apply(matrix,1,weight),use="everything",method=method)
  
  return(w)
}

###############################################################################
########################### Selecting Neighborhoods ###########################
###############################################################################

select_neighbors <- function(weight, thresholding = 0.5){
  ### Weight Thresholding
  ### Input: weight (similarity weight matrix)
  ###        thresholding (min-abs-corr)
  ### Output: A list of neighbors's index for each user
  list_neighbors <- list()
  n <- nrow(weight)
  for(i in 1:n){
    eachrow <- weight[i,]
    index <- which((abs(eachrow) > thresholding)&(eachrow != 1))
    list_neighbors[[i]] <- index
  }
  return(list_neighbors)
}

#select_neighbors2 <- function(weight, num = 20){
  ### Weight Thresholding
  ### Input: weight (similarity weight matrix)
  ###        thresholding (min-abs-corr)
  ### Output: A list of neighbors's index for each user
#  list_neighbors <- list()
#  n <- nrow(weight)
#  for(i in 1:n){
#    eachrow <- weight[i,]
#    index <- order(abs(eachrow),decreasing = T)[2:num+1]
#    list_neighbors[[i]] <- index
#  }  return(list_neighbors)
#}

###############################################################################
##################################### SimRank #################################
###############################################################################



###############################################################################
########################### Producing a Prediction ############################
###############################################################################

prediction_1 <- function(testset = test_1, trainset = train_1, weight, list_neighbors){
  ### Rating Normalization: Deviation from mean
  ### Input: testset (MS test dataset)
  ###        trainset (MS train dataset)
  ###        weight (similarity weight matrix)
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
  ### Input: testset (Movie test dataset)
  ###        trainset (Movie train dataset)
  ###        weight (similarity weight matrix)
  ###        list_neighbors (A list of neighbors's index for each user)
  ### Output: A prediction matrix
  test_logic <- is.na(testset)
  n <- nrow(testset)
  p <- ncol(testset)
  pred_mat <- matrix(0, ncol = p, nrow = n)
  trainset[trainset==0] <- NA
  avg_rate <- apply(trainset, 1, mean,na.rm = T)
  for(i in 1:nrow(trainset)){
    colnames_not_na <- colnames(testset)[which(!is.na(testset[i,]))] # 
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
