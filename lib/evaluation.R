###############################################################################
########################### Evaluation Measure: MAE ###########################
###############################################################################

mae <- function(pred_mat, testset){
  ### MAE 
  ### Input: pred_mat (predicted matrix calculated before)
  ###        testset (true test dataset)
  ### Output: Mean absolute error of predicted value
  mae_value <- mean(abs(pred_mat - testset), na.rm = T)
  return(mae_value)
}

###############################################################################
########################## Evaluation Measure: ROC-4 ##########################
###############################################################################

roc <- function(roc_value = 4, pred_mat, testset){
  ### ROC-4 
  ### Input: pred_mat (predicted matrix calculated before)
  ###        testset (true test dataset)
  ### Output: The sensitivity of the test
  roc_mat <- matrix(roc_value, nrow = nrow(pred_mat), ncol = ncol(pred_mat))
  roc_sum <- sum((pred_mat >= roc_mat) == (testset >= roc_mat), na.rm=T)
  n <- sum(!is.na(pred_mat))
  return(roc_sum/n)
}

###############################################################################
####################### Evaluation Measure: Rank Score ########################
###############################################################################

first <- function(x){return(rank(x,ties.method = "first"))}

rank_score <- function(pred_mat, testset){
  ### Rank Score
  ### Input: pred_mat (predicted matrix calculated before)
  ###        testset (true test dataset)
  ### Output: The rank score of predicted value
  d <- 0.02
  rank_pred <- ncol(pred_mat)+1-t(apply(pred_mat,1,first))
  rank_test <- ncol(testset)+1-t(apply(testset,1,first))
  logic <- ifelse(testset - d > 0,testset - d,0)
  r_a <- apply(1/(2^((rank_pred-1)/4)) * logic,1,sum)
  r_a_max <- apply(1/(2^((rank_test-1)/4)) * logic,1,sum)
  r <- 100*sum(r_a)/sum(r_a_max)
  return(r)
}


