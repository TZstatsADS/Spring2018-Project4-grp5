###############################################################################
########################### Evaluation Measure: MAE ###########################
###############################################################################

mae <- function(pred_mat, testset){
  ### MAE 
  ### Input: pred_mat (predicted matrix calculated before)
  ###        testset (processed test dataset)
  ### Output: Mean absolute error of predicted value
  
  mae_value <- mean(abs(pred_mat - testset), na.rm = T)
  return(mae_value)
}

###############################################################################
####################### Evaluation Measure: Rank Score ########################
###############################################################################

first <- function(x){return(rank(x,ties.method = "first"))} # first occurrence wins

rank_score <- function(pred_mat, testset){
  ### Rank Score
  ### Input: pred_mat (predicted matrix calculated before)
  ###        testset (processed test dataset)
  ### Output: The final score for an experiment over a set of active users in the test set
  
  d <- 0.02 # difference between the vote and the default
  rank_pred <- ncol(pred_mat)+1-t(apply(pred_mat,1,first))
  rank_test <- ncol(testset)+1-t(apply(testset,1,first))
  logic <- ifelse(testset - d > 0,testset - d,0)
  r_a <- apply(1/(2^((rank_pred-1)/4)) * logic,1,sum)
  r_a_max <- apply(1/(2^((rank_test-1)/4)) * logic,1,sum)
  r <- 100*sum(r_a)/sum(r_a_max)
  return(r)
}


