find_var <- function(mat){
  var_vector <- apply(mat, 2, var, na.rm=TRUE)
  var_max <- max(var_vector, na.rm = TRUE)
  var_min <- min(var_vector, na.rm = TRUE)
  v <- (var_vector - var_min)/var_max
  return(v)
}

variance_weight_assign <- function(i, j, v, mat){
  r_i <- scale(mat[i, ],center = T,scale = T)
  r_j <- scale(mat[j, ],center = T,scale = T)
  weight <- sum(v*r_i*r_j)/sum(v)
  return(weight)
}

variance_weight_matrix <- function(mat){
  v <- find_var(mat)
  mat_dim <- dim(mat)[1]
  mat_weight = matrix(1, nrow=mat_dim, ncol=mat_dim)
  for (i in 1:(mat_dim)){
    print(i)
    print(Sys.time())
    for (j in (i+1):mat_dim){
      weight <- variance_weight_assign(i, j, v, mat)
      mat_weight[i, j] <- weight
      mat_weight[j, i] <- weight
    }
  }
  return(mat_weight)
}

train1 <- read.csv("../data/data_matrix/train_1.csv",header = T)
train1 <- train1[,-(1:2)]
train2 <- read.csv("../data/data_matrix/train_2.csv",header = T)
train2 <- train2[,-(1:2)]
var1 <- variance_weight_matrix(train1)
var2 <- variance_weight_matrix(train2)
save(var1,file = "../output/variance_weight_1.RData")
save(var2,file = "../output/variance_weight_2.RData")