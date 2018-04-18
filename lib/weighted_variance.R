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

