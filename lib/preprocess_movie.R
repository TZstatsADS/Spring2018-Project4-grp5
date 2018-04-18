function(data){
  library("reshape2")
  data<- data[,-1]
  new<- dcast(data, User~...)
  return(new)
}
