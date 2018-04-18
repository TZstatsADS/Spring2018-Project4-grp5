# cluster
library(MCMCpack)
library(plyr)
cluster<-function(train_1,test_1){
# step 1 init the parameter
num.C<- 3# number of the class
M    <-ncol(train_1)-2  # number of the movies
N    <-nrow(train_1) #number of the users

M==length(colnames(train_1)[3:ncol(train_1)])


mu         <-rdirichlet(1, rep(1,num.C))# store the vector of probability for a user belonging to a class c
gamma      <-array(rdirichlet(M*num.C,c(1,1)),dim=c(M,num.C,2),
                   dimnames = list(website=colnames(train_1)[3:ncol(train_1)], # M
                                   class=seq(1:num.C),
                                   visited=c("yes","no")# 2
                   )) 
userhistory<-train_1[,3:ncol(train_1)]


# step 2 Expectation 
## compute the responsibilities for each user i 
# fix a user 
get_expectation<-function(userhistory,mu,gamma){
  # input: 
  ## userhistory N*M matrix store the user behaviour
  ## mu: C*1 vector  
  ## gamma: M*C*2 array
  # output:
  ## pi: N*C matrix
  fi     <-matrix(rep(NA,num.C*N),ncol=num.C,nrow=N)
  pi     <-matrix(rep(NA,num.C*N),ncol=num.C,nrow=N)
  
  for(this.user in 1:N){
    index_1<-userhistory[this.user,]==1
    index_0<-userhistory[this.user,]==0
    #get value of fi
    for(this.class in 1:num.C){
      fi[this.user,this.class]<-prod(gamma[index_1,this.class,"yes"])*prod(gamma[index_0,this.class,"no"])}
    denominator <- mu%*%fi[this.user,]
    # get value of pi
    for(this.class in 1:num.C){
      pi[this.user,this.class]<-mu[this.class]*fi[this.user,this.class]/denominator
    }
  }
  return(pi) 
}


# step 3: Maximization
maximization<-function(myuserhistory,pi,gamma){
  #input: pi,user_behaviour,gamma
  #output: updated gamma, updated mu
  new_mu     <-apply(pi,2,mean)
  
  row_sum_pi <-apply(pi,2,sum)
  df_divide  <-matrix(rep(row_sum_pi,M),ncol=num.C,nrow=M,byrow = T)
  
  temp_1       <-t(as.matrix(myuserhistory==1)) %*% pi
  # print(temp_1)
  new_gamma_yes<-temp_1/df_divide
  temp_2       <-t(as.matrix(myuserhistory==0)) %*% pi
  new_gamma_no <-temp_2/df_divide
  
  gamma[,,"yes"] <-new_gamma_yes
  gamma[,,"no"]  <-new_gamma_no
  
  return(list(mu=new_mu,gamma=gamma))
}


# step 4 iteration till convergence
i=0
repeat{
  i =i+1
  pi    <-get_expectation(userhistory,mu,gamma)
  mylist<-maximization(myuserhistory = userhistory,pi,gamma)
  gamma_l1_dev      <-sum(abs(mylist$gamma-gamma))
  mu_l1_dev         <-sum(abs(mylist$mu-mu))
  cat("\niteration ",i,"gamma abs(diff): ", gamma_l1_dev," mu abs(diff): ",mu_l1_dev)
  
  if(gamma_l1_dev<=10){
    break
  }
  
  gamma <-mylist$gamma
  mu    <-mylist$mu
}

# prediction
# fix a user
pred_one_user<-function(this.user,userhistory){
  # fix a user, return its predition
  stopifnot(length(this.user)==1) # one user at a time
  yes_index  <-userhistory[this.user,]==1
  
  
  this.user_class_likeli_yes<-apply(mylist$gamma[yes_index,,"yes"],2,prod)
  
  aligned_matrix_yes<-matrix(rep(this.user_class_likeli_yes,M),ncol=num.C,nrow=M,byrow = T)
  
  this.user.pred_yes<-(aligned_matrix_yes*mylist$gamma[,,"yes"])%*%mylist$mu
  
  denominator_user<-this.user_class_likeli_yes %*% mylist$mu
  prob_yes <-this.user.pred_yes/as.numeric(denominator_user)
  
  return(prob_yes)
}



test_index      <-which(train_1$user %in% test_1$user)
mypred          <-sapply(test_index,pred_one_user,userhistory)
mypred          <-t(mypred)
colnames(mypred)<-colnames(train_1)[3:ncol(train_1)]
colindex<-colnames(mypred) %in% colnames(test_1)
mypred<-mypred[,colindex]

return(mypred)}

# 
# test_1 <- read.csv("../data/data_matrix/test_1.csv", header = T)
# train_1 <- read.csv("../data/data_matrix/train_1.csv", header = T)
# 
# p<-cluster(train_1,test_1)




