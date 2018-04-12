
####################################
## EM algorithm for cluster model ##
####################################

# date: 2018/4/10
# author: wesley tao

# step 1 take initial guess for all the parameters
library(MCMCpack)
library(plyr)
setwd("C:/Users/Wesle/Documents/GitHub/Spring2018-Project4-grp5/lib")
train_1<-read.csv("../data/data_matrix/train_1.csv")
test_1<-read.csv("../data/data_matrix/test_1.csv")

head(colnames(train_1))

# step 1 init the parameter
num.C<- 4# number of the class
M    <-ncol(train_1)-2  # number of the movies
N    <-nrow(train_1) #number of the users
M
M==length(colnames(train_1)[3:ncol(train_1)])


mu         <-rdirichlet(1, rep(1,num.C))# store the vector of probability for a user belonging to a class c
gamma      <-array(rdirichlet(M*num.C,c(1,1)),dim=c(M,num.C,2),
                        dimnames = list(website=colnames(train_1)[3:ncol(train_1)], # M
                                        class=seq(1:num.C),
                                        visited=c("yes","no")# 2
                                        )) #(score,movie,class) store the probability

# step 2 Expectation 
## compute the responsibilities for each user i 
# fix a user 

fi     <-matrix(rep(NA,num.C*N),ncol=num.C,nrow=N)
pi     <-matrix(rep(NA,num.C*N),ncol=num.C,nrow=N)

for(this.user in 1:N){
  index_1<-train_1[this.user,3:ncol(train_1)]==1
  index_0<-train_1[this.user,3:ncol(train_1)]==0
  #get value of fi
  for(this.class in 1:num.C){
    fi[this.user,this.class]<-prod(gamma[index_1,this.class,"yes"])*prod(gamma[index_0,this.class,"no"])
  }
  denominator<- mu%*%fi[this.user,]
  # get value of pi
  for(this.class in 1:num.C){
    pi[this.user,this.class]<-mu[this.class]*fi[this.user,this.class]/denominator
  }
}
# dim(gamma)
# 
# # implementation 2
# prod_con<-function()
# # fix a class  
# aaply(gamma,.(2),sum)
# gamma[,2,]
gamma[,2,]
# step 3: Maximization
new_mu    <-apply(pi,2,mean)

pi[,1] 
train_1[1,3:ncol(train_1)]
    
sum(new_mu)
pi[1,1]<-1
pi

temp_1     <-t(as.matrix(train_1[,3:ncol(train_1)]==1)) %*% pi
temp_1
row_sum_pi <-apply(pi,2,sum)
df_divide  <-matrix(rep(row_sum_pi,M),ncol=4,nrow=M,byrow = T)

new_gamma_yes<-temp_1/df_divide

temp_2<-t(as.matrix(train_1[,3:ncol(train_1)]==0)) %*% pi
new_gamma_no<-temp_2/df_divide


gamma[,,"yes"] <-new_gamma_yes
gamma[,,"no"]  <-new_gamma_no

# step 4 iterate until convergence




