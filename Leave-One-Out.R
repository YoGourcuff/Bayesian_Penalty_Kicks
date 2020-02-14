rm(list = ls())
setwd("/Users/Ludo/Documents/Documents importants/2018 - 2020 - Polimi/Bayesian Statistics/Projet")
library(pgdraw)
library(MASS)
library(matlib)
load("Final_dataset.Rda")
data <- matrix(as.numeric(unlist(t(final_dataset))), ncol = 10, byrow = TRUE)

#Functions that we will need further along
p <- function(x){
  1./(1+exp(-x))
}#Logistic function

invp <- function(x){
  -log(1/x-1)
}#To get the intercept

psi <- function(X,i,b){
  X[i,]%*%b
}

V_omega <- function(W){
  Omega <- matrix(0,length(W),length(W))
  for (t in seq(1,length(W))){
    Omega[t,t] <- W[t]
  }
  inv(t(X)%*%Omega%*%X + invB0)
} # Covariance Matrix used to sample from the multivariate Gaussian

m_omega <- function(V){
  V%*%(t(X)%*%kappa+productinvB0b0)
} # Mean Vector used to sample from the multivariate gaussian

#Classification Test
Classif <- function(i,threshold){
  if (p(psi(X_test,i,b_sample))>threshold){
    return(1)
  }
  else {return(0)}
}

#Data Preparation
Count <- nrow(data)
vec_acc <- matrix(0,Count,1)
for (q in seq(1,Count)){
#Split into train and test set
train_set <- -q
test_set <- q

#Construct variables for the Gibbs Sampler
Y<-data[train_set,1] #Success or not -> target class Y=(Yi)
N<-matrix(1,length(Y),1) #Nb of tries for each Yi (=1 for each i in our case)
kappa <- Y-0.5*N
n <- ncol(data)
X<-data[train_set,2:n] #Covariates for each Yi => X=(Xij), Xi = (Xi1,...Xi9)
r <- ncol(X)
X <-cbind(matrix(1,nrow(X),1),X)

#Construction of the prior on Beta = b_sample
#Fouskakis, Ntzoufras, and Draper (2009) recommend b = 0 and g = 4 for
#logistic regression based on unit information considerations. 

b0 <- matrix(0,r,1)
intercept <- invp(0.75)
b0 <- rbind(intercept,b0)

#β ∼ Np(b0, g*r*t(X)%*%X) according to Zellner's prior
g <- 4
B0 <- g*r*inv(t(X)%*%X)

invB0 <- inv(B0)
productinvB0b0 <- invB0%*%b0 #That will be useful further along

b_sample <- mvrnorm(n=1, mu=b0, Sigma = B0) #Gaussian prior on b or Beta
W_given_b <- matrix(0,nrow(X),1)#Variable initialization

#Gibbs Sampling Algorithm

iter <- 350 #Nb of iterations
for (k in seq(1,iter)){
  for (i in seq(1, nrow(X))){
    W_given_b[i] <- pgdraw(N[i],psi(X,i,b_sample))
  }
  V <- V_omega(W_given_b)
  m <- m_omega(V)
  b_sample <- mvrnorm(n=1,m,V)
}


#Classification part

X_test<-data[test_set,2:n] #Covariates for each Yi => X=(Xij), Xi = (1,Xi1,...Xi9)
X_test <-c(1,X_test)
Y_test <-data[test_set,1] #Real Values, success or fail

Y_predicted <- p(X_test%*%b_sample)>0.5
vec_acc[q] <- (Y_test == Y_predicted)
}

vec_acc
mean(vec_acc)

x <- c(100,200,250,350,500,750,1000,1250,1500,1750,2000)
y <- c(0.6698113,0.6792453,0.6981132,0.7169811,0.7075472,0.7169811,0.7264151,0.7345832,0.7455527,0.7418931,0.7475931) 
#Values gathered by hand running the algorithms for different values of T = iter
plot(x, y,main = "LOO cross-validation", xlab = "Number of iterations",ylab="Mean performance")
