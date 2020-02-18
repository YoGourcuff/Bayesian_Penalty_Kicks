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

  #Split into train and test set

  #Construct variables for the Gibbs Sampler
  Y<-data[,1] #Success or not -> target class Y=(Yi)
  N<-matrix(1,length(Y),1) #Nb of tries for each Yi (=1 for each i in our case)
  kappa <- Y-0.5*N
  n <- ncol(data)
  X<-data[,2:n] #Covariates for each Yi => X=(Xij), Xi = (Xi1,...Xi9)
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
  
  iter <- 1500 #Nb of iterations
  for (k in seq(1,iter)){
    for (i in seq(1, nrow(X))){
      W_given_b[i] <- pgdraw(N[i],psi(X,i,b_sample))
    }
    V <- V_omega(W_given_b)
    m <- m_omega(V)
    b_sample <- mvrnorm(n=1,m,V)
  }
  
  
  #Classification part
  penalty <- rbind(c(1.0,1.0,1,1,0.43283582,0.8888889,0.5127980,0.5948435,0.43308739)
                    ,c(1.0,0.0,1,1,0.42857143,0.5555556,1.0000000,0.7642726,0.88884066))
  X_test <-  penalty[2,]
 #Covariates for each Yi => X=(Xij), Xi = (1,Xi1,...Xi9)
  X_test <-c(1,X_test)
  Y_test <- 0 #Real Values, success or fail
  p(X_test%*%b_sample)
  Y_predicted <- p(X_test%*%b_sample)>0.5
  
if (Y_predicted){
  "The penalty is predicted as scored"}
if (!Y_predicted){
  "The penalty is predicted as missed"}


