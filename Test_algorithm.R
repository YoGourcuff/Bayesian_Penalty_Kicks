rm(list = ls())
setwd("/Users/Ludo/Documents/Documents importants/2018 - 2020 - Polimi/Bayesian Statistics/Projet")
library(pgdraw)
library(MASS)
library(matlib)
r <- 9
n <- 1000
Id <- diag(r)
X <- mvrnorm(n,matrix(0,r,1),Id)
X <- cbind(matrix(1,nrow(X),1),X)
Y_real <- matrix(0,n,1)
N<-matrix(1,n,1) #Nb of tries for each Yi (=1 for each i in our case)
kappa <- Y_real-0.5*N

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
  if (p(psi(X,i,beta_true))>threshold){
    return(1)
  }
  else {return(0)}
}

intercept <- 0
#Construction of the prior on Beta = b_sample
#Fouskakis, Ntzoufras, and Draper (2009) recommend b = 0 and g = 4 for
#logistic regression based on unit information considerations. 

b0 <- matrix(0,r,1)
b0 <- rbind(intercept,b0)

#β ∼ Np(b, g*r*t(X)%*%X) according to Zellner's prior
g <- 4
B0 <- g*r*inv(t(X)%*%X)

B0 <- diag(r+1)
invB0 <- inv(B0)
productinvB0b0 <- invB0%*%b0 #That will be useful further along

beta_true <- mvrnorm(n=1, mu=b0, Sigma = B0)#The beta we will use as a base
b <- beta_true

for (i in seq(1,n)){
  Y_real[i] <- rbinom(1,1,p(psi(X,i,beta_true)))
}

W_given_b <- matrix(0,n,1)#Variable initialization

iter <- 1000 #Nb of iterations
for (k in seq(1,iter)){
  for (i in seq(1, n)){
    W_given_b[i] <- pgdraw(N[i],psi(X,i,beta_true))
  }
  V <- V_omega(W_given_b)
  m <- m_omega(V)
  beta_true <- mvrnorm(n=1,m,V)
}

Y_predicted <- matrix(0,n,1)
proba_predicted <- matrix(0,n,1)
for (l in seq(1,length(Y_predicted))){
  proba_predicted[l] <- p(psi(X,l,b))
  Y_predicted[l] <- proba_predicted[l]>0.5
}#Prediction according to the logistic regression

proba_predicted
tab <- table(Y_predicted, Y_real)
tab#Comparison of the prediction and actual values
accuracy <- tr(tab)/sum(tab)
accuracy#Percentage of well-predicted penalty kicks
