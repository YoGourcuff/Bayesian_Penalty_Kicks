rm(list = ls())
setwd("/Users/Ludo/Documents/Documents importants/2018 - 2020 - Polimi/Bayesian Statistics/Projet")
library(pgdraw)
library(MASS)
library(matlib)

load("Final_dataset.Rda")

#Function that we will need further along
p <- function(x){
  1./(1+exp(-x))
}#Logistic function

invp <- function(x){
  -log(1/x-1)
}#To get the intercept

psi <- function(X,i,b){
  X[i,]%*%b+intercept
}

Classif <- function(i,threshold){
  if (p(psi(X_test,i,b_sample))>threshold){
    return(1)
  }
  else {return(0)}
}#Classification test


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


#Data Preprocessing

#Split into train and test set
values <- runif(nrow(final_dataset)) 
train_set <- (values<0.75)
test_set <- (values>=0.75)

#Construct variables for the Gibbs Sampler
Y<-final_dataset[train_set,]$isGoal #Success or not -> target class Y=(Yi)
N<-matrix(1,length(Y),1) #Nb of tries for each Yi (=1 for each i in our case)
kappa <- Y-0.5*N
n<-ncol(final_dataset)
X<-final_dataset[train_set,2:n] #Covariates for each Yi => X=(Xij), Xi = (Xi1,...Xi9)
r <- ncol(X)
X <- matrix(as.numeric(unlist(t(X))), ncol = r, byrow = TRUE) #Turn X into a matrix of numerical values

#Initalization Section
b0 <- matrix(0,r,1)#Values of the Gaussian prior, mean vector b0 and covariance matrix B0
B0 <- 0.1*diag(r) #0.1 because ?
invB0 <- inv(B0)
productinvB0b0 <- invB0%*%b0
intercept <- invp(0.75) #To get 75% chances to score a penalty kick without the covariates
b_sample <- mvrnorm(n=1, mu=b0, Sigma = B0) #Gaussian prior on b or Beta

W_given_b <- matrix(1,nrow(X),1)

#Gibbs Sampling Algorithm Section

iter <- 10000 #Nb of iterations
for (k in seq(1,iter)){
  for (i in seq(1, nrow(X))){
    W_given_b[i] <- pgdraw(N[i],psi(X,i,b_sample))
  }
  V <- V_omega(W_given_b)
  m <- m_omega(V)
  b_sample <- mvrnorm(n=1,m,V)
}

#Classification Section

X_test<-final_dataset[test_set,2:n] #Covariates for each Yi => X=(Xij), Xi = (Xi1,...Xi9)
X_test <- matrix(as.numeric(unlist(t(X_test))), ncol = r, byrow = TRUE) #Turn X_test into a matrix of numerical values
Y_test <-final_dataset[test_set,]$isGoal #Real Values, success or fail

Y_predicted <- matrix(0,nrow(X_test),1)
proba_predicted <- matrix(0,nrow(X_test),1)
for (l in seq(1,length(Y_predicted))){
  proba_predicted[l] <- p(psi(X_test,l,b_sample))
  Y_predicted[l] <- Classif(l,0.75)# if >0.5 classified as scored. Intercept of 0.75 so that 75% of penalty kicks are scored
}#Prediction according to the logistic regression

proba_predicted
tab <- table(Y_predicted, Y_test)
tab#Comparison of the prediction and actual values
accuracy <- tr(tab)/sum(tab)
accuracy#Percentage of well-predicted penalty kicks
