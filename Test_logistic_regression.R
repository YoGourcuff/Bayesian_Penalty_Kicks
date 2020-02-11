library(MASS)
library(rjags)
library(mvtnorm)

r<-6
beta_true<-mvrnorm(1,c(0,0,0,0,0,0),diag(r))
X<-mvrnorm(1000,c(0,0,0,0,0),diag(r-1))

Y<-c()
for(i in 1:1000){
  temp<-beta_true[1] + beta_true[2:6]%*%X[i,]
  p<-exp(temp)/(1+exp(temp))
  Y[i]<-rbinom(1,1,p)
}
n<-length(Y)



##------------------MODEL CREATION-----------------------------------

logistic_model <- "model{

# Likelihood

for(i in 1:n){
Y[i] ~ dbern(q[i])
logit(q[i]) <- beta[1] + beta[2]*X[i,1] + beta[3]*X[i,2] + 
beta[4]*X[i,3] + beta[5]*X[i,4] + beta[6]*X[i,5]
}

#Priors

for(j in 1:6){
beta[j] ~ dnorm(0,0.1)
}

}"


dat   <- list(Y=Y,n=n,X=X)
model <- jags.model(textConnection(logistic_model),data = dat,n.chains=3, quiet=TRUE)

update(model, 1000)

samp <- coda.samples(model, 
                     variable.names=c("beta"), 
                     n.iter=1000)

beta1 <- samp[[1]]  
beta2 <- samp[[2]]
beta3 <- samp[[3]]

beta  <- rbind(beta1,beta2,beta3)


beta.mn  <- as.matrix(colMeans(rbind(beta1[1000,],beta2[1000,],beta3[1000,])))

x_test<-X
x_test <- cbind(1, x_test)

y_hat <- x_test %*% beta.mn

prediction<-c()
for(i in 1:1000){
  temp<-exp(y_hat[i])
  prediction[i]<-as.numeric(temp>1)
}
table(prediction)
table<-table(Y,prediction)
table
TP<-table[2,2]
FP<-table[1,2]
TN<-table[1,1]
FN<-table[2,1]

balanced.accuracy<-0.5*(TP/(TP+FN)+TN/(TN+FP))
precision<-TP/(TP+FP)
recall<-TP/(FN+TP)

balanced.accuracy
precision
recall

beta.mn
beta_true




