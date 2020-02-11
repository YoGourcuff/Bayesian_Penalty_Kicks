rm(list = ls())

setwd("/Users/Ludo/Desktop/Documents importants/2018 - 2020 - Polimi/Bayesian Statistics/Projet")
load("penalty.Rda")
load("mcshapiro.test.RData")
detach(penalty)
attach(penalty)
Left_Foot <- data.frame("Direction"=Kick_Direction[Foot=="L"])
Right_Foot <- data.frame("Direction"=Kick_Direction[Foot=="R"])
Home <- data.frame("Direction"=Kick_Direction[HomeAway=="Home"])
Away <- data.frame("Direction"=Kick_Direction[HomeAway=="Away"])

#Naive Bayes
DataNB <- data.frame("Class"=Scored,"Keep_Direction"=Keeper_Direction,"Kick_Direction"=Kick_Direction)

length(Away[Away$Direction=="C",])

library(e1071)
model <- naiveBayes(Class ~ ., data = DataNB, laplace = 0)
pred<-predict(model, DataNB)
table(pred, DataNB$Class)

#Répartition de pénaltys suit une Normale ?
shapiro.test(tb)

#Modèle par Dirichlet Process et loi multinomiale
#For every player, his kick direction follows a multinomial law (n=number of players, p1 (left),p2(centre),p3(right))
#p1,p2,p3 are determinated with a Dirichlet law of parameters (alpha1,alpha2,alpha3) to be determinated with JAGS or BUGS
library(dirichletprocess)
library(rjags)
install.packages("rjags", configure.args = c("--with-jags-include={path_to_jags}/include/JAGS", "--with-jags-lib={path_to_jags}/lib"))
install.packages("rjags", dependencies=TRUE)
install.packages( pkg = "rjags", 
                  lib = "/perm/hechtmay/R/R-3.0.1/library", 
                  repos = "http://cran.us.r-project.org" , 
                  configure.args = "--with-jags-include=/perm/hechtmay/JAGS/JAGS-3.3.0/include/JAGS --with-jags-lib=/perm/hechtmay/JAGS/JAGS-3.3.0/lib64 --enable-rpath", 
                  dependencies=TRUE)
c <- 2
G_0 <- function(n) rnorm(n, 0, 10)
n <- 100
b <- rbeta(n, 1, c)
p <- numeric(n)
p[1] <- b[1]
p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))
y <- G_0(n)
theta <- sample(y, prob = p, replace = TRUE)

library(MCMCpack)
density <- ddirichlet(c(.1,.2,.7), c(1,1,1))
draws <- rdirichlet(20, c(100,1,10) )
?rdirichlet


#Bayesian Discriminant Analysis

library(Rlda)


Kick_Direction<-as.character(Kick_Direction)
Kick_Direction[Kick_Direction=="L"]<-1
Kick_Direction[Kick_Direction=="C"]<-2
Kick_Direction[Kick_Direction=="R"]<-3

Keeper_Direction<-as.character(Keeper_Direction)
Keeper_Direction[Keeper_Direction=="L"]<-1
Keeper_Direction[Keeper_Direction=="C"]<-2
Keeper_Direction[Keeper_Direction=="R"]<-3
Saved[Saved==2]<-3
Saved[Saved==1]<-2
Saved[Saved==0]<-1


DataRLDA <- data.frame("Class"=Keeper_Direction,"Kick_Direction"=Kick_Direction, "Saved"=Saved)
model <- rlda(Class ~ ., data = DataRLDA,resprior = c(0.45,0.1,0.45))
pred<-predict(model, DataRLDA)
table(pred, DataRLDA$Class)
rlda(Class ~ .,DataRLDA)
?rlda

#Discriminant Multinomial

DataRLDAMulti<- data.frame("Foot L"=c(9,1,9),"Foot R"=c(40,16,31),"Home"=c(28,9,29),"Away"=c(21,8,11)) #"Total"=c(49,17,40)
beta <- rep(1,ncol(DataRLDAMulti))
gamma <- 0.01
model<-rlda.multinomial(data=DataRLDAMulti, n_community = 3, beta = beta, gamma = gamma, n_gibbs = 10, ll_prior = TRUE, display_progress = FALSE)
model$Theta
model$Phi
pred<- predict(model, c(1,), nclus=3)

#Using dawai 

Foot<-as.character(Foot)
Foot[Foot=="L"]<--1
Foot[Foot=="R"]<-1
Foot<-as.numeric(Foot)
HomeAway<-as.character(HomeAway)
HomeAway[HomeAway=="Away"]<--1
HomeAway[HomeAway=="Home"]<-1
HomeAway<-as.numeric(HomeAway)

DataDAWAIKicker <- data.frame("Class"=Kick_Direction,"Foot"=Foot,"HomeAway"=HomeAway,"Time"=Time.of.Penalty.Awarded)
Kick_Direction<-as.numeric(Kick_Direction)
DataDAWAIKeeper<- data.frame("Class"=Keeper_Direction,"Foot"=Foot,"HomeAway"=HomeAway,"Time"=Time.of.Penalty.Awarded,"Kick_Zone"=Kick_Direction)
levels(DataDAWAIKeeper$Class)
table(DataDAWAIKeeper$Class)
A <- matrix(0, ncol = 12, nrow = 6)

set.seed(-9152)
values <- runif(nrow(DataDAWAIKeeper))

train <- (values < 0.66)

library(dawai)

obj <- rlda(Class ~ Foot + HomeAway + Time + Kick_Zone, DataDAWAIKeeper, subset = train, restext = "s>1", prior = c(0.45, 0.1, 0.45))
obj
test <- (values >= 0.66)
DataDAWAIKeeper[test,]
pred <- predict(obj, newdata = DataDAWAIKeeper[test, ])
pred$class[,1]
#pred$error.rate
table(pred$class[,1], DataDAWAIKeeper[test,]$Class)

