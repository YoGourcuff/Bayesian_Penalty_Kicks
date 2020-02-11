rm(list = ls())
detach(penalty)
detach(penos)
setwd("/Users/Ludo/Desktop/Documents importants/2018 - 2020 - Polimi/Bayesian Statistics/Projet")
load("penalty.Rda")
load("mcshapiro.test.RData")
attach(penalty)

Left_Foot <- data.frame("Direction"=Kick_Direction[Foot=="L"])
Right_Foot <- data.frame("Direction"=Kick_Direction[Foot=="R"])
Home <- data.frame("Direction"=Kick_Direction[HomeAway=="Home"])
Away <- data.frame("Direction"=Kick_Direction[HomeAway=="Away"])


#Kick direction distribution
tb1<-table(Kick_Direction)[c(3,2,4)]
barplot(tb1,
        main="Kick Repartition",
        xlab="Direction",
        ylab="Count",
        ylim = c(0,60),
        border="red",
        col="blue",
)

# Keeper direction distribution
tb2<-table(Keeper_Direction)[c(3,2,4)]
barplot(tb2,
        main="Dive Repartition",
        xlab="Direction",
        ylab="Count",
        ylim = c(0,60),
        border="red",
        col="blue",
)

# Preferential direction of a left-handed kicker
barplot(table(Left_Foot)[c(3,2,4)],
        main="Preferential direction of a left-handed kicker",
        xlab="Direction",
        ylab="Count",
        ylim = c(0,15),
        border="red",
        col="blue",
)

#Preferential direction of a right-handed kicker
barplot(table(Right_Foot)[c(3,2,4)],
        main="Preferential direction of a right-handed kicker",
        xlab="Direction",
        ylab="Count",
        ylim = c(0,50),
        border="red",
        col="blue"
)


# Distribution of penalties awarded
Minute10 <- Time.of.Penalty.Awarded
Minute10<-floor(Minute10/10)*10
tb3 <- table(Minute10)
barplot(tb,
        main="Répartition des penaltys distribués",
        xlab="Minute",
        ylab="Count",
        ylim = c(0,20),
        border="red",
        col="blue"
)
library(moments)
agostino.test(tb)

shapiro.test(tb)

MissedMinute <- data.frame("Time"=Time.of.Penalty.Awarded[Scored=="Missed"])
MissedMinute<-floor(MissedMinute/10)*10
tb <- table(MissedMinute)
barplot(tb,
        main="Répartition des penaltys distribués",
        xlab="Count",
        ylab="Minute",
        ylim = c(0,20),
        border="red",
        col="green"
)

