rm(list = ls())
setwd("/Users/Ludo/Documents/Documents importants/2018 - 2020 - Polimi/Bayesian Statistics/Projet")
penos<-read.csv("penalty_data.csv")
load("final_dataset.Rda")

#Correlation Analysis

#1 Correlation Matrix
penos$Saved <- as.numeric(penos$Saved)
penos$Player <- as.numeric(penos$Player)
penos$Scored <- as.numeric(penos$Scored)
penos$Foot <- as.numeric(penos$Foot)
penos$Kick_Direction <- as.numeric(penos$Kick_Direction)
penos$Keeper_Direction <- as.numeric(penos$Keeper_Direction)
penaltyCor <- penos[c(4,8,10,11,12)]
library(corrplot)
corPlot <- cor(penaltyCor)
corrplot(corPlot)
cor(penaltyCor)

#1.2 Second Correlation Matrix
final_dataset$isGoal<- as.numeric(final_dataset$isGoal)
final_dataset$isHome<- as.numeric(final_dataset$isHome)
final_dataset$Zone_kick<- as.numeric(final_dataset$Zone_kick)
final_dataset$Zone_keeper<- as.numeric(final_dataset$Zone_keeper)
final_dataset$isRightFooted <- as.numeric(final_dataset$isRightFooted)
final_dataset$Scored_penaltys_by_kicker<- as.numeric(final_dataset$Scored_penaltys_by_kicker)
final_dataset$Failed_penaltys_by_kicker<- as.numeric(final_dataset$Failed_penaltys_by_kicker)
final_dataset$Not_stopped_penaltys_by_keeper<- as.numeric(final_dataset$Not_stopped_penaltys_by_keeper)
final_dataset$Stopped_penaltys_by_keeper<- as.numeric(final_dataset$Stopped_penaltys_by_keeper)
final_dataset$Time.of.Penalty.Awarded<- as.numeric(final_dataset$Time.of.Penalty.Awarded)
data <- final_dataset[c(1,2,3,4,5,6,8,10)]
names(data) <- c("Scored","Kick Zone","Dive Direction","IsRightFooted","Home/Away","HistoryKicker","HistoryKeeper","TimePenalty")
penaltyCor <- 
corPlot <- cor(data)
corrplot(corPlot)
cor(penaltyCor)

#2 Correlation between Foot and scored ?
library(ggpubr)
ggscatter(penos, x = "Foot", y = "Scored", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Foot", ylab = "Scored")

#Comment : The dot at 2.0 refers to the left foot and the dot at 3.0 refers to the right
# foot. Similarly in the y-axis, the 'Kick Result' value displays the result of the 
# 'Scored' variable -- 1.00 is 'Missed' and 2.00 is 'Scored'. In the dataset, right-footed
# penalty takers outnumbered the left-footed kickers, but when we look at whether this
# impacted the picture on the outcome of the penalty kicks, there is practically no
# difference. The correlation is positive as the vast majority of the kickers do end up
# scoring from the penalty mark.

#Pearson Correlation:
pear <- cor.test(penos$Foot, penos$Scored)
pear
# No correlation between those two


#3 Correlation between Foot and Keeper_Direction ?

ggscatter(penos, x = "Foot", y = "Keeper_Direction", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Foot", ylab = "Keeper_Direction")


#Comment : The values 2, 3, 4, indicate, center, left, and right, in that order. The plot shows that the correlation is 0.22
# which is stronger than the previous observed tests.
#The plot shows that the correlation is 0.22, which is stronger than the previous observed
# tests. The p-value is lower here as well, compared to our previous plots. So, this
# indicates that there is 2.4 percent probability that the null-hypothesis is true (there
# is no relationship, effect, between the foot and the keeper's direction). So, the
# goalkeepers do guess the direction of the ball based on the kicking foot significantly 
# to reject the null.

#Pearson Correlation:
pear <- cor.test(penos$Foot, penos$Keeper_Direction)
pear

’#4 Correlation between Foot and Kick_Direction ?

ggscatter(penos, x = "Foot", y = "Kick_Direction", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Foot", ylab = "Kick_Direction")

#Comment : Based on this finding, the correlation between the foot and kick direction is
# weaker than the correlation between the foot and keeper's direction. So, perhaps the
# goalkeepers can predict which foot the kicker can use, and players know which direction
# they choose to kick to, and so the keepers need to make an educated guess where to dive
# to make a save.

#Pearson Correlation:
pear <- cor.test(penalty$Foot, penalty$Kick_Direction)
pear
