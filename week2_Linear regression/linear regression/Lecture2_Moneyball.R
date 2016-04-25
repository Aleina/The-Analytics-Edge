### Lecture 2: Moneyball ###

#look at the structure of the data
str(baseball)

# subset for only years before Paul DePodesta
moneyball <- subset(baseball, Year < 2002)

# look at it's structure
str(moneyball)


## create a prediction of wins with runs scored and runs allowed

# craeate a variable of the difference between runs scored and allowed
moneyball$RD <- moneyball$RS - moneyball$RA

# is there a relationship between runs differences and wins?
plot(moneyball$RD, moneyball$W)

# create the model
WinsReg <- lm(W ~ RD, data = moneyball)

# look at summary
summary(WinsReg) #equation: W = 80.8814 + 0.1058 * RD


## What are the most important variable to win runs

# create the model
RunsReg <- lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg) #problems of colinearity: negative coeficient

# Runs model again with BA
RunsReg <- lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg) # RS = -804.63 + 2737.77*OBP + 1584.91*SLG

# Allowed Runs Model with opponent variables
AllowedRunsReg <- lm(RA ~ OOBP + OSLG, data=moneyball)
summary(AllowedRunsReg) # RA = -837.38 + 2913.6*OOBP + 1514.29*OSLG

#players performance
giambi <- -804.63 + 2737.77*0.391 + 1584.91*0.450
menechino <- -804.63 + 2737.77*0.369 + 1584.91*0.374
myers <- -804.63 + 2737.77*0.313 + 1584.91*0.447
pena <- -804.63 + 2737.77*0.361 + 1584.91*0.500


## Team ranks analysis ##

#create a vector for ranks
teamRank <- c(1,2,3,3,4,4,4,4,5,5)

#create a vector for team wins in 2012 and 2013
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)

wins2013 <- c(97,97,92,93,92,96,94,96,92,90)

# What is the correlation between teamRank and wins2012?
cor(teamRank, wins2012)

# What is the correlation between teamRank and wins2013?
cor(teamRank, wins2013)


