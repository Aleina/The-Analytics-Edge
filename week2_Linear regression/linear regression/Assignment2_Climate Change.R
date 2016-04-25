### Assignment 2: Climate Change ###

# split the data in a train (until 2006) and test set (after)
climateTrain <- subset(climate, Year <= 2006)
climateTest <- subset(climate, Year > 2006)

# create a linear regression to predict temperature from atmospheric concetrations
ClimReg <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climateTrain)
summary(ClimReg)

# compute correlation between all variable to check for collinearity
cor(climateTrain)

# Since there is Collinearity with N2O let's bild the model without colliniear variables
ClimReg2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data= climateTrain)
summary(ClimReg2)

# create the best optimizated model for this problem with the step function
bestClimReg <- step(ClimReg)
summary(bestClimReg)

# predict temperatures in the test set based on the best model
predictTemp <- predict(bestClimReg, newdata=climateTest)
summary(predictTemp)

# Calculate SSE in test set
SSE <- sum((predictTemp - climateTest$Temp)^2)

# Calculate SST in test set
SST <- sum((mean(climateTrain$Temp)- climateTest$Temp)^2)

# Calculate R^2
R2 <- 1 - SSE/SST
