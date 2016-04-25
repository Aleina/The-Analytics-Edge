### Recitation 2: Moneyball on NBA ###

# look at the structure of NBA dataframe
str(NBA)


## Predict wins ##

# create a table to see who many wins are necessary to make it to the playoffs
table(NBA$W, NBA$Playoffs)

# create a variable that is the difference between points scored and points allowed
NBA$PTSDiff <- NBA$PTS - NBA$oppPTS

# create a scatter plot to the see the relationship between wins and difference in points
plot(NBA$PTSDiff, NBA$W)

# create a linear regression model between wins and points difference
WinsReg <- lm(W ~ PTSDiff, data=NBA)
summary(WinsReg) #equation: W = 4.1 + 0.03259 * PTSDiff


## Prdict Points ##

# create a linear regression to predict points
PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=NBA)
summary(PointsReg)

# calculate the SSE
SSE <- sum(PointsReg$residuals^2)

# calculate RMSE
RMSE <- sqrt(SSE/nrow(NBA))

# Remove the variable with least significant p-value :TOV
PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
summary(PointsReg2)

# Drop the next less significant variable: DRB
PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=NBA)
summary(PointsReg3)

# Drop the next less significant variable: BLK
PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(PointsReg4)

# Calculate SSE and RMSE to see if the errors inflated to much
SSE_4 <- sum(PointsReg4$residuals^2)
RMSE_4 <- sqrt(SSE_4/nrow(NBA)) # The amount of error stay about the same with a simpler model


## Making predictions ##

# Predicting in test set with our best model os Points
PointsPrediction <- predict(PointsReg4, newdata=NBA_test)

# Compute SSE on test sample
SSE_test <- sum((PointsPrediction - NBA_test$PTS)^2)

# compute SST on test sample
SST_test <- sum((mean(NBA$PTS) - NBA_test$PTS)^2)

# Calculate the R^2
R2 <- 1 - SSE_test/SST_test

# Calculate RMSE
RMSE_test <- sqrt(SSE_test/nrow(NBA_test))
