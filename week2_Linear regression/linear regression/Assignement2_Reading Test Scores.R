### Assignement 2: READING TEST SCORES ###

# look at data
str(pisaTrain)
summary(pisaTrain)

# what is the average reading test score of males and females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Remove NA from both datasets
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)

# Set the reference level in raceeth to the most common observation in both data sets
pisaTrain$raceeth <- relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth, "White")

# build a linear regression model using the training set to predict readingScore using all variables
lmscore <- lm(readingScore ~ ., data=pisaTrain)
summary(lmscore)

# What is the training-set root-mean squared error (RMSE) of lmScore?
SSE <- sum(lmscore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))

# use the lmScore model to predict the reading scores of students in pisaTest
predTest <- predict(lmscore, newdata = pisaTest)
summary(predTest)

# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE_test <- sum((predTest - pisaTest$readingScore)^2)

# What is the root-mean squared error (RMSE) of lmScore on the testing set?
RMSE_test <- sqrt(SSE_test/nrow(pisaTest))

# What is the predicted test score used in the baseline model?
mean(pisaTrain$readingScore)

# What is the sum of squared errors of the baseline model on the testing set? 
SST_test <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)

# Calculate the R^2 of the test model
R2 <- 1 - SSE_test/SST_test
