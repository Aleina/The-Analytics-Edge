### Assignment 2: DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA ###

# look at the data
str(FluTrain)
summary(FluTrain)

# Which week corresponds to the highest percentage of ILI-related physician visits?
which.max(FluTrain$ILI)
FluTrain$Week[303]

# Which week corresponds to the highest percentage of ILI-related query fraction?
which.max(FluTrain$Queries)
FluTrain$Week[303]

# What best describes the distribution of values of ILI?
hist(FluTrain$ILI)

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(log(FluTrain$ILI), FluTrain$Queries)

# Let's call the regression model from the previous problem
FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

# use the cor function to compute the correlation between two variables
Correlation <- cor(log(FluTrain$ILI), FluTrain$Queries)

# what is the relationship between the correlation and r2
exp(-0.5/Correlation)
log(1/Correlation)
Correlation^2

# Predicts the ILI value on test set with exponentiation to counter the log()
PredTest1 <- exp(predict(FlueTrend1, newdata=FluTest))

# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
which(FluTest$Week == '2012-03-11 - 2012-03-17')
PredTest1[11]

# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations?
SSE_test <- sum((PredTest1 - FluTest$ILI)^2)
RMSE_test <- sqrt(SSE_test/nrow(FluTest))


# use the "zoo" package, which provides a number of helpful methods for time series models
install.packages('zoo')
library('zoo')

# Run the following commands to create the ILILag2 variable in the training set:
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

# How many values are missing in the new ILILag2 variable?
summary(FluTrain$ILILag2)

# Use the plot() function to plot the log of ILILag2 against the log of ILI.
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries and the log of the ILILag2.
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)


# Add an ILILag2 variable to the FluTest data frame
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# How many missing values are there in this new variable?
summary(FluTest$ILILag2)

# Since both sets are sequential fill in the missing values for ILILag2 in FluTest with the lasts from FlueTrain
FluTest$ILILag2[1] <- FluTrain$ILI[416] 
FluTest$ILILag2[2] <- FluTrain$ILI[417]

# Obtain test set predictions of the ILI variable from the FluTrend2 model
PredTest2 <- exp(predict(FluTrend2, newdata=FluTest))

# What is the test-set RMSE of the FluTrend2 model?
SSE_test2 <- sum((PredTest2 - FluTest$ILI)^2)
RMSE_test2 <- sqrt(SSE_test2/nrow(FluTest))
