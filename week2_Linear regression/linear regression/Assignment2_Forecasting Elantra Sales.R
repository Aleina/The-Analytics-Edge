### Assigment 2: FORECASTING ELANTRA SALES ###


## A Linear Regression Model ##

# Look at the data
str(elantra)

# Split the data set into training and testing sets as follows: place all observations for 2012 and earlier in the training set, and all observations for 2013 and 2014 into the testing set.
elantraTrain <- subset(elantra, Year <= 2012)
elantraTest <- subset(elantra, Year >= 2013)

# Build a linear regression model to predict monthly Elantra sales using Unemployment, CPI_all, CPI_energy and Queries as the independent variables
elantraModel1 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(elantraModel1)


## Modeling Seasonality ##

# To incorporate the seasonal effect due to the month, build a new linear regression model that predicts monthly Elantra sales using Month as well as the other variables
elantraModel2 <- lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(elantraModel2) # Adjust R^2 decreases, not a better model

#  what is the absolute difference in predicted Elantra sales given that one period is in January and one is in March?
abs((1 * 110.69) - (3 * 110.69)) 

# what is the absolute difference in predicted Elantra sales given that one period is in January and one is in May?
abs((1 * 110.69) - (5 * 110.69))


# Re-run the regression with the Month variable modeled as a factor variable
elantraModel3 <- lm(ElantraSales ~ as.factor(Month) + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(elantraModel3) 


## Multicolinearity ##

# compute the correlations of the variables in the training set
cor(elantraTrain)

# Let us now simplify our model
elantraModel4 <- lm(ElantraSales ~ as.factor(Month) + Unemployment + CPI_all + CPI_energy, data = elantraTrain)
summary(elantraModel4)


## Test Set Predictions ##

# make predictions on the test set
predElantra <- predict(elantraModel4, newdata=elantraTest)

# What is the sum of squared errors of the model on the test set?
SSE_test <- sum((predElantra - elantraTest$ElantraSales)^2)

# What would the baseline method predict for all observations in the test set?
SST <- sum((mean(elantraTrain$ElantraSales) - elantraTest$ElantraSales)^2)

# What is the test set R-Squared?
R2 <- 1 - (SSE_test/SST) 

# What is the largest absolute error that we make in our test set predictions?
sort(abs(predElantra - elantraTest$ElantraSales))

# In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(predElantra - elantraTest$ElantraSales))
elantraTest$Month[5]
elantraTest$Year[5]
