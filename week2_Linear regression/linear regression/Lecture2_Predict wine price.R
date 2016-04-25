### Lecture 2: Predict wine price ###

## create linear regression models ##

# look at data structure and summary
str(wine)
summary(wine)

# predict price from AGST
model1 <- lm(Price ~ AGST, data=wine)

# look at the summary of this model
summary(model1)

# look at the residuals
model1$residuals

# calculate SSE
sum(model1$residuals^2)


# predict price from AGST and harvest rain
model2 <- lm(Price ~ AGST + HarvestRain, data=wine)

#look at the summary
summary(model2)

# calculate SSE
sum(model2$residuals^2)

# predict price with all variables from wine data frame
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)

# look at the summary of this model
summary(model3)

#compute the SSE
sum(model3$residuals^2)

# create a linear regression model to predict Price using HarvestRain and WinterRain as independent variables
model_ex <- lm(Price ~ HarvestRain + WinterRain, data = wine)

#look at the summary of this exercise model
summary(model_ex)


# create a new model without the less significant variable FrancePop
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine) 

# look at it's summary
summary(model4)


## Compute Correlations ##

# compute the correlation between winter rain and price
cor(wine$WinterRain, wine$Price)

# compute the correlation between age and france population
cor(wine$Age, wine$FrancePop)

# compute all correlations with rhis dataset
cor(wine)

# compute the model without all varibles that were insignificant
model5 <- lm(Price ~ AGST + WinterRain + HarvestRain, data=wine)

# look at model
summary(model5) #the model quality dropped from model 4: Always remove variables one by one


## Making Predictions ##

#use test data and look at it structure
str(wineTest)

# make predictions of price in the test set with model4
predictTest <- predict(model4, newdata=wineTest)
predictTest

# compute the SSE for this new predictions 
SSE <- sum((wineTest$Price - predictTest)^2)

# compute SST
SST <- sum((wineTest$Price - mean(wine$Price))^2)

# compute out of sample R squared
1-SSE/SST

