### Assignment 2 : STATE DATA ###


## Data Exploration ##

# Load the dataset and convert it to a data frame by running the following two commands in R:
data(state)
statedata <- cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

# Plot all of the states' centers with latitude on the y axis (the "y" variable in our dataset) and longitude on the x axis (the "x" variable in our dataset)
plot(statedata$x, statedata$y)

# Using the tapply command, determine which region of the US has the highest average high school graduation rate of all the states in the region
tapply(statedata$HS.Grad, statedata$state.region, mean)

# make a boxplot of the murder rate by region
boxplot(statedata$Murder ~ statedata$state.region)

# Which state is the outlier in Northeast States
which.max(statedata$Murder[statedata$state.region == 'Northeast'])
statedata$state.name[statedata$state.region == 'Northeast'][6]


## Predicting Life Expectancy ##

# Build a model to predict life expectancy by state using the state statistics we have in our dataset.
LifeExpModel2 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpModel2)

#Now plot a graph of life expectancy vs. income using the command:
plot(statedata$Income, statedata$Life.Exp)


## Refining the Model and Analyzing Predictions ##

#  Experiment with removing independent variables from the original model. 
LifeExpModel2 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpModel2)

LifeExpModel3 <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpModel3)

LifeExpModel4 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpModel4) # All variables are significant, low R^2 drop. Best Model!

LifeExpModel5 <- lm(Life.Exp ~ Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpModel5) # R^2 drop to much

# Which state do we predict to have the lowest life expectancy?
predLifeExp <- predict(LifeExpModel4)
sort(predLifeExp)

# Which state actually has the lowest life expectancy?
which.min(statedata$Life.Exp)
statedata$state.name[40]

# Which state actually has the highest life expectancy?
which.max(statedata$Life.Exp)
statedata$state.name[11]

# Take a look at the vector of residuals
#For which state do we make the smallest absolute error?
sort(abs(LifeExpModel4$residuals))
