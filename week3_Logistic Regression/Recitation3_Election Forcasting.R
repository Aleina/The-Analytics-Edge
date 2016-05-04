### Recitation 3: Election Forcasting ###


## Dealing with missing data

# look at data
str(polling)

# create table of data per year
table(polling$Year)

# look at missing data with summary
summary(polling)

# load and install mice package
library('mice')

# create a simpler data set with only the varibles Rasmussen, SurveyUSA, DiffCount, PropR 
simple <- polling[c('Rasmussen', 'SurveyUSA', 'DiffCount', 'PropR' )]

# set seed to allow reproducibility
set.seed(144)

# using imputation to fill missing values
imputed <- complete(mice(simple))

# look at summary rto see no more missing values
summary(imputed)

# pass the completed variables to the original data frame
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA

# check if everything is compeleted on polling data frame
summary(polling)


## Split data where test is data from 2012 and the rest is on train set
train <- subset(polling, Year==2004 | Year==2008)
test <- subset(polling, Year==2012)


## Baseline Model

# evaluate the baseline model
table(train$Republican) 
# is more commum to be republican, the model always predict republican with 53% accuracy 

# create a more sofisticated baseline model with sign function
table(train$Republican, sign(train$Rasmussen))


## Look for multicolinearity

# compute correction of numeric variables in the data set
cor(train[c('Rasmussen', 'SurveyUSA', 'DiffCount', 'PropR', 'Republican')])
# they are so correlated that we use only the best model


## Create the model
model1 <- glm(Republican ~ PropR, data=train, family='binomial')
summary(model1)

# Make predictions in the training set
trainPred1 <- predict(model1, type='response') # not mutch better than baseline

# Compute confusion matrix with threshold of 0.5
table(train$Republican, trainPred1 > 0.5)



## Create other model with the two less correlated independent variables
model2 <- glm(Republican ~ SurveyUSA + DiffCount, data=train, family='binomial')
summary(model2) # AIC is not much better 

# Make predictions in the training set
trainPred2 <- predict(model2, type='response')

# Compute confusion matrix with threshold of 0.5
table(train$Republican, trainPred2 > 0.5)


## Make prediction on test data
testPred <- predict(model2, newdata=test, type='response')

# create the confusion matrix
table(test$Republican, testPred > 0.5)

# look for the only error we made
subset(test, testPred >= 0.5 & Republican == 0)
