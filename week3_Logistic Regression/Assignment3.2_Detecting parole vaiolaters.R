### Assignment 3.2: PREDICTING PAROLE VIOLATORS ###

# Investigate the data set using str() and sumary()
str(parole)
summary(parole)

# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)


## Preparing the Dataset

# convert unordered variables factors with at least 3 levels to factors
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)


## Splitting into a Training and Testing Set

# To ensure consistent training/testing set splits, run the following 5 lines of code
set.seed(144)

library(caTools)

split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)

## Building a Logistic Regression Model
model1 <- glm(violator ~ ., data=train, family=binomial)
summary(model1)

## Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, 
# served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny
## According to the model, what are the odds this individual is a violator?
odds <- exp(-4.2411574 + 0.3869904*1 + 0.8867192*1 +  -0.0001756*50 + -0.1238867*3 + 0.0802954*12 + 0.6837143)
##According to the model, what is the probability this individual is a violator?
prob <- 1/(1 + exp(-(-4.2411574 + 0.3869904*1 + 0.8867192*1 +  -0.0001756*50 + -0.1238867*3 + 0.0802954*12 + 0.6837143)))


## Evaluating the Model on the Testing Set ##

# obtain the model's predicted probabilities for parolees in the testing set, 
predTest <- predict(model1, newdata=test, type='response')

# What is the maximum predicted probability of a violation?
max(predTest)


## Evaluate the model's predictions on the test set using a threshold of 0.5.

# Create the confusion matrix
table(test$violator, predTest > 0.5)

# What is the model's sensitivity?
12/(11+12)

# What is the model's specificity?
167/(167+12)

# What is the model's accuracy?
(167 + 12)/(167+12+11+12)

# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
(167 + 12)/(167+12+11+12)


#load ROCR
library(ROCR)
# predict a function of ROC
ROCRpred <- prediction(predTest, test$violator)
#  what is the AUC value for the model?
auc <- as.numeric(performance(ROCRpred, 'auc')@y.values)
