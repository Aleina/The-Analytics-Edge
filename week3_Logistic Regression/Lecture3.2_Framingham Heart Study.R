### Lecture 3.2: Framingham Heart Study ###

# look at the data
str(framingham)

## split the data

# load library and set seed
library('caTools')
set.seed(1000)

# create a split
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)


## Build model
framinghamLog <- glm(TenYearCHD ~ ., data=train, family=binomial)
summary(framinghamLog)

## Make prediction on the test set
predictTest <- predict(framinghamLog, type = 'response', newdata = test)

# Use a threshold of 0.5 to create a confusion matrix
table(test$TenYearCHD, predictTest > 0.5)

# compute accuracy of the model
(1069 + 11)/(6 + 187 + 1069 + 11)

# compute the baseline model: P(0)
(1069 + 6)/(6 + 187 + 1069 + 11)

# compute sensitivity
11/(11 + 187) # the prob of being positive when the result is positive

# compute specificity
1069/(1069 + 6) # the prob of being negative when the result is negative 


## calculate out of sample AUC
# load library
library(ROCR)

#call the output o ROCRpred
ROCRpred <- prediction(predictTest, test$TenYearCHD)

# calculate AUC
as.numeric(performance(ROCRpred,'auc')@y.values)
