### Assignment 3.3: PREDICTING LOAN REPAYMENT ###

## Preparing the Dataset ##

# What proportion of the loans in the dataset were not paid in full?
str(loans)
table(loans$not.fully.paid)
1533/(1533+8045)

# Which of the following variables has at least one missing observation?
summary(loans)

# best reason to fill in the missing values for these variables instead of removing them?
missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) 
                  | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)
12/62

##  fill missing values with multiple imputation
# load mice library
library('mice')
# set seed
set.seed(144)
# perfor the imputation
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed
# add the dependent variable to imputed
imputed$not.fully.paid <- loans$not.fully.paid
# look at the summary
summary(imputed)


## Due to differences in results of imputation data I will use from now on the loans imputed data set from now on
imputed <- loans.imputed


## Prediction Models ##

## split it into a training and testing set with 70% of the data in the training set
# load library caTools
library(caTools)
# set seed
set.seed(144)
# do the split
split <- sample.split(imputed$not.fully.paid, SplitRatio = 0.7)
# assign the split to train and test sets
train <- subset(imputed, split == TRUE)
test <- subset(imputed, split == FALSE)

# use logistic regression trained on the training set to predict the dependent variable not.fully.paid using all the independent variables.
model1 <- glm(not.fully.paid ~ ., data=train, family = 'binomial')
summary(model1)

# Consider two loan applications, which are identical other than the fact (A) has FICO credit score 700 and (B) has FICO credit score 710.
# What is the value of Logit(A) - Logit(B)?
logit.diff <- (-9.406e-03*700) - (-9.406e-03*710)

# let O(A) be the odds of loan A not being paid back in full and define O(B) similarly for loan B. 
# What is the value of O(A)/O(B)?
odds.ratio <- exp(logit.diff)

## Predict the probability of the test set loans not being paid back in full
predicted.risk <- predict(model1, newdata=test, type='response')
  
# Store predicted.risk and add it to your test set
test$predicted.risk <- predicted.risk

## Compute the confusion matrix using a threshold of 0.5.
table(test$not.fully.paid, predicted.risk > 0.5)
# What is the accuracy of the logistic regression model? 
(2400 + 3)/(2400 + 13 + 457 + 3) 
# What is the accuracy of the baseline model? 
(2400 + 13)/(2400 + 13 + 457 + 3) 

## Use the ROCR package to compute the test set AUC
# load package
library(ROCR)
ROCRpred <- prediction(predicted.risk, test$not.fully.paid)
auc <- as.numeric(performance(ROCRpred, 'auc')@y.values)


## A smart baseline model ##

# Using the training set, build a bivariate logistic regression model that predicts not.fully.paid using the variable int.rate
baseline.model <- glm(not.fully.paid ~ int.rate, data = train, family = 'binomial')
summary(baseline.model)

# Make test set predictions for the bivariate model.
pred.baseline <- predict(baseline.model, newdata = test, type = 'response')
summary(pred.baseline)

# What is the AUC for the baseline model
ROCRpred.baseline <- prediction(pred.baseline, test$not.fully.paid)
auc <- as.numeric(performance(ROCRpred.baseline, 'auc')@y.values)


## Computing the Profitability of an Investment ##

# To evaluate the quality of an investment strategy compute this profit for each loan in the test set
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

# What is the maximum profit of a $10 investment in any loan in the testing set 
10 * max(test$profit)


## An Investment Strategy Based on Risk ##

# build a data frame of the test set loans with an interest rate of at least 15%.
highInterest <- subset(test, int.rate >= 0.15)

# What is the average profit of a $1 investment in one of these high-interest loans?
summary(highInterest$profit)

# What proportion of the high-interest loans were not paid back in full?
table(highInterest$not.fully.paid)
110/437

# Determine the 100th smallest predicted probability of not paying in full
cutoff <- sort(highInterest$predicted.risk, decreasing = FALSE)[100]

# build a data frame consisting of the high-interest loans with predicted risk not exceeding the cutoff we just computed
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)

# What is the profit of the investor, who invested $1 in each of these 100 loans?
sum(selectedLoans$profit)

# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid)
