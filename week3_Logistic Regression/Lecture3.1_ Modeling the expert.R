### Lecture 3.1: Modeling the expert ###

# look at the data
str(quality)

# Who many patient had poor care
table(quality$PoorCare)

# what is the baseline model? The probability of the most probable outcome
baseline <- 98/131


## split the data in train and test set

# import caTools
install.packages("caTools")
library("caTools")

# set a random seed
set.seed(88)

# create the split
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)

# split the data
qualityTrain <- subset(quality, split==TRUE)
qualityTest <- subset(quality, split==FALSE)


## create the model to predict poor care from office visits and narcotics 
QualityLog <- glm(PoorCare ~  OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

# Make predict on the training set
predictTrain <- predict(QualityLog, type = 'response')
summary(predictTrain)

# Compute the avarege probability of each true outcome
tapply(predictTrain, qualityTrain$PoorCare, mean) #bigger prob for poor care


# predict "PoorCare" using the independent variables "StartedOnCombination" and "ProviderCount".
QualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog2)


## Compute confusion matrix ##

# for p = 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)
# compute sensitivity: TP/(TP + FN)
10/(10 + 15)
# compute specificity TN/(TN + FP)
70/(70 + 4)

# increase the threshold to 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
# sensitivity
8/(8+17)
# specificity
73/(73+1)      # it becames more specific and less sensitive when threshold increases

# decrease the the threshold to 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)
# sensitivity
16/(16+9)
# specificity
54/(54+20)     # it becomes more sensitive and less specific when thw threshold decreases


## Roc Curves ##

# install ROCR
install.packages('ROCR')
library('ROCR')

## crearte a ROC curve
# predict a function of ROC
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
# Define what will be on the plot
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
#plot the function
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7) )


## Interpret the Model ##

# Make ou of sample predictions
predictTest <- predict(QualityLog, type='response', newdata=qualityTest)

# Compute a confusion matrix for threshold 0.3
table(qualityTest$PoorCare, predictTest > 0.3)
# Calculate Accuracy: (TN + TP)/N
(19+6)/32 
# Calculate False Positive Rate: FP/(FP + TN)
5/(5+19)
# Calculate the True Positive Rate: TP/(TP + FN)
6/(6+2)

# What is the AUC of this model on the test set?
ROCRpredTest <- prediction(predictTest, qualityTest$PoorCare)
auc <- as.numeric(performance(ROCRpredTest, 'auc')@y.values)
