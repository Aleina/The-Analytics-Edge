### Assignment 3.1: POPULARITY OF MUSIC RECORDS ###

## Understanding the data
str(songs)

# How many observations (songs) are from the year 2010?
table(songs$year)

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
MichaelJackson <- subset(songs, songs$artistname == "Michael Jackson")

# Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
MichaelJackson$songtitle[MichaelJackson$Top10 == 1]

# What are the values of timesignature that occur in our dataset? Which is the most frequent?
table(songs$timesignature)

# What is the song with the highest tempo?
which.max(songs$tempo)
songs$songtitle[6206]


## Creating Our Prediction Model

# split the data in a test set for the year 2010 and a trainning set with the rest of the data
train <- subset(songs, year <= 2009)
test <- subset(songs, year == 2010)

# exclude some of the variables in our dataset from being used as independent variables
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
train = train[ , !(names(train) %in% nonvars) ]
test = test[ , !(names(test) %in% nonvars) ]

# Build a logistic regression model to predict Top10 using all of the other variables
SongsLog1 <- glm(Top10 ~ ., data= train, family=binomial)
summary(SongsLog1)


## Beware of Multicollinearity Issues!

# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(train$loudness, train$energy)

# Create Model 2, which is Model 1 without the independent variable "loudness".
SongsLog2 <- glm(Top10 ~ .-loudness, data= train, family=binomial)
summary(SongsLog2)

# Now, create Model 3, which should be exactly like Model 1, but without the variable "energy".
SongsLog3 <- glm(Top10 ~ .-energy, data= train, family=binomial)
summary(SongsLog3)


## Validating Our Model

# Make predictions on the test set using Model 3
predTest <- predict(SongsLog3, type = 'response', newdata = test)

# What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
table(test$Top10, predTest > 0.45)
(309 + 19)/(309 + 5 + 40 + 19)

# What would the accuracy of the baseline model be on the test set?
(309 + 5)/(309 + 5 + 40 + 19)

# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
19/(40 + 19)

# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
309/(309 + 5)
