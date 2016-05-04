### Assignment 3.4: PREDICTING THE BASEBALL WORLD SERIES CHAMPION ###


## Limiting to Teams Making the Playoffs ##

# How many team/year pairs are there in the whole dataset?
str(baseball)

# Identify the total number of years included in this dataset.
length(table(baseball$Year))
 
# replace baseball with a data frame limited to teams that made the playoffs
baseball <- subset(baseball, Playoffs == 1)

# How many team/year pairs are included in the new dataset?
str(baseball)

# Which of the following has been the number of teams making the playoffs in some season?
PlayoffTable <- (table(baseball$Year))


## Adding an Important Predictor ##

# What best describes the output of names(PlayoffTable)?
names(PlayoffTable)

# Which function call returns the number of playoff teams in 1990 and 2001?
PlayoffTable[c('1990', '2001')]

# look up the number of teams in the playoffs for each team/year pair in the dataset, and store it as a new variable
baseball$NumCompetitors <- PlayoffTable[as.character(baseball$Year)]

# How many playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs?
table(baseball$NumCompetitors)


## Bivariate Models for Predicting World Series Winner ##

# Add a variable named WorldSeries to the baseball data frame of wether or not they win the world series
baseball$WorldSeries <- as.numeric(baseball$RankPlayoffs == 1)

# How many observations do we have in our dataset where a team did NOT win the World Series?
table(baseball$WorldSeries)


## build bivariate models that predict the outcome using a single independent variable
# year
model1 <- glm(WorldSeries ~ Year, data = baseball, family = 'binomial')
summary(model1)

# Runs Scores
model2 <- glm(WorldSeries ~ RS, data = baseball, family = 'binomial')
summary(model2)

# Runs Allowed
model3 <- glm(WorldSeries ~ RA, data = baseball, family = 'binomial')
summary(model3)

# Wins
model4 <- glm(WorldSeries ~ W, data = baseball, family = 'binomial')
summary(model4)

# On Base Percentage
model5 <- glm(WorldSeries ~ OBP, data = baseball, family = 'binomial')
summary(model5)

# slugging percentage
model6 <- glm(WorldSeries ~ SLG, data = baseball, family = 'binomial')
summary(model6)

# batting average
model7 <- glm(WorldSeries ~ BA, data = baseball, family = 'binomial')
summary(model7)

# Rank Season
model8 <- glm(WorldSeries ~ RankSeason, data = baseball, family = 'binomial')
summary(model8)

# Opponent on base percentage
model9 <- glm(WorldSeries ~ OOBP, data = baseball, family = 'binomial')
summary(model9)

# Opponent slugging percentage
model10 <- glm(WorldSeries ~ OSLG, data = baseball, family = 'binomial')
summary(model10)

# Number of Competitors
model11 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = 'binomial')
summary(model11)

# League
model12 <- glm(WorldSeries ~ League, data = baseball, family = 'binomial')
summary(model12)


## Multivariate Models for Predicting World Series Winner ##

# Build a multivariate model that combine all the variables we found to be significant in bivariate models
multi.model <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = 'binomial')
summary(multi.model)

# Which of the following variable pairs have a high degree of correlation?
cor(baseball$Year, baseball$RA)
cor(baseball$Year, baseball$RankSeason)
cor(baseball$Year, baseball$NumCompetitors)
cor(baseball$RA, baseball$RankSeason)
cor(baseball$RA, baseball$NumCompetitors)
cor(baseball$RankSeason, baseball$NumCompetitors)

## Build all six of the two variable models listed in the previous problem
# Year/RA
model2.1 <- glm(WorldSeries ~ Year + RA, data = baseball, family = 'binomial')
summary(model2.1)

# Year/RankSeason
model2.2 <- glm(WorldSeries ~ Year + RankSeason, data = baseball, family = 'binomial')
summary(model2.2)

# Year/NumCompetitors
model2.3 <- glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = 'binomial')
summary(model2.3)

# RA/RankSeason
model2.4 <- glm(WorldSeries ~ RA + RankSeason, data = baseball, family = 'binomial')
summary(model2.4)

# RA/NumCompetitors
model2.5 <- glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = 'binomial')
summary(model2.5)

# RankSeason/NumCompetitors
model2.6 <- glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = 'binomial')
summary(model2.6)
