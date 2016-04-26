### Assignment 1: An analytical detective ###

## Car theft in Chicago ##

# Look ate the summary and structure of the data
str(mvt)
summary(mvt)

# covert date
DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(mvt$Date)
mvt$Date <- DateConvert

# create new variables month and week day
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

#which month has the fewest motor vehicle thefts?
table(mvt$Month)

# which weekday has the most motor vehicle thefts?
table(mvt$Weekday)

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Arrest, mvt$Month)

# create a histogram of dates
hist(mvt$Date, breaks=100)

# create boxplot of arrest by date
boxplot(mvt$Date ~ mvt$Arrest)

# For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Year, mvt$Arrest)
Arrest2001 <- 2152/(18517 + 2152)
Arrest2007 <- 1212/(13068+ 1212)
Arrest2012 <- 550/(13542 + 550) 

# Top 5 locations for crime
sort(table(mvt$LocationDescription))

# Create a subset of the top 5 crime locations
Top5 <- subset(mvt, mvt$LocationDescription == 'STREET' |
                 mvt$LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' |
                 mvt$LocationDescription == 'GAS STATION' |
                 mvt$LocationDescription == 'ALLEY'|
                 mvt$LocationDescription == 'DRIVEWAY - RESIDENTIAL')
str(Top5)

# One of the locations has a much higher arrest rate than the other locations. Which is it? 
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

# arrest rates by location
alley <-  249/(249 + 2059)
driveway <- 132/(1543+132)
gas <- 439/(1672+439)
parking <- 1603/(13249+1603)
street <- 11595/(144969+11595)

# On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$Weekday[Top5$LocationDescription == 'GAS STATION'])

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(Top5$Weekday[Top5$LocationDescription == 'DRIVEWAY - RESIDENTIAL'])
