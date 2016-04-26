### Lecture 1: Working with  data ###


# look at data frame structure
str(WHO)
summary(WHO)

# subset data frame
WHO_europe <- WHO[WHO$Region == "Europe",]
str(WHO_europe)

# write a csv file
write.csv(WHO_europe, "WHO_Europe.csv")

# Remove data
rm(WHO_europe)

# preforme summaries of a variable
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)

# find minimum value
which.min(WHO$Under15)
WHO$Country[86]

#find maximum value
which.max(WHO$Under15)
WHO$Country[124]

# create scatterplot
plot(WHO$GNI, WHO$FertilityRate)

# subset outliers
outliers <- subset(WHO, GNI > 10000 & FertilityRate > 2.5)

# select only useful info
outliers[c("Country", "GNI", "FertilityRate")]

# find mean value
mean(WHO$Over60)

# find minimum value
which.min(WHO$Over60)
WHO$Country[183]

# find country with maximum literacy
which.max(WHO$LiteracyRate)
WHO$Country[44]

# create histogram of cellular use
hist(WHO$CellularSubscribers)

#boxplot life expectancy by region
boxplot(WHO$LifeExpectancy ~ WHO$Region, ylab ="Life Expectancy", main = "Life Expectancy by region")

#create counts table
table(WHO$Region)

# table of mean percentage of people over 60 by region
tapply(WHO$Over60, WHO$Region, mean)

# same for minimun life expectancy with removing missing values
tapply(WHO$LifeExpectancy, WHO$Region, min, na.rm=TRUE )

# same for mean child mortality
tapply(WHO$ChildMortality, WHO$Region, mean)
