### Recitation 1: Working with data ###

## Understanding Food ##

# check the stucture and summary of a food dataset
str(usda)
summary(usda)

# find the sodium maximo
which.max(usda$Sodium)
usda$Description[265]

# create data frame with foods with high sodium content
HighSodium <- subset(usda, Sodium > 10000)

#find caviar
usda$Sodium[match("CAVIAR", usda$Description)]

# create summary for sodium in food
summary(usda$Sodium)
sd(usda$Sodium, na.rm=TRUE)


# create plot of protein vs fat
plot(usda$Protein, usda$TotalFat, xlab="Protein", ylab='Fat', main='Protein vs Fat', col='red')

#create histogram of vit-C
hist(usda$VitaminC, 
     xlab = "Vitamin C (mg)", 
     main = 'Histogram of Vitamin C Levels', 
     xlim = c(0, 100),
     breaks = 2000)

# create boxplot for sugar
boxplot(usda$Sugar, 
        main = "Boxplot of Sugar Levels",
        ylab = "Sugar (gr)")

# is the firsy food in the data set more rich in sodium than average?
usda$Sodium[1] > mean(usda$Sodium, na.rm=TRUE)

# create a vector high sodium
HighSodium <- usda$Sodium > mean(usda$Sodium, na.rm=TRUE)
str(HighSodium)

# change data type of this vector to numeric
usda$HightSodium <- as.numeric(HighSodium)
str(usda$HightSodium)

# create variable of high protain
usda$HightProtain <- as.numeric(usda$Protein > mean(usda$Protein, na.rm=TRUE))

# create variable of high fat
usda$HightFat <- as.numeric(usda$TotalFat > mean(usda$TotalFat, na.rm=TRUE))

# create variable of high carbohydrate
usda$HightCarbs <- as.numeric(usda$Carbohydrate > mean(usda$Carbohydrate, na.rm=TRUE)) 

str(usda)

# how many food have hight sodium
table(usda$HightSodium)

# how many food have hight sodium and fat
table(usda$HightSodium, usda$HightFat)

# calculate the average amount of iron for low and high protain
tapply(usda$Iron, usda$HightProtain, mean, na.rm=TRUE)

# calculate the maximum amount of vitamin c for low and high carbs
tapply(usda$VitaminC, usda$HightCarbs, max, na.rm=TRUE)

# calculate the summary of vitamin c for low and high carbs
tapply(usda$VitaminC, usda$HightCarbs, summary, na.rm=TRUE)
