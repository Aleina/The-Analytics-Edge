### Assigment 1: DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES ###

# Look at data structcure and summary
str(CPS)
summary(CPS)

# which state with more and less intervewees
sort(table(CPS$State))

# proportion of usa citizens
table(CPS$Citizenship)
(116639+7073)/nrow(CPS)

# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic)

# Are the missing values in marital status raleted to the region?
table(is.na(CPS$Married), CPS$Region)
# and with sex?
table(is.na(CPS$Married), CPS$Sex)
# and with age?
table(CPS$Age, is.na(CPS$Married))
# and with citizenship
table(CPS$Citizenship, is.na(CPS$Married))

# How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
table(CPS$State, is.na(CPS$MetroAreaCode))

# Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)


## Merge the CPS data frame with the codes map for MetroAreaCode
CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# Review CPS data frame
summary(CPS)
str(CPS)

# Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea))

# Which of the following metropolitan areas has the largest number of interviewees?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean))

# Which metropolitan area has the smallest proportion of interviewees who have received no high school diploma?
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))


# Integrating Country of Birth Data
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

#Review CPS data frame
summary(CPS)
str(CPS)

# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))

# proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States?
sort(tapply(CPS$MetroArea.x=="New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country.x!="United States", sum, na.rm=TRUE))
1668/(1668+3736)

# Which metropolitan area has the largest number of interviewees with a country of birth in India?
sort(tapply(CPS$Country.x=="India", CPS$MetroArea.x, sum, na.rm=TRUE))

# and from Brazil?
sort(tapply(CPS$Country.x=="Brazil", CPS$MetroArea.x, sum, na.rm=TRUE))

#and from Somalia?
sort(tapply(CPS$Country.x=="Somalia", CPS$MetroArea.x, sum, na.rm=TRUE))
