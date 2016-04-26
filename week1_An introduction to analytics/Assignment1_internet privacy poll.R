### Assignment 1: INTERNET PRIVACY POLL ###

# look at the data
summary(poll)
str(poll)

# look at smartphone use
table(poll$Smartphone)
summary(poll$Smartphone)
43 + 487 + 472

# Look at states in the Midwest census region?
table(poll$State, poll$Region)

# Which was the state in the South census region with the largest number of interviewees?
sort(table(poll$State[poll$Region=='South']))

# look at internet and smartphone use
table(poll$Internet.Use, poll$Smartphone)

# looking for missing values in internet and smartphone use
sum(is.na(poll$Internet.Use))
sum(is.na(poll$Smartphone))

# subset a data frame called "limited", which is limited to those who reported Internet or smartphone use
limited <- subset(poll, Internet.Use==1 | Smartphone==1)

# look at limited data frame
str(limited)
summary(limited)

# mean of pieces of info on internet
mean(limited$Info.On.Internet)

# how many reported no personal info on internet
sum(limited$Info.On.Internet==0)
# how many reported maximum value
sum(limited$Info.On.Internet==11)

# What proportion worry about how much information is available about them on the Internet?
table(limited$Worry.About.Info)
386/(386+404)

# What proportion think it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible)
278/(278+475)

# What proportion have tried masking their identity on the Internet?
table(limited$Tried.Masking.Identity)
128/(128+656)

# What proportion find United States privacy laws effective?
table(limited$Privacy.Laws.Effective)
186/(186+541)

# Build a histogram of the age of interviewees
hist(limited$Age)

# What is the largest number of interviewees that have  the same value in their Age AND Info.On.Internet variables
table(limited$Age, limited$Info.On.Internet)

# To avoid points covering each other up, we can use the jitter(). Experimenting with the command 
jitter(c(1, 2, 3))

# Now, plot Age against Info.On.Internet with jitter
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

# summary of the Info.On.Internet value, broken down by whether an interviewee is a smartphone user
tapply(limited$Info.On.Internet, limited$Smartphone, mean)

# break down the Tried.Masking.Identity variable for smartphone and non-smartphone users.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm=TRUE)
