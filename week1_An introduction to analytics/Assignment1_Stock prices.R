### Assignment 1: STOCK DYNAMICS ###

# change date variable to be a date
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

Procter$Date = as.Date(Procter$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# what is mean stock price of IBM?
mean(IBM$StockPrice)

# What is the minimum stock price of General Electric (GE) over this time period?
summary(GE$StockPrice)

# What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola$StockPrice)

# What is the median stock price of Boeing over this time period?
summary(Boeing$StockPrice)

# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(Procter$StockPrice)

# Around what year did Coca-Cola has its highest stock price in this time period?
plot(CocaCola$Date, CocaCola$StockPrice)
# add a line for procter data
lines(Procter$Date, Procter$StockPrice, col='red')
# draw a vertical line
abline(v=as.Date(c("2000-03-01")), lwd=2)
#Which one was going up in 1983?
abline(v=as.Date(c('1983-03-01')), col = 'blue')

# This will plot the CocaCola stock prices from 1995 through 2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

# add Boeing line of different color
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="blue")

# add GE line of different color
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")

# add IBM line of different color
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="yellow")

# add Boeing line of different color
lines(Procter$Date[301:432], Procter$StockPrice[301:432], col="violet")

# compare october to november 1997
abline(v=as.Date(c('1997-09-01')), col="grey")
abline(v=as.Date(c('1997-11-30')), col = "grey")


# Use the tapply command to calculate the mean stock price of IBM, sorted by months.
tapply(IBM$StockPrice, months(IBM$Date), mean)

# compare the monthly averages to the overall average stock price
mean(IBM$StockPrice)

# calculate the mean stock price of GE, sorted by months.
tapply(GE$StockPrice, months(GE$Date), mean)

# calculate the mean stock price of Coca-Cola, sorted by months.
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
