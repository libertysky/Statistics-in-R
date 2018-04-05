# Batch Fund Download
# calcinv: 09/19/2013
 
# Import tseries and zoo libraries
library(zoo)
library(quantmod)
library(xts)

#library(tseries)
 
# Uncomment the setInternet2 line if a proxy is required
# setInternet2(TRUE)
 
# Set Start Date
startdate = "2013-10-18"
 
# Extract tickers and fund weights
ticks <- c("XLK", "MDY", "SPY", "EEM", "IEF")
 
# Setup Equity Fund Variables
funds <- NULL
 
# Download equity fund data
for(i in 1:length(ticks)){
# Download Adjusted Price Series from Yahoo! Finance
prices <- get.hist.quote(ticks[i], quote="Adj", start=startdate, retclass="zoo")
 
# Convert daily closing prices to monthly closing prices
monthly.prices <- aggregate(prices, as.yearmon, tail, 1)
 
# Convert selected monthly prices into monthly returns to run regression
r <- diff(log(monthly.prices))  # convert prices to log returns
r1 <- exp(r)-1                  # back to simple returns
 
# Now shift out of zoo object into ordinary matrix
rj <- coredata(r1)
 
# Put fund returns into matrix
funds <- cbind(funds, rj)
}
 
fundfile <- cbind(as.character(index(r1)),funds)
header <- c("Dates",as.character(ticks))
fundfile <- rbind(header,fundfile)
 
returns<-matrix(funds)
# covar<-cov(returns)
 #avg<-colMeans(returns)

 # Write output data to csv file
#write.table(fundfile, file="fundreturns.csv", sep=",", row.names=FALSE,col.names=FALSE)
