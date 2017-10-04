# INGENIERÍA FINANCIERA
#################################################
# Se cargan las librerias o paquetes que se necesitan

install.packages('quantmod')
library("quantmod")

#########################################################################
# Apple
# Download Current Stock Quote

getQuote("AAPL")

# Load and Manage Data from Multiple Sources

getSymbols("AAPL") 

# getSymbols("SPY", from = "2000-01-01", to = "2016-06-30", src =  "yahoo", adjust =  TRUE)

View(AAPL)

# daily,weekly,monthly,quarterly, and yearly 
dailyReturn(AAPL, type='arithmetic') # returns by day 
weeklyReturn(AAPL, type='arithmetic') # returns by week 
monthlyReturn(AAPL, type='arithmetic') # returns by month, indexed by yearmon 

allReturns(AAPL, type='arithmetic') # note the plural
allReturns(AAPL, type='log') # note the plural

#Charting
barChart(AAPL)
candleChart(AAPL,multi.col=TRUE,theme="white") 
addMACD() 
addBBands() 

# Return and Volatility I
dAAPL <- dailyReturn(AAPL, type='log')
vol <- sd(dAAPL)*sqrt(252)

# Return and Volatility II
ret <-  log(AAPL$AAPL.Adjusted) - log(lag(AAPL$AAPL.Adjusted)) 
vol <- sd(ret, na.rm=TRUE)*sqrt(252)

# Black-Scholes Option Value
blackscholes <- function(S, X, r, T, sigma) {
  price <- c(2)
  d1 <- (log(S/X)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  price[1] <- S*pnorm(d1) - X*exp(-r*T)*pnorm(d2)
  price[2] <- X*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  price
}

# Black-Scholes
blackscholes(150,160,0.05,1,vol)
