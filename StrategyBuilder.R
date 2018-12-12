library(quantstrat)
getSymbols("AAPL", 
           adjust =  TRUE)
##############################################################
# Initialization settings
##############################################################
# These parameters will be fixed for now, except (from, to)
# params, create issue to add the rest later
# Create initdate, from, and to strings

# No need to add them
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- Sys.Date()

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD 
currency("USD")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Use stock() to initialize SPY and set currency to USD
stock("AAPL", currency = "USD")
# user para,s
# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000

# Define the names of your strategy, portfolio and account
# Remove the existing strategy if it exists
rm.strat("firststrat")

strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

# Initialize the portfolio
initPortf(portfolio.st, symbols = "AAPL", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

##############################################################
# Indicators
##############################################################
# Add a 200-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA", #indicator param
              
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 200), #indicator list of arguments
              
              # Label your indicator SMA200
              label = "SMA200")
# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 50), 
              
              # Label your indicator SMA50
              label = "SMA50")

# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the RSI 3 function
              name = "RSI",
              
              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              
              # Label your indicator RSI_3
              label = "RSI_3")
