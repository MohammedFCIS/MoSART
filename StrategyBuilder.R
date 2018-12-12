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
##############################################################
# Signals
##############################################################

# Add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
add.signal(strategy.st, name = "sigComparison", # user param
           
           # We are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), # user param
                            
                            # Particularly, we are interested when the SMA50 is greater than the SMA200
                            relationship = "gt"), # user param
           
           # Label this signal longfilter
           label = "longfilter") # user param

# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           
           # We're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            
                            # The relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),
           
           # Label it filterexit
           label = "filterexit")

# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           
           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            
                            # The threshold is 20
                            threshold = 20, 
                            
                            # We want the oscillator to be under this value
                            relationship = "lt", 
                            
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           
           # Label it longthreshold
           label = "longthreshold")
# add rest of signals the same way

##############################################################
# rules 
##############################################################
# Fill in the rule's type as exit
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")
# Create an entry rule of 1 share when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal", 
         
         # Use the longentry column as the sigcol
         arguments=list(sigcol = "longentry", 
                        
                        # Set sigval to TRUE
                        sigval = TRUE, 
                        
                        # Set orderqty to 1
                        orderqty = 1,
                        
                        # Use a market type of order
                        ordertype = "market",
                        
                        # Take the long orderside
                        orderside = "long",
                        
                        # Do not replace other signals
                        replace = FALSE, 
                        
                        # Buy at the next day's opening price
                        prefer = "Open"),
         
         # This is an enter type rule, not an exit
         type = "enter")
