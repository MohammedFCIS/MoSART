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
