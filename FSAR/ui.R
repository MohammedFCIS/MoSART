#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyquant)

# popuplate the selections below
# switch input based on the radio button
# display datatable with the output
## Refs:
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ01-core-functions-in-tidyquant.html
#https://github.com/MohammedFCIS/shinyapps-tutorials/blob/master/Movie-Rating/app.R
#http://shiny.rstudio.com/gallery/update-input-demo.html
#http://shiny.rstudio.com/gallery/dynamic-ui.html
#https://gallery.shinyapps.io/076-widget-select/
stock_indexes <- c()
stock_exchanges <- c()
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Retrieve Consolidated Symbol Data"),
  

  sidebarLayout(
    sidebarPanel(
      radioButtons("stockType", label = h3("Please select stock type"),
                   choices = list("Stock Indexes" = 1, "Stock Exchanges" = 2), 
                   selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       
    )
  )
))
