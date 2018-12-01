#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)




# complete user selections
# data table features
## Refs:
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ01-core-functions-in-tidyquant.html
#https://github.com/MohammedFCIS/shinyapps-tutorials/blob/master/Movie-Rating/app.R
#http://shiny.rstudio.com/gallery/dynamic-ui.html
#https://gallery.shinyapps.io/076-widget-select/

# Define UI for application that draws a histogram
stock_choices <- c("Stock Indexes" = "index", "Stock Exchanges" = "exchange")
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Retrieve Consolidated Symbol Data"),
  

  sidebarLayout(
    sidebarPanel(
      radioButtons("stockType", label = h3("Select Stock Type"),
                   choices = stock_choices),
      uiOutput("stock")
                  
    ),
    
    # returned stocks
    mainPanel(
      dataTableOutput("stocks")
    )
  )
))
