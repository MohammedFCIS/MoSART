#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyquant)

stock_indexes <- c("Russell 1000" = "RUSSELL1000", "Russell 2000" = "RUSSELL2000",
                   "Russell 3000" = "RUSSELL3000", "DOW" = "DOW",
                   "DOW Global" = "DOWGLOBAL", "S&P 400" = "SP400",
                   "S&P 500" = "SP500", "S&P 600" = "SP600",
                   "S&P 1000" = "SP1000")
stock_exchanges <- c("AMEX" = "AMEX", "NASDAQ" = "NASDAQ", "NYSE" = "NYSE")

shinyServer(function(input, output) {
   stock_choices <- reactive({
     switch(input$stockType,
            "index" = stock_indexes,
            "exchange" = stock_exchanges)
   })
   
   stocks_df <- reactive({
     switch(input$stockType,
            "index" = tq_index(input$stock),
            "exchange" = tq_exchange("AMEX"))
   })
   
   output$stock <- renderUI({
     selectInput("stock", label = h3("Select Stock"), choices = stock_choices())
   })
   
   output$stocks <- DT::renderDataTable({
     if (is.null(input$stock))
       return()
     DT::datatable(data = stocks_df (), 
                   options = list(pageLength = 10), 
                   rownames = FALSE)
   })
})
