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
index_headers <- c("Symbol", "Company", "Weight", "Sector", "Shared Held")
exchange_headers <- c("Symbol", "Company", "Last Sale Price", "Market Capital", 
                      "IPO", "Sector", "Industry")

shinyServer(function(input, output) {
   stock_choices <- reactive({
     switch(input$stockType,
            "index" = stock_indexes,
            "exchange" = stock_exchanges)
   })
   # return the correct stock based on stock type
   stocks_df <- reactive({
     switch(input$stockType,
            "index" = tq_index(input$stock),
            "exchange" = tq_exchange("AMEX"))
   })
   
   # return the correct table headers based on stock type
   stocks_df_headers <- reactive({
     switch(input$stockType,
            "index" = index_headers,
            "exchange" = exchange_headers)
   })
   
   # render available stocks subtypes for main type
   output$stock <- renderUI({
     selectInput("stock", label = h3("Select Stock"), choices = stock_choices())
   })
   
   output$stocks <- DT::renderDataTable({
     if (is.null(input$stock))
       return()
     current_stocks <- stocks_df ()
     current_stocks$sector <- as.factor(current_stocks$sector)
     if ("industry" %in% colnames(current_stocks)) {
       current_stocks$industry <- as.factor(current_stocks$industry)
     }
     stocks_Dt <- DT::datatable(data = current_stocks, 
                   options = list(columnDefs = list(list(
                     targets = 1,
                     render = JS(
                       "function(data, type, row, meta) {",
                       "return type === 'display' && data.length > 25 ?",
                       "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                       "}")
                   )),
                   pageLength = 10, orderClasses = TRUE), 
                   rownames = FALSE,
                   selection = "single",
                   colnames = stocks_df_headers(),
                   filter = "top")
     if ("weight" %in% colnames(current_stocks)) {
       return(stocks_Dt %>% 
                DT::formatRound("weight", 6))
     }
     return(stocks_Dt)
   })
})
