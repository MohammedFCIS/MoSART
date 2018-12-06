#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("XLConnect")
library(shiny)
library(tidyquant)
library(alphavantager)
library(Quandl)
library(XML)

# add chart series setup (its different parameters and options)
## https://www.quantmod.com/examples/charting/
## https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
## https://rpubs.com/Felix/6653

# add zooming feature
## https://www.quantmod.com/documentation/zoomChart.html
## https://stackoverflow.com/questions/9137350/using-rechart-in-quantmod
## https://stackoverflow.com/questions/42131839/r-zoomchart-shiny
## https://rdrr.io/rforge/quantmod/src/R/zoomChart.R
## https://stackoverflow.com/questions/42270664/r-shiny-quantmod-zoomchart-and-fixed-coloring-of-points
## https://stackoverflow.com/questions/53150221/quanstrat-with-shiny
## https://groups.google.com/forum/#!topic/shiny-discuss/vR7Jx8OfwSg

# add indicators
## https://www.quantmod.com/examples/charting/
## https://stackoverflow.com/questions/45984122/r-addta-function-in-shiny-app

# add the rest of analysis
## https://www.business-science.io/investments/2016/10/23/SP500_Analysis.html

Quandl.api_key("2AxuBQTEuzWdH_rFH-y9")
av_api_key("JEMUK6SHIYMEVJKW")

stock_indexes <-
  c(
    "Russell 1000" = "RUSSELL1000",
    "Russell 2000" = "RUSSELL2000",
    "Russell 3000" = "RUSSELL3000",
    "DOW" = "DOW",
    "DOW Global" = "DOWGLOBAL",
    "S&P 400" = "SP400",
    "S&P 500" = "SP500",
    "S&P 600" = "SP600",
    "S&P 1000" = "SP1000"
  )
stock_exchanges <-
  c("AMEX" = "AMEX",
    "NASDAQ" = "NASDAQ",
    "NYSE" = "NYSE")
index_headers <-
  c("Symbol", "Company", "Weight", "Sector", "Shared Held")
exchange_headers <-
  c("Symbol",
    "Company",
    "Last Sale Price",
    "Market Capital",
    "IPO",
    "Sector",
    "Industry")

shinyServer(function(input, output, session) {
  stock_choices <- reactive({
    switch(input$stockType,
           "index" = stock_indexes,
           "exchange" = stock_exchanges)
  })
  # return the correct stocks based on stock type
  stocks_df <- reactive({
    if (is.null(input$stockType)) {
      return()
    }
    switch(
      input$stockType,
      "index" = tq_index(input$stock),
      "exchange" = tq_exchange("AMEX")
    )
  })
  
  # return the correct table headers based on stock type
  stocks_df_headers <- reactive({
    switch(input$stockType,
           "index" = index_headers,
           "exchange" = exchange_headers)
  })
  
  # render available stocks subtypes for main type
  output$stock <- renderUI({
    selectInput("stock",
                label = h3("Select Stock"),
                choices = stock_choices())
  })
  
  output$stocks <- DT::renderDataTable({
    if (is.null(input$stock))
      return()
    current_stocks <- stocks_df ()
    current_stocks$sector <- as.factor(current_stocks$sector)
    if ("industry" %in% colnames(current_stocks)) {
      current_stocks$industry <- as.factor(current_stocks$industry)
    }
    stocks_Dt <- DT::datatable(
      data = current_stocks,
      extensions = c("Buttons"),
      options = list(
        columnDefs = list(list(
          targets = 1,
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 25 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
            "}"
          )
        )),
        pageLength = 10,
        orderClasses = TRUE,
        dom = 'Bfrtip',
        buttons =  list(
          'copy',
          'print',
          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          )
        )
      ),
      rownames = FALSE,
      selection = "single",
      colnames = stocks_df_headers(),
      filter = "top"
    )
    if ("weight" %in% colnames(current_stocks)) {
      return(stocks_Dt %>%
               DT::formatRound("weight", 6))
    }
    return(stocks_Dt)
  })
  
  selected_stock <-
    reactive(if (!is.null(input$stocks_rows_selected)) {
      stocks_df()[input$stocks_rows_selected, "symbol"]
    })
  
  output$stock_actions <- renderUI({
    if (length(selected_stock()) > 0 && !is.null(selected_stock())) {
      span(
        actionButton("jumpToPrices", "Prices"),
        actionButton("jumpToRatios", "Key Ratios"),
        actionButton("jumpToStats", "Key Stats"),
        actionButton("jumpToFinanceStatement", "Finance Statement")
      )
    }
    
  })
  
  observeEvent(input$jumpToPrices, {
    updateTabsetPanel(session = session,
                      inputId =  "mosart",
                      selected = "stockPrices")
  })
  
  observeEvent(input$jumpToRatios, {
    updateTabsetPanel(session = session,
                      inputId =  "mosart",
                      selected = "keyRatios")
  })
  
  observeEvent(input$jumpToStats, {
    updateTabsetPanel(session = session,
                      inputId =  "mosart",
                      selected = "keyStats")
  })
  
  observeEvent(input$jumpToFinanceStatement, {
    updateTabsetPanel(session = session,
                      inputId =  "mosart",
                      selected = "financeStatement")
  })
  
  # return the correct stock prices based on stock source
  stock_pricess_df <- reactive({
    switch(
      input$pricesSource,
      "yahoo" = {
        stock_df <- tq_get(selected_stock(), get = "stock.prices")
        return(xts(stock_df[-1], order.by = stock_df$date))
      },
      "qundl" = {
        stock_df <- tq_get(paste("WIKI", "/", selected_stock(), sep = ""),
                           get = "quandl")
        return(xts(stock_df[-1], order.by = stock_df$date))
      },
      "alphavantage" = {
        stock_df <- tq_get(
          selected_stock(),
          get = "alphavantager",
          av_fun = "TIME_SERIES_INTRADAY",
          interval = "5min"
        )
        return(xts(stock_df[-1], order.by = stock_df$timestamp))
      }
    )
  })
  
  output$stock_prices <- DT::renderDataTable({
    stocks_Dt <- DT::datatable(
      data = stock_pricess_df(),
      caption = paste(
        selected_stock(),
        "Prices from",
        min(index(stock_pricess_df())),
        "to",
        max(index(stock_pricess_df()))
      )
    )
  })
  
  output$prices_plot <-
    renderPlot({
      print(chartSeries(
        stock_pricess_df(),
        type = input$pricesChartType,
        theme = chartTheme(input$pricesChartTheme),
        name = paste(selected_stock(), "chart"),
        show.grid = input$priceChartGrid,
        log.scale = input$priceLogScale,
        TA = NULL
      ))
      for(ind in input$indicators) {
        switch (ind,
          "addVo" = print(addVo()),
          "addADX" = print(addADX()),
          "addATR" = print(addATR())
        )
      }
    })
  
  #   renderPlot({
  #   plot(
  #     stock_pricess_df() %>% ggplot(aes(
  #       x = date, y = close, volume = volume
  #     )) +
  #       geom_candlestick(aes(
  #         open = open,
  #         high = high,
  #         low = low,
  #         close = close
  #       )) +
  #       geom_ma(
  #         ma_fun = VWMA,
  #         n = 15,
  #         wilder = TRUE,
  #         linetype = 5
  #       ) +
  #       geom_ma(
  #         ma_fun = VWMA,
  #         n = 50,
  #         wilder = TRUE,
  #         color = "red"
  #       ) +
  #       theme_tq()
  #   )
  # })
  #
  # return thestock key ratios
  stock_key_ratios <- reactive({
    tq_get(selected_stock(), get = "key.ratios")
  })
  
  output$stock_ratios_financials <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[1, 2]])
  })
  
  output$ratios_financials_plot <- renderPlot({
    plot(stock_key_ratios()[[1, 2]] %>%
           ggplot(aes(x = date, y = value)) +
           geom_line(aes(col = factor(category)), size = 1)  +
           theme_tq())
  })
  
  output$stock_ratios_profitability <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[2, 2]])
  })
  
  output$ratios_profitability_plot <- renderPlot({
    plot(stock_key_ratios()[[2, 2]] %>%
           ggplot(aes(x = date, y = value)) +
           geom_line(aes(col = factor(category)), size = 1)  +
           theme_tq())
  })
  
  output$stock_ratios_growth <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[3, 2]])
  })
  
  output$ratios_growth_plot <- renderPlot({
    plot(stock_key_ratios()[[3, 2]] %>%
           ggplot(aes(x = date, y = value)) +
           geom_line(aes(col = factor(category)), size = 1)  +
           theme_tq())
  })
  
  output$stock_ratios_cash_flow <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[4, 2]])
  })
  
  output$ratios_cash_flow_plot <- renderPlot({
    plot(
      stock_key_ratios()[[4, 2]] %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(col = factor(category)), size = 1)  +
        theme_tq() +
        scale_color_tq()
    )
  })
  
  output$stock_ratios_financial_health <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[5, 2]])
  })
  
  output$ratios_financial_health_plot <- renderPlot({
    plot(stock_key_ratios()[[5, 2]] %>%
           ggplot(aes(x = date, y = value)) +
           geom_line(aes(col = factor(category)), size = 1)  +
           theme_tq())
  })
  
  output$stock_ratios_efficiency <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[6, 2]])
  })
  
  output$ratios_efficiency_plot <- renderPlot({
    plot(
      stock_key_ratios()[[6, 2]] %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(col = factor(category)), size = 1)  +
        theme_tq() +
        scale_color_tq()
    )
  })
  
  output$stock_ratios_value_ratios <- DT::renderDataTable({
    DT::datatable(data = stock_key_ratios()[[7, 2]])
  })
  
  output$ratios_value_ratios_plot <- renderPlot({
    plot(
      stock_key_ratios()[[7, 2]] %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(col = factor(category)), size = 1)  +
        theme_tq() +
        scale_color_tq()
    )
  })
  
  # return stock finviz stats
  stock_key_stats_finviz_df <- reactive({
    url <- paste0("http://finviz.com/quote.ashx?t=", selected_stock())
    webpage <- readLines(url)
    html <-
      htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
    tableNodes <- getNodeSet(html, "//table")
    
    # ASSIGN TO STOCK NAMED DFS
    stats <- readHTMLTable(
      tableNodes[[9]],
      header = c(
        "data1",
        "data2",
        "data3",
        "data4",
        "data5",
        "data6",
        "data7",
        "data8",
        "data9",
        "data10",
        "data11",
        "data12"
      )
    )
  })
  
  # return stock yahoo stats
  stock_key_stats_yahoo_df <- reactive({
    url <-
      paste('https://finance.yahoo.com/quote/HD/analysts?p=',
            selected_stock(),
            sep = "")
    webpage <- readLines(url)
    html <-
      htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
    tableNodes <- getNodeSet(html, "//table")
    
    list(
      readHTMLTable(tableNodes[[1]]),
      readHTMLTable(tableNodes[[2]]),
      readHTMLTable(tableNodes[[3]]),
      readHTMLTable(tableNodes[[4]]),
      readHTMLTable(tableNodes[[5]]),
      readHTMLTable(tableNodes[[6]])
    )
  })
  
  output$stock_key_stats_finviz <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_finviz_df())
  })
  
  output$stock_key_stats_earning_estimates <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_yahoo_df()[[1]], caption = "Earning Estimates")
  })
  
  output$stock_key_stats_earning_history <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_yahoo_df()[[3]], caption = "Earning History")
  })
  
  output$stock_key_stats_revenue_estimates <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_yahoo_df()[[2]], caption = "Revenue Estimates")
  })
  
  output$stock_key_stats_eps_trend <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_yahoo_df()[[4]], caption = "EPS Trend")
  })
  
  output$stock_key_stats_eps_revisions <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_yahoo_df()[[5]], caption = "EPS Revisions")
  })
  
  output$stock_key_stats_growth_est <- DT::renderDataTable({
    DT::datatable(data = stock_key_stats_yahoo_df()[[6]], caption = "Growth Estimates")
  })
  
  # return stock finance statments
  stock_finance_statements <- reactive({
    getFin(selected_stock())
  })
  
  output$stock_finance_statement_is <- DT::renderDataTable({
    DT::datatable(data = stock_finance_statements()$IS, caption = "IS")
  })
  
  output$stock_finance_statement_bs <- DT::renderDataTable({
    DT::datatable(data = stock_finance_statements()$BS, caption = "BS")
  })
  
  output$stock_finance_statement_cf <- DT::renderDataTable({
    DT::datatable(data = stock_finance_statements()$CF, caption = "CF")
  })
  
})

getFin <- function(stock) {
  if ("rvest" %in% installed.packages()) {
    library(rvest)
  } else{
    install.packages("rvest")
    library(rvest)
  }
  for (i in 1:length(stock)) {
    tryCatch({
      url <- "https://finance.yahoo.com/quote/"
      url <- paste0(url, stock[i], "/financials?p=", stock[i])
      wahis.session <-
        html_session(url)
      p <-    wahis.session %>%
        html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table') %>%
        html_table(fill = TRUE)
      IS <- p[[1]]
      colnames(IS) <- paste(IS[1, ])
      IS <- IS[-c(1, 5, 12, 20, 25), ]
      names_row <- paste(IS[, 1])
      IS <- IS[, -1]
      IS <- apply(IS, 2, function(x) {
        gsub(",", "", x)
      })
      IS <- as.data.frame(apply(IS, 2, as.numeric))
      rownames(IS) <- paste(names_row)
      temp1 <- IS
      url <- "https://finance.yahoo.com/quote/"
      url <-
        paste0(url, stock[i], "/balance-sheet?p=", stock[i])
      wahis.session <- html_session(url)
      p <-    wahis.session %>%
        html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table') %>%
        html_table(fill = TRUE)
      BS <- p[[1]]
      colnames(BS) <- BS[1, ]
      BS <- BS[-c(1, 2, 17, 28), ]
      names_row <- BS[, 1]
      BS <- BS[, -1]
      BS <- apply(BS, 2, function(x) {
        gsub(",", "", x)
      })
      BS <- as.data.frame(apply(BS, 2, as.numeric))
      rownames(BS) <- paste(names_row)
      temp2 <- BS
      url <- "https://finance.yahoo.com/quote/"
      url <- paste0(url, stock[i], "/cash-flow?p=", stock[i])
      wahis.session <- html_session(url)
      p <-    wahis.session %>%
        html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table') %>%
        html_table(fill = TRUE)
      CF <- p[[1]]
      colnames(CF) <- CF[1, ]
      CF <- CF[-c(1, 3, 11, 16), ]
      names_row <- CF[, 1]
      CF <- CF[, -1]
      CF <- apply(CF, 2, function(x) {
        gsub(",", "", x)
      })
      CF <- as.data.frame(apply(CF, 2, as.numeric))
      rownames(CF) <- paste(names_row)
      temp3 <- CF
      return(list(IS = temp1, BS = temp2, CF = temp3))
      
    },
    error = function(cond) {
      message(stock[i], "Give error ", cond)
    })
  }
}
