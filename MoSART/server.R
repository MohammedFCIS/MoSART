#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("XLConnect")
library(tidyverse)
library(shiny)
library(tidyquant)
library(alphavantager)
library(Quandl)
library(XML)

# add chart series setup (its different parameters and options)
## https://www.quantmod.com/examples/charting/ (done)
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
## https://github.com/timelyportfolio/shiny-websockets/blob/master/shiny-chartseries.r

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
## keep track of inserted indicators and not yet removed
inserted_indicators <- c()

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
    switch(input$stockType,
           "index" = tq_index(input$stock),
           "exchange" = {
             if (!input$stock %in% tq_exchange_options()) {
               return(tq_exchange("AMEX"))
             }
             ex <- tq_exchange(input$stock)
             glimpse(ex)
             return(ex)
           })
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
    # current_stocks$sector <- as.factor(current_stocks$sector)
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
      stocks_df()[["symbol"]][input$stocks_rows_selected]
      
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
        getSymbols(selected_stock(), auto.assign = FALSE)
      },
      "qundl" = {
        Quandl(paste("WIKI", "/", selected_stock(), sep = ""),
                           type="xts")
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
  
  output$prices_plot <-
    renderPlot({
      #print(stock_pricess_df())
      print(
        chartSeries(
          stock_pricess_df(),
          type = input$pricesChartType,
          theme = chartTheme(input$pricesChartTheme),
          name = paste(selected_stock(), "chart"),
          show.grid = input$priceChartGrid,
          log.scale = input$priceLogScale,
          TA = NULL,
          subset    = paste(input$daterange, collapse = "::")
        )
      )
      for (ind in input$indicators) {
        switch (
          ind,
          "addVo" = print(addVo()),
          "addADX" = print(addADX()),
          "addATR" = print(addATR()),
          "addBBands" = print(addBBands()),
          "addBBands2" = print(addBBands(draw = "width")),
          "addBBands3" = print(addBBands(draw = "percent")),
          "addCCI" = print(addCCI()),
          "addCMF" = print(addCMF()),
          "addCMO" = print(addCMO()),
          "addDEMA" = print(addDEMA()),
          "addDPO" = print(addDPO()),
          "addEMA" = print(addEMA()),
          "addEnvelope" = print(addEnvelope()),
          "addEVWMA" = print(addEVWMA()),
          "addExpiry" = print(addExpiry()),
          "addMACD" = print(addMACD()),
          "addMomentum" = print(addMomentum()),
          "addROC" = print(addROC()),
          "addRSI" = print(addRSI()),
          "addSAR" = print(addSAR()),
          "addSMA" = print(addSMA()),
          "addSMI" = print(addSMI()),
          "addTRIX" = print(addTRIX()),
          "addWMA" = print(addWMA()),
          "addWPR" = print(addWPR()),
          "addZLEMA" = print(addZLEMA())
        )
      }
    })
  return_df <- reactive({
    switch (
      input$return_features,
      "high" = get_returns(
        stock.symbol = selected_stock(),
        from_to = paste(input$daterange_return, collapse = "::"),
        type = input$return_function,
        period = input$return_period,
        select = c("high")
      ),
      "open" = get_returns(
        stock.symbol = selected_stock(),
        from_to = paste(input$daterange_return, collapse = "::"),
        type = input$return_function,
        period = input$return_period,
        select = c("open")
      ),
      "low" = get_returns(
        stock.symbol = selected_stock(),
        from_to = paste(input$daterange_return, collapse = "::"),
        type = input$return_function,
        period = input$return_period,
        select = c("low")
      ),
      "close" = get_returns(
        stock.symbol = selected_stock(),
        from_to = paste(input$daterange_return, collapse = "::"),
        type = input$return_function,
        period = input$return_period,
        select = c("close")
      ),
      "adjusted" = get_returns(
        stock.symbol = selected_stock(),
        from_to = paste(input$daterange_return, collapse = "::"),
        type = input$return_function,
        period = input$return_period,
        select = c("adjusted")
      ),
      "volume" = get_returns(
        stock.symbol = selected_stock(),
        from_to = paste(input$daterange_return, collapse = "::"),
        type = input$return_function,
        period = input$return_period,
        select = c("volume")
      )
    )
  })
  
  output$return_plot <- renderPlot({
    return_df() %>%
      ggplot(aes(x = date_trans, y = returns)) +
      geom_hline(yintercept = 0, color = palette_light()[[1]]) +
      geom_point(size = 2, color = palette_light()[[3]]) +
      geom_line(size = 1, color = palette_light()[[3]]) +
      geom_smooth(method = "lm") +
      labs(
        title = paste(
          selected_stock(),
          paste(
            "Visualizing Trends in",
            firstup(input$return_period),
            "Returns"
          ),
          sep = ":"
        ),
        x = "",
        y = paste(firstup(input$return_period),
                  "Returns"),
        color = ""
      ) +
      theme_tq()
  })
  
  daily_xts <- reactive({
    stock <- selected_stock() %>%
      tq_get(get  = "stock.prices")
    
    xts(stock[-1], order.by = stock$date)
  })
  
  output$daily_return_plot <- renderPlot({
    daily_xts() %>%
      Ad() %>%
      dailyReturn(type = "log") %>%
      ggplot(aes(x = daily.returns)) +
      geom_histogram(bins = 100) +
      geom_density() +
      geom_rug(alpha = 0.5)
  })
  
  output$simulation_plot <- renderPlot({
    daily_log <- daily_xts() %>%
      dailyReturn(type = "log")
    mu <- mean(daily_log, na.rm = TRUE)
    sigma <- sd(daily_log, na.rm = TRUE)
    N <- input$days_num
    M <- input$sim_num
    
    day <- 1:N
    price_init <- daily_xts()$adjusted[[nrow(daily_xts()$adjusted)]]
    # Simulate prices
    set.seed(123)
    monte_carlo_mat <- matrix(nrow = N, ncol = M)
    for (j in 1:M) {
      monte_carlo_mat[[1, j]] <- price_init
      for (i in 2:N) {
        monte_carlo_mat[[i, j]] <-
          monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
      }
    }
    # Format and organize data frame
    price_sim <- cbind(day, monte_carlo_mat) %>%
      as_tibble()
    nm <- str_c("Sim.", seq(1, M))
    nm <- c("Day", nm)
    names(price_sim) <- nm
    price_sim <- price_sim %>%
      gather(key = "Simulation", value = "Stock.Price",-(Day))
    
    end_stock_prices <- price_sim %>%
      filter(Day == max(Day))
    probs <- c(.005, .025, .25, .5, .75, .975, .995)
    dist_end_stock_prices <-
      quantile(end_stock_prices$Stock.Price, probs = probs)
    dist_end_stock_prices %>% round(2)
    # Visualize simulation
    price_sim %>%
      ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) +
      geom_line(alpha = 0.1) +
      ggtitle(
        str_c(
          selected_stock(),
          "::",
          M,
          " Monte Carlo Simulations for Prices Over ",
          N,
          " Trading Days"
        )
      ) +
      labs(
        subtitle = paste(
          "The 95% confidence interval is between ",
          "$",
          round(dist_end_stock_prices[2], 2),
          " and $",
          round(dist_end_stock_prices[6], 2),
          " with a median (“most likely”) estimated",
          " price of $",
          round(dist_end_stock_prices[5], 2),
          sep = ""
        )
      ) +
      theme(
        plot.title = element_text(hjust = 0, size = 18),
        plot.subtitle = element_text(hjust = 0, size = 16)
      )
    
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
  
  observeEvent(input$add_indic, {
    req(input$strat_indicators)
    btn <- input$add_indic
    id <- paste0('ind', btn)
    indic_id <- paste0(input$strat_indicators, "_", id)
    insertUI(
      selector = '#indicators_placeholder',
      ui = tags$div(
        id = paste0("ind_div", id),
        fluidRow(
          column(
            width = 4,
            textInput(
              inputId = indic_id,
              label = "Indicator Name",
              placeholder = "Indicator Name"
            )
          ),
          column(
            width = 4,
            numericInput(paste0("n_", id),
                         label = "Calculations Period",
                         value = 14)
          ),
          column(
            width = 4,
            br(),
            actionButton(
              paste0("ind_remove_btn_", id),
              label = 'Remove',
              class = "btn-danger"
            )
          )
        )
      )
    )
    inserted_indicators <<- c(indic_id, inserted_indicators)
    observeEvent(input[[paste0("ind_remove_btn_", id)]], {
      shiny::removeUI(selector = paste0("#ind_div", id))
      inserted_indicators <<- inserted_indicators[!inserted_indicators %in% c(indic_id)]
      indicators_names <- c()
      for(ind in inserted_indicators){
        indicators_names <- c(input[[ind]], indicators_names)
      }
      updateSelectInput(session, inputId = "first_indic", choices = indicators_names)
      updateSelectInput(session, inputId = "second_indic", choices = indicators_names)
    })
    
    observeEvent(input[[indic_id]], {
      indicators_names <- c()
      for(ind in inserted_indicators){
        indicators_names <- c(input[[ind]], indicators_names)
      }
      updateSelectInput(session, inputId = "first_indic", choices = indicators_names)
      updateSelectInput(session, inputId = "second_indic", choices = indicators_names)
    })
  })
  
  observeEvent(input$predict_btn, {
    req(input$indicators)
    # build formula
    print("build formula")
    ## get stock
    print("get stock") 
    stock <- selected_stock()
    print(paste("The stock is:", stock))
    stock <- stock_pricess_df()
    stock_formula <- "Cl(stock) ~ Delt(Cl(stock),k=1:10)"
    data.model <- specifyModel(formula = stock_formula)
    Tdata.train <- as.data.frame(modelData(data.model,
                                           data.window=c('2007-01-03','2017-01-03')))
    print(head(Tdata.train))
    Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2017-01-04','2018-12-19')))) 
    print(head(Tdata.eval))
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
      colnames(IS) <- paste(IS[1,])
      IS <- IS[-c(1, 5, 12, 20, 25),]
      names_row <- paste(IS[, 1])
      IS <- IS[,-1]
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
      colnames(BS) <- BS[1,]
      BS <- BS[-c(1, 2, 17, 28),]
      names_row <- BS[, 1]
      BS <- BS[,-1]
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
      colnames(CF) <- CF[1,]
      CF <- CF[-c(1, 3, 11, 16),]
      names_row <- CF[, 1]
      CF <- CF[,-1]
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

get_returns <- function(stock.symbol,
                        from_to = paste(Sys.Date() - 3650,
                                        Sys.Date(), sep = "::"),
                        select     = "adjusted",
                        mutate_fun = "periodReturn",
                        type       = "log",
                        period     = "yearly") {
  stock.symbol %>%
    tq_get(get  = "stock.prices") %>%
    tq_transmute(
      select     = select,
      mutate_fun = periodReturn,
      type       = type,
      period     = period,
      subset     = from_to,
      col_rename = "returns",
      leading = TRUE
    ) %>%
    mutate(date_trans = switch (
      period,
      "yearly" = year(date),
      "quarterly" = quarter(date),
      "monthly" = month(date),
      "weekly" = week(date),
      "daily" = day(date)
    ))
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

get_stock_prices <- function(symbole) {
  stock <- symbole %>%
    tq_get(get  = "stock.prices")
  
  xts(stock[-1], order.by = stock$date)
}

# Indicator functions
mySMA <- function(x) SMA(Cl(x))[,'SMA'] # Moving Average 
myEMA <- function(x) EMA(Cl(x))[,'EMA'] # Exponential Moving Average 
myDEMA <- function(x) DEMA(Cl(x))[,'DEMA'] # Double Exponential Moving Average 
myWMA <- function(x) WMA(Cl(x))[,1] # Weigthed Moving Average
myEVWMA <- function(x) EVWMA(Cl(x), Vo(x))[,1] # Exponential Volume Weigthed Moving Average
myZLEMA <- function(x) ZLEMA(Cl(x))[,1] # Zero lag exponential Moving Averages
myMACD <- function(x) MACD(Cl(x))[,2] # Moving Average Convergence Divergence
myCCI <- function(x) CCI(HLC(x))[,'cci'] # Commodity Channel Index 
myCMF <- function(x) CMF(HLC(stock), volume = Vo(stock))[,1] # Chaikin Money Flow
myCMO <- function(x) CMO(Cl(stock))[,'cmo'] # Chaikin Money Oscillator
myDPO <- function(x) DPO(Cl(x))[,1] # Detrended Price Oscillator 
myROC <- function(x) ROC(Cl(x))[,1] # Calculate the (rate of) change of a series over n periods. 
myRSI <- function(x) RSI(Cl(x))[,"rsi"] # Relative Strength Indicator 
myTRIX <- function(x) TRIX(Cl(x))[,"TRIX"] # Relative Strength Indicator
myWPR <- function(x) WPR(HLC(x))[,1] # William's %R
myATR <- function(x) ATR(HLC(x))[,'atr'] # Average True Range, measures volatility of series  
mySMI <- function(x) SMI(HLC(x))[, "SMI"] #  Stochastic Momentum Index 
myADX <- function(x) ADX(HLC(x))[,'ADX'] # Welles Wilder's Directional Movement Index 
myAroon <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator # Identify starting trends
myBB <- function(x) BBands(HLC(x))[, "pctB"] # Bollinger Bands
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1] # Chaikin Volatility
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1] # Close Location Value 
myEMV <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2] # Arms' Ease of Movement Value 
myMFI <- function(x) MFI(HLC(x), Vo(x)) # Money Flow Index
mySAR <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1] # Parabolic Stop-and-Reverse
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1] # volatility
