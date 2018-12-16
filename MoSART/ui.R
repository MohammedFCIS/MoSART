#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


# Determine what infromation to be retrieved and associated apis/input and output
# Determine the best layour
# implement them
## Refs:
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ01-core-functions-in-tidyquant.html
#https://github.com/MohammedFCIS/shinyapps-tutorials/blob/master/Movie-Rating/app.R
#http://shiny.rstudio.com/gallery/dynamic-ui.html
#https://gallery.shinyapps.io/076-widget-select/

# Define UI for application that draws a histogram
stock_choices <-
  c("Stock Indexes" = "index", "Stock Exchanges" = "exchange")
prices_choices <-
  c("Yahoo" = "yahoo",
    "Qundl" = "qundl",
    "Alpha Vantage" = "alphavantage")
prices_chart_types <- c(
  "Bar Chart" = "bars",
  "Line Chart" = "line",
  "Match Sticks" = "matchsticks",
  "Candlestick Chart" = "candlesticks"
)
prices_chart_themes <- c("White" = "white",
                         "Black" = "black")
indicators <- c(
  "Volume" = "addVo",
  "Moving Average" = "addSMA",
  "Welles Wilder's Directional Movement Indicator" = "addADX",
  "Average True Range" = "addATR",
  "Bollinger Bands" = "addBBands",
  "Bollinger Band Width" = "addBBands2",
  "Bollinger %b" = "addBBands3",
  "Commodity Channel Index" = "addCCI",
  "Chaiken Money Flow" = "addCMF",
  "Chande Momentum Oscillator" = "addCMO",
  "Double Exponential Moving Average" = "addDEMA",
  "Detrended Price Oscillator" = "addDPO",
  "Exponential Moving Average" = "addEMA",
  "Price Envelope" = "addEnvelope",
  "Exponential Volume Weigthed Moving Average" = "addEVWMA",
  "Options and Futures Expiration" = "addExpiry",
  "Moving Average Convergence Divergence" = "addMACD",
  "Momentum" = "addMomentum",
  "Rate of Change" = "addROC",
  "Relative Strength Indicator" = "addRSI",
  "Parabolic Stop and Reverse" = "addSAR",
  "Stocastic Momentum Index" = "addSMI",
  "Triple Smoothed Exponential Oscillator" = "addTRIX",
  "Weighted Moving Average" = "addWMA",
  "Williams %R" = "addWPR",
  "ZLEMA" = "addZLEMA"
)
return_features <- c(
  "Open" = "open",
  "Close" = "close",
  "High" = "high",
  "Low" = "low",
  "Adjusted" = "adjusted",
  "Volume" = "volume"
)
return_options <- c("Log" = "log",
                    "Arithmetic" = "arithmetic")
return_period_options <- c(
  "Yearly" = "yearly",
  "Quarterly" = "quarterly",
  "Monthly" = "monthly",
  "Weekly" = "weekly",
  "Daily" = "daily"
)

shinyUI(
  navbarPage(
    title = "MoSART",
    id = "mosart",
    tabPanel(
      title = "Stock Selection",
      value = "stockSelector",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "stockType",
            label = h3("Select Stock Type"),
            choices = stock_choices
          ),
          uiOutput("stock")
          
        ),
        # returned stocks
        mainPanel(dataTableOutput("stocks"),
                  uiOutput("stock_actions"))
      )
    ),
    tabPanel(
      title = "Stock Prices Analysis",
      value = "stockPrices",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Indicators & Signals",
          value = "indi_sig",
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "pricesSource",
                label = h3("Select Price Source"),
                choices = prices_choices
              ),
              hr(),
              h3("Chart Setup"),
              radioButtons(
                "pricesChartType",
                label = h5("Chart Type"),
                choices = prices_chart_types
              ),
              br(),
              radioButtons(
                "pricesChartTheme",
                label = h5("Chart Theme"),
                choices = prices_chart_themes
              ),
              checkboxInput("priceChartGrid",
                            "Show Grid",
                            value = TRUE),
              checkboxInput("priceLogScale",
                            "Log Scale",
                            value = FALSE),
              selectInput("indicators", "Indicators",
                          indicators , multiple = TRUE),
              dateRangeInput(
                inputId = "daterange",
                label = "Date range",
                start = Sys.Date() - 365,
                end = Sys.Date()
              )
            ),
            # returned stocks
            mainPanel(
              dataTableOutput("stock_prices"),
              br(),
              plotOutput("prices_plot")
            )
          )
        ),
        tabPanel(
          title = "Periodic Returns",
          value = "perdioic_returns",
          sidebarLayout(
            sidebarPanel(
              dateRangeInput(
                inputId = "daterange_return",
                label = "Date range",
                start = Sys.Date() - 3650,
                end = Sys.Date()
              ),
              selectInput("return_features", "Apply return to",
                          return_features),
              selectInput("return_function", "Transformation Function",
                          return_options),
              selectInput(
                "return_period",
                "Transformation Priod",
                return_period_options,
                selected = "yearly"
              )
            ),
            mainPanel(plotOutput("return_plot"))
          )
        ),
        tabPanel(
          title = "Simulation",
          value = "simulation",
          sidebarLayout(
            sidebarPanel(
              numericInput("days_num", "Number of Stock Price Simulations", value = 252),
              numericInput("sim_num", "Number of Monte Carlo Simulations", value = 250)
            ),
            mainPanel(
              plotOutput("daily_return_plot"),
              plotOutput("simulation_plot")
            )
          )
        ),
        tabPanel(
          title = "Strategy Builder",
          value = "strategy_builder",
          fluidRow(
            h3("Initialization"),
            column(
              width = 3,
              textInput("strategy_name",
                        label = "Strategy Name", placeholder = "Strategy Name")
            ),
            column(
              width = 3,
              numericInput("trade_size", label = "Teade Size", value = 1000)
            ),
            column(
              width = 3,
              numericInput("initial_equity", label = "Initial Equity", value = 1000)
            ),
            column(
              width = 3,
              dateRangeInput("trade_preiod", label = "Trade Period", 
                             start = Sys.Date() - 365, end = Sys.Date())
            )
          ),
          fluidRow(
            h3("Indicators Builder"),
            column(width = 3,
                   wellPanel(selectInput("strat_indicators", "Indicators",
                               c('Select Indicator'='',indicators[-1])),
                             actionButton("add_indic", "Add"))),
            column(width = 9,
                   tags$div(id ="indicators_placeholder"))),
          fluidRow(column(width = 12,
                          h3("Signal Builder"))),
          fluidRow(column(width = 12,
                          h3("Signal Builder")))
        )
      )
    ),
    tabPanel(
      title = "Stock key Ratios",
      value = "keyRatios",
      navlistPanel(
        "key Ratios Analysis",
        tabPanel(
          "Financials",
          dataTableOutput("stock_ratios_financials"),
          plotOutput("ratios_financials_plot")
        ),
        tabPanel(
          "Profitability",
          dataTableOutput("stock_ratios_profitability"),
          plotOutput("ratios_profitability_plot")
        ),
        tabPanel(
          "Cash Flow",
          dataTableOutput("stock_ratios_cash_flow"),
          plotOutput("ratios_cash_flow_plot")
        ),
        tabPanel(
          "Financial Health",
          dataTableOutput("stock_ratios_financial_health"),
          plotOutput("ratios_financial_health_plot")
        ),
        tabPanel(
          "Efficiency Ratios",
          dataTableOutput("stock_ratios_efficiency"),
          plotOutput("ratios_efficiency_plot")
        ),
        tabPanel(
          "Growth",
          dataTableOutput("stock_ratios_growth"),
          plotOutput("ratios_growth_plot")
        ),
        tabPanel(
          "Valuation Ratios",
          dataTableOutput("stock_ratios_value_ratios"),
          plotOutput("ratios_value_ratios_plot")
        )
      )
    ),
    tabPanel(
      title = "Stock Key Stats",
      value = "keyStats",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Yahoo",
          br(),
          dataTableOutput("stock_key_stats_earning_estimates"),
          hr(),
          dataTableOutput("stock_key_stats_revenue_estimates"),
          hr(),
          dataTableOutput("stock_key_stats_earning_history"),
          hr(),
          dataTableOutput("stock_key_stats_eps_trend"),
          hr(),
          dataTableOutput("stock_key_stats_eps_revisions"),
          hr(),
          dataTableOutput("stock_key_stats_growth_est")
        ),
        tabPanel("FinViz", dataTableOutput("stock_key_stats_finviz"))
      )
    ),
    tabPanel(
      title = "Stock Finance Statement",
      value = "financeStatement",
      br(),
      dataTableOutput("stock_finance_statement_is"),
      hr(),
      dataTableOutput("stock_finance_statement_bs"),
      hr(),
      dataTableOutput("stock_finance_statement_cf")
    )
  )
)