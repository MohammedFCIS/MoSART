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
prices_chart_types <- c("Line Chart" = "line",
                        "Bar Chart" = "bars",
                        "Match Sticks" = "matchsticks",
                        "Candlestick Chart" = "candlesticks")
prices_chart_themes <- c("White" = "white",
                        "Black" = "black")


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
                        value = TRUE)
          
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