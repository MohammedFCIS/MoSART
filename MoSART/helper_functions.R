# Helper function to load local stored
# index stock data frames
load_stock_df <- function(stock) {
  tryCatch({
    #  retrive global field value
    return(as.tibble(get(stock)))
  },
  error = function(err) {
    message(paste(stock, "is not loaded yet"))
    message(err)
  },
  finally = {
    tryCatch({
      assign(stock,
             read_csv(paste0("data/", stock, ".csv")),
             envir = .GlobalEnv)
    },
    error = function(err) {
      message(err)
    })
    return(as.tibble(get(stock, envir = .GlobalEnv)))
  })
}

# This function will be called
# when user wants to referesh stocks list
update_stocks_list <- function(stock_indexes, stock_exchanges) {
  tryCatch({
    walk(stock_indexes, ~ write_stock_df(.x))
    walk(stock_exchanges, ~ write_stock_df(.x, FALSE))
  },
  error = function(err) {
    message("Could not update stocks")
    message(err)
  },
  warning = function(warn) {
    message(warn)
  })
}

# Helper function to retrieve online
# index stocks and write stock data
# fram into csv file
write_stock_df <- function(stock, index = TRUE) {
  if (index) {
    stock_df <- tq_index(stock)
  } else {
    stock_df <- tq_exchange(stock)
  }
  
  if (nrow(stock_df) > 0) {
    write_csv(stock_df, paste0("MoSART/data/", stock, ".csv"))
  }
}
