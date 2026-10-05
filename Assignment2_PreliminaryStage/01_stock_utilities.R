# BDA400 Assignment 2 - Technical Analysis Utilities
# Student: Clinton Chemuta

required_packages <- c("quantmod", "TTR")
missing_packages <- required_packages[!(required_packages %in% rownames(installed.packages()))]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(quantmod)
library(TTR)

# Read symbols from portfolio.txt and download daily market data.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]

  stock_data <- list()
  for (symbol in symbols) {
    message("Loading ", symbol, "...")
    x <- getSymbols(symbol, src = "yahoo", from = from, to = to,
                    auto.assign = FALSE)
    stock_data[[symbol]] <- x
  }
  stock_data
}

# Statistical mode for a numeric vector.
stat_mode <- function(x) {
  x <- na.omit(as.numeric(x))
  if (length(x) == 0) return(NA_real_)
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Calculate statistics from adjusted closing prices.
calculate_statistics <- function(stock_xts, symbol = "Stock", ma_period = 20) {
  prices <- as.numeric(Ad(stock_xts))
  prices <- prices[!is.na(prices)]
  ma <- SMA(Ad(stock_xts), n = ma_period)

  data.frame(
    Symbol = symbol,
    Observations = length(prices),
    Mean_Adjusted_Close = mean(prices),
    Mode_Adjusted_Close = stat_mode(prices),
    Median_Adjusted_Close = median(prices),
    SD_Adjusted_Close = sd(prices),
    Latest_MA20 = as.numeric(last(na.omit(ma))),
    stringsAsFactors = FALSE
  )
}

calculate_portfolio_statistics <- function(stock_list, ma_period = 20) {
  do.call(rbind, lapply(names(stock_list), function(sym) {
    calculate_statistics(stock_list[[sym]], sym, ma_period)
  }))
}
