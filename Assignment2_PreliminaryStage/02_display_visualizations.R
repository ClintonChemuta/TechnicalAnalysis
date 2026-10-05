# BDA400 Assignment 2 - Data Display and Visualizations
# Student: Clinton Chemuta
source("01_stock_utilities.R")

stocks <- load_stock_data("portfolio.txt")
statistics <- calculate_portfolio_statistics(stocks, ma_period = 20)

# Display calculated statistics.
print(round(statistics, 2))

# Display the first and last observations for every stock.
for (symbol in names(stocks)) {
  cat("\n=====", symbol, "- FIRST 6 ROWS =====\n")
  print(head(stocks[[symbol]]))
  cat("\n=====", symbol, "- LAST 6 ROWS =====\n")
  print(tail(stocks[[symbol]]))
}

# Interactive/standard quantmod candlestick charts.
for (symbol in names(stocks)) {
  chartSeries(
    stocks[[symbol]],
    name = paste(symbol, "Daily Price"),
    theme = chartTheme("white"),
    TA = "addSMA(n=20,col='blue');addVo()"
  )
}

# Base R adjusted-close line charts with 20-day SMA.
for (symbol in names(stocks)) {
  x <- stocks[[symbol]]
  price <- Ad(x)
  ma20 <- SMA(price, n = 20)
  plot(index(price), as.numeric(price), type = "l",
       main = paste(symbol, "Adjusted Close and 20-Day SMA"),
       xlab = "Date", ylab = "Adjusted Close")
  lines(index(ma20), as.numeric(ma20), lty = 2)
  legend("topleft", legend = c("Adjusted Close", "20-Day SMA"),
         lty = c(1, 2), bty = "n")
}
