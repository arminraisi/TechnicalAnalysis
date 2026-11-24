# BDA400 Assignment 6 — Technical Analysis using R (Visualization Phase)

**Student:** Armin Raisinasab  
**Project:** Interactive Stock Portfolio Dashboard using R Shiny

This dashboard retrieves historical stock prices from **Yahoo Finance**, via the `quantmod` package, and presents price charts, technical indicators, and trading-rule annotations.

## Setup and run

1. Open RStudio, and open `app.R`.
2. Run once in the RStudio Console:
   ```r
   install.packages(c("shiny", "ggplot2", "quantmod"))
   ```
3. Click **Run App** in RStudio, or execute `shiny::runApp(".")` from the repository folder.
4. Internet access is required when requesting Yahoo Finance stock data.

## Dashboard controls

- **Ticker**: default `AAPL`; accepts other Yahoo Finance ticker symbols.
- **Date range and time frame**: choose daily, weekly, or monthly data.
- **Chart type**: line, candlestick, or area.
- **Technical indicators**: show/hide short and long simple moving averages on the price chart, RSI (14) and MACD (12,26,9) in aligned panels below it.
- **MA windows**: default 20 (short) and 50 (long), customizable.
- **Buy/Sell annotations**: enable or disable markers at actual moving-average crossovers.

**Trading rule:** Buy when short SMA crosses above long SMA; Sell when it crosses below; otherwise Hold. Warm-up values are not treated as signals. Both the price chart and recent trading events table show the results.

## Files

`app.R` is the complete, commented Shiny implementation, organized into data collection, visualizations, indicator calculations, and trading rules. This README contains the run instructions.

## Data and practical notes

Data are retrieved live from Yahoo Finance; availability depends on its service and internet connectivity. Short or empty history cannot support some indicators. RSI and MACD appear in panels aligned by date because their numeric scales differ from share prices. This is a classroom demonstration rather than a validated trading strategy.

**Repository folder:** https://github.com/arminraisi/TechnicalAnalysis/tree/main/Assignment6
