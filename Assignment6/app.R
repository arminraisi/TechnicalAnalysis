# BDA400 Assignment 6 - Technical Analysis using R: Visualization Phase
# Interactive stock dashboard. Data source: Yahoo Finance via quantmod.
# Required packages: shiny, ggplot2, quantmod (TTR and xts are installed with quantmod).
# Install once using: install.packages(c("shiny", "ggplot2", "quantmod"))

required_packages <- c("shiny", "ggplot2", "quantmod")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(paste("Install missing packages:", paste(missing_packages, collapse = ", "),
             "using install.packages()."), call. = FALSE)
}
suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(quantmod)
})

# --- Step 1: Data collection and setup ---------------------------------------
# Download daily OHLCV data once per symbol and date range, then aggregate it.
# Displayed prices use Yahoo adjusted close only when the selected series is adjusted.
get_stock_data <- function(symbol, from, to, frame) {
  symbol <- toupper(trimws(symbol))
  if (!grepl("^[A-Z0-9.^=-]{1,20}$", symbol)) {
    stop("Enter a valid ticker symbol (e.g., AAPL, MSFT, or SPY).")
  }
  if (is.na(from) || is.na(to) || from >= to) {
    stop("Select a valid date range with the start before the end.")
  }
  # Yahoo's 'to' is effectively exclusive; add one day to include the end date.
  daily <- suppressWarnings(quantmod::getSymbols(
    symbol, src = "yahoo", from = from, to = to + 1,
    auto.assign = FALSE, warnings = FALSE
  ))
  if (NROW(daily) < 2L) stop("Not enough price observations were returned.")
  # Create a clean daily OHLCV frame first. Aggregate calendar weeks/months
  # explicitly to avoid differences in xts period conversion column naming.
  result <- data.frame(
    Date = as.Date(zoo::index(daily)),
    Open = as.numeric(quantmod::Op(daily)),
    High = as.numeric(quantmod::Hi(daily)),
    Low = as.numeric(quantmod::Lo(daily)),
    Close = as.numeric(quantmod::Cl(daily)),
    Volume = as.numeric(quantmod::Vo(daily))
  )
  if (frame != "Daily") {
    grouping <- if (frame == "Weekly") {
      as.Date(cut(result$Date, breaks = "week", start.on.monday = TRUE))
    } else {
      as.Date(format(result$Date, "%Y-%m-01"))
    }
    grouped <- split(seq_len(nrow(result)), grouping)
    result <- do.call(rbind, lapply(grouped, function(idx) {
      segment <- result[idx, , drop = FALSE]
      data.frame(Date = max(segment$Date),
                 Open = segment$Open[1L], High = max(segment$High, na.rm = TRUE),
                 Low = min(segment$Low, na.rm = TRUE),
                 Close = segment$Close[nrow(segment)],
                 Volume = sum(segment$Volume, na.rm = TRUE))
    }))
  }
  result <- result[stats::complete.cases(result[, c("Open", "High", "Low", "Close")]), ]
  result <- result[order(result$Date), , drop = FALSE]
  if (nrow(result) < 2L) stop("Not enough non-missing price observations remain.")
  rownames(result) <- NULL
  result
}

# A rolling SMA calculated on a numeric price vector; first period-1 values are NA.
rolling_sma <- function(x, n) {
  ans <- rep(NA_real_, length(x))
  if (n <= length(x)) {
    for (i in seq.int(n, length(x))) {
      window <- x[(i - n + 1L):i]
      if (all(is.finite(window))) ans[i] <- mean(window)
    }
  }
  ans
}

# RSI with Wilder smoothing (TTR implementation), padded to observation length.
calculate_rsi <- function(x, n = 14L) {
  if (length(x) <= n) return(rep(NA_real_, length(x)))
  as.numeric(TTR::RSI(x, n = n))
}

# MACD uses EMA differences and the EMA signal line. TTR pads warm-up periods.
calculate_macd <- function(x, fast = 12L, slow = 26L, signal = 9L) {
  if (length(x) <= slow + signal) {
    return(data.frame(MACD = rep(NA_real_, length(x)),
                      Signal = rep(NA_real_, length(x)),
                      Histogram = rep(NA_real_, length(x))))
  }
  raw_macd <- TTR::MACD(x, nFast = fast, nSlow = slow, nSig = signal,
                        maType = "EMA", percent = FALSE)
  line <- as.numeric(raw_macd[, 1])
  sig <- as.numeric(raw_macd[, 2])
  data.frame(MACD = line, Signal = sig, Histogram = line - sig)
}

# --- Step 4: Trading rules and annotations -----------------------------------
# An actual upward CROSSING generates Buy, a downward CROSSING generates Sell.
# Remaining observations are Hold, not repeated Buy/Sell recommendations.
trading_signals <- function(short_ma, long_ma) {
  n <- length(short_ma)
  result <- rep("Hold", n)
  if (n < 2L) return(result)
  for (i in 2:n) {
    values <- c(short_ma[i-1L], long_ma[i-1L], short_ma[i], long_ma[i])
    if (!all(is.finite(values))) next
    if (short_ma[i-1L] <= long_ma[i-1L] && short_ma[i] > long_ma[i]) {
      result[i] <- "Buy"
    } else if (short_ma[i-1L] >= long_ma[i-1L] && short_ma[i] < long_ma[i]) {
      result[i] <- "Sell"
    }
  }
  result
}

# --- Step 2: Shiny user interface and interactive controls ------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body { background: #f5f7fb; color: #20314a; }\n    .well { border-radius: 10px; background: white; border: 1px solid #e2e8f0; }\n    .main-title { margin-bottom: 3px; font-weight: 700; }\n    .hint { color: #64748b; margin-bottom: 18px; }\n    .metric { padding: 12px; background: white; border: 1px solid #e2e8f0;\n              border-radius: 9px; margin-bottom: 12px; }\n  "))),
  titlePanel(div(class = "main-title", "Stock Portfolio | Technical Analysis")),
  div(class = "hint", "BDA400 Assignment 6 — interactive stock prices, technical indicators, and trading signals"),
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput("symbol", "Stock symbol", value = "AAPL"),
      dateRangeInput("date_range", "Date range", start = Sys.Date() - 730,
                     end = Sys.Date(), max = Sys.Date()),
      selectInput("time_frame", "Time frame", c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Price chart", c("Line", "Candlestick", "Area")),
      checkboxGroupInput("technical_indicators", "Indicator overlays / panels",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")),
      numericInput("short_period", "Short moving average", value = 20, min = 2, max = 500),
      numericInput("long_period", "Long moving average", value = 50, min = 3, max = 1000),
      checkboxInput("show_signals", "Annotate Buy / Sell crossings", value = TRUE),
      actionButton("refresh", "Fetch / refresh prices", class = "btn-primary"),
      tags$hr(),
      helpText("Data: Yahoo Finance via quantmod. Dates correspond to trading periods. Signals are educational, not investment advice.")
    ),
    mainPanel(width = 9,
      uiOutput("status"),
      fluidRow(
        column(4, div(class = "metric", strong("Latest close"), textOutput("last_close", inline = TRUE))),
        column(4, div(class = "metric", strong("Latest signal"), textOutput("last_signal", inline = TRUE))),
        column(4, div(class = "metric", strong("Observations"), textOutput("n_rows", inline = TRUE)))
      ),
      plotOutput("stock_chart", height = "430px"),
      conditionalPanel("input.technical_indicators.indexOf('RSI') >= 0",
                       plotOutput("rsi_chart", height = "190px")),
      conditionalPanel("input.technical_indicators.indexOf('MACD') >= 0",
                       plotOutput("macd_chart", height = "215px")),
      h4("Latest trading signals"),
      tableOutput("signal_table")
    )
  )
)

# --- Step 2 / 3 / 4: Data transformations, indicators and charts ------------
server <- function(input, output, session) {
  # The action button initiates an explicit data fetch; inputs also trigger refresh.
  stock_data <- reactive({
    input$refresh
    withProgress(message = "Fetching Yahoo Finance prices", value = 0.2, {
      get_stock_data(input$symbol, input$date_range[1], input$date_range[2], input$time_frame)
    })
  })

  analysis_data <- reactive({
    df <- stock_data()
    short_n <- as.integer(input$short_period)
    long_n <- as.integer(input$long_period)
    validate(need(is.finite(short_n) && is.finite(long_n) && short_n >= 2L &&
                  long_n > short_n, "Set moving-average periods with 2 <= short < long."))
    df$ShortMA <- rolling_sma(df$Close, short_n)
    df$LongMA <- rolling_sma(df$Close, long_n)
    df$RSI <- calculate_rsi(df$Close)
    macd_data <- calculate_macd(df$Close)
    df$MACD <- macd_data$MACD
    df$MACDSignal <- macd_data$Signal
    df$Histogram <- macd_data$Histogram
    df$Signal <- trading_signals(df$ShortMA, df$LongMA)
    df
  })

  output$status <- renderUI({
    tryCatch({
      df <- stock_data()
      div(style = "color:#476582; margin:5px 0 12px;",
          paste("Source: Yahoo Finance |", nrow(df), input$time_frame,
                "observations |", format(min(df$Date)), "to", format(max(df$Date))))
    }, error = function(e) div(style = "color:#a94442;", paste("Data unavailable:", conditionMessage(e))))
  })
  output$last_close <- renderText({
    df <- analysis_data()
    paste0("$", format(round(tail(df$Close, 1), 2), nsmall = 2))
  })
  output$last_signal <- renderText({tail(analysis_data()$Signal, 1)})
  output$n_rows <- renderText({nrow(analysis_data())})

  output$stock_chart <- renderPlot({
    df <- analysis_data()
    p <- ggplot(df, aes(x = Date)) +
      labs(title = paste(input$symbol, "|", input$time_frame, "stock price"),
           x = NULL, y = "Price (USD)") +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
    if (input$chart_type == "Candlestick") {
      # Dates with zero price movement retain their high-low wick.
      p <- p + geom_segment(aes(xend = Date, y = Low, yend = High), color = "#334155") +
        geom_rect(aes(xmin = Date - 0.35, xmax = Date + 0.35,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                      fill = Close >= Open), color = "#334155", linewidth = 0.2) +
        scale_fill_manual(values = c("TRUE" = "#16a085", "FALSE" = "#e66b67"),
                          guide = "none")
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "#8eb8e5", alpha = 0.65) +
        geom_line(aes(y = Close), color = "#315f9f", linewidth = 0.5)
    } else {
      p <- p + geom_line(aes(y = Close), color = "#315f9f", linewidth = 0.8)
    }
    # Overlay moving averages on the same price axis when enabled.
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = ShortMA, color = "Short SMA"), linewidth = 0.75, na.rm = TRUE) +
        geom_line(aes(y = LongMA, color = "Long SMA"), linewidth = 0.75, na.rm = TRUE) +
        scale_color_manual(values = c("Short SMA" = "#f59e0b", "Long SMA" = "#9333ea"),
                           name = NULL)
    }
    # Only annotate cross events; Hold would clutter the entire price chart.
    if (isTRUE(input$show_signals)) {
      trades <- df[df$Signal %in% c("Buy", "Sell"), , drop = FALSE]
      if (nrow(trades) > 0L) {
        p <- p + geom_point(data = trades,
                            aes(y = Close, shape = Signal), size = 3.2,
                            color = "#111827") +
          geom_text(data = trades, aes(y = Close, label = Signal),
                    vjust = -1.1, size = 3.1, check_overlap = TRUE) +
          scale_shape_manual(values = c(Buy = 24, Sell = 25), name = "Trade")
      }
    }
    print(p)
  })

  # RSI and MACD are shown in aligned, separate panels: these indicators have
  # different scales from price, so plotting them on the price axis is misleading.
  output$rsi_chart <- renderPlot({
    df <- analysis_data()
    validate(need(any(is.finite(df$RSI)), "Select more data to calculate the 14-period RSI."))
    print(ggplot(df, aes(Date, RSI)) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date),
               ymin = 30, ymax = 70, fill = "#eaf4ff", alpha = 0.7) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "#64748b") +
      geom_line(color = "#1d8a80", linewidth = 0.7, na.rm = TRUE) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "Relative Strength Index (RSI 14)", y = "RSI", x = NULL) +
      theme_minimal(base_size = 11))
  })

  output$macd_chart <- renderPlot({
    df <- analysis_data()
    validate(need(any(is.finite(df$MACD)), "Select more data to calculate MACD (12, 26, 9)."))
    print(ggplot(df, aes(x = Date)) +
      geom_col(aes(y = Histogram, fill = Histogram >= 0), alpha = 0.6, na.rm = TRUE) +
      geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.65, na.rm = TRUE) +
      geom_line(aes(y = MACDSignal, color = "Signal"), linewidth = 0.65, na.rm = TRUE) +
      scale_fill_manual(values = c("TRUE" = "#16a085", "FALSE" = "#e66b67"), guide = "none") +
      scale_color_manual(values = c(MACD = "#2563eb", Signal = "#f59e0b"), name = NULL) +
      labs(title = "MACD (12, 26, 9)", y = "MACD", x = "Date") +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom"))
  })

  output$signal_table <- renderTable({
    df <- analysis_data()
    events <- df[df$Signal != "Hold", c("Date", "Close", "Signal"), drop = FALSE]
    if (nrow(events) == 0L) return(data.frame(Status = "No MA crossover events in selected period"))
    events <- tail(events, 10)
    events$Close <- round(events$Close, 2)
    events[rev(seq_len(nrow(events))), , drop = FALSE]
  }, rownames = FALSE)
}

shinyApp(ui = ui, server = server)
