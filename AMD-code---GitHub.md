ACTL1101 Assignment Part A
================
Aadeesh Singhal
2024 T2

# Algorithmic Trading Strategy

## Step 1: Data Loading

This step loads the data in R Studio and plots the closing prices over
time to visualise the price movement

``` r
amd_df <- read.csv("AMD.csv")
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]

plot(amd_df$date, amd_df$close,'l')
```

![](AMD-code---GitHub_files/figure-gfm/load-data-1.png)<!-- -->

## Step 2: Trading Algorithm

``` r
# Initialise columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialise if needed for tracking

# Initialise variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Conditions for trading algorithm
for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i] #redefining variable as current price for ease
  
  # buy if previous price = 0
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } 
  # sell on last day
  else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
    accumulated_shares <- 0
  } 
  # buy if current price is less than previous price
  else if (current_price < previous_price) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } 
  
  # Initialising columns to adjust for calculations
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price

}
```

This code adds the new variables: trade_type, costs_proceeds, and
accumulated_shares to the dataframe and then applies the trading
algorithm using the conditions in the for loop to decide when to buy and
sell shares.

## Step 3: Customize Trading Period

``` r
start_date <- as.Date('2022-07-01')
end_date <- as.Date('2023-06-30')

amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
```

## Step 4: Run Your Algorithm and Analyze Results

``` r
# Initialise columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialise if needed for tracking

# Initialising variables
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Re-running the conditions of the trading algorithm for the newly defined period
for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  # buy if previous_price = 0
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } 
  # sell on last day
  else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
    accumulated_shares <- 0
  } 
  # buy if current price is less than previous price
  else if (current_price < previous_price) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } 
  
  # Initialising columns to adjust for calculations
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price

}

# Calculating total profit/loss, total capital invested, and ROI
total_profit_loss_1 <- sum(amd_df$costs_proceeds, na.rm = TRUE)
total_capital_invested_1 <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
ROI_1 <- (total_profit_loss_1 / total_capital_invested_1) * 100
```

The trading algorithm developed in Step 2 is applied to the new trading
period defined in Step 3. The Total Profit and Loss (PnL), Total Capital
Invested (TCI), and ROI are calculated to determine the profitability of
the algorithm.

The PnL for the trading period is \$386232.1, and the TCI is \$1026252.

Therefore, the ROI is 37.64%.

## Step 5: Profit-Taking Strategy

``` r
# Initialise columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialise if needed for tracking

# Initialising variables
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Initialising variables for strategy 2
amd_df$total_share_price <- 0 # accumulates the cost of the total shares bought
amd_df$average_purchase_price <- 0

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  # buy on first day
  if (previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    total_share_price <- share_size * current_price
    average_purchase_price <- total_share_price / accumulated_shares
  } 
  # sell on last day
  else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
    accumulated_shares <- 0
  } 
  # sell if current price has increased by 40% from the average purchase price
  else if (current_price >= average_purchase_price * 1.4) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares / 2
    accumulated_shares <- accumulated_shares / 2 # sell half of holdings
    total_share_price <- total_share_price / 2
  } 
  # buy if current price is less than previous price
  else if (current_price < previous_price) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    total_share_price <- total_share_price + share_size * current_price
    average_purchase_price <- total_share_price / accumulated_shares
  }
  
  # Initialising columns to adjust for calculations
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
  amd_df$total_share_price[i] <- total_share_price
  amd_df$average_purchase_price[i] <- average_purchase_price
  
}
```

A new strategy is implemented to maximise profit by selling half of the
holdings if the current price has increased by 40% from the average
purchase price.

## Step 6: Summarize Your Findings

``` r
# Calculating total profit/loss, total capital invested, and ROI for strategy 2
total_profit_loss_2 <- sum(amd_df$costs_proceeds, na.rm = TRUE)
total_capital_invested_2 <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
ROI_2 <- (total_profit_loss_2 / total_capital_invested_2) * 100

plot(amd_df$date, amd_df$close, 'l')
```

![](AMD-code---GitHub_files/figure-gfm/summary-1.png)<!-- --> The graph
of the defined period depicts AMD shares from 01/07/2022 to 30/06/2023.
It is evident that the AMD shares had a large decrease from August 2022
to October 2022 but gradually increased over the following period.

In May 2023, AMD launched the Radeon RX 7600 which was aimed at
high-performance gaming and also released the Ryzen Z1 mobile processor.
This suggests that investors believed in the market expectations of this
product evident by the increase in stocks during this period from
\$81.62 on 03/05/2023 to \$118.21 on 31/05/2023.

Strategy 1 only bought shares within the period and only sold at the
very end. This is compared to strategy 2 which also sold shares if the
current price was more than 40% above the average purchase price on the
given day. When implementing the second strategy, the outcome sold in
May 2023 when the stock market was at its peak. To determine the better
strategy, we can compare their respectively calculated PnL and ROI.

We can calculate the change in PnL using the following formula:

$$\Delta{\text{PnL}}={\text{PnL}}_{\text{strategy 2}}-{\text{PnL}}_{\text{strategy 1}}$$

Given that the PnL calculated for strategy 1 was \$386232.1 and for
strategy 2 was \$485894.7 The change in PnL is \$99662.61. Since this
value is positive, we can conclude that the second strategy earned more
and was therefore more beneficial. It follows that the second strategy
should produce a higher ROI which is evident when comparing their
respective ROI:

$$\Delta{\text{ROI}}={\text{ROI}}_{\text{strategy 2}}-{\text{ROI}}_{\text{strategy 1}}$$

The ROI for strategy 1 was 37.64%, while it was 53.56% for strategy 2.
This produces a difference of 15.92% which is also positive indicating
that the second strategy provided a higher ROI. This is also supported
by the increase in PnL.
