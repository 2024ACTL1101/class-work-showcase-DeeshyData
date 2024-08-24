## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by
incorporating financial metrics to evaluate its profitability. This
exercise simulates a real-world scenario where you, as part of a
financial technology team, need to present an improved version of a
trading algorithm that not only executes trades but also calculates and
reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have
been tasked by the Trading Strategies Team to modify your trading
algorithm. This modification should include tracking the costs and
proceeds of trades to facilitate a deeper evaluation of the algorithm’s
profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to
include costs, proceeds, and return on investments metrics to assess the
profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a
    DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm
    based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total
    profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and
    analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the “Data Loading” section
to generate a DataFrame containing AMD stock closing data. This will
serve as the basis for your trading decisions. First, create a data
frame named `amd_df` with the given closing prices and corresponding
dates.

``` r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

\##Plotting the Data Plot the closing prices over time to visualize the
price movement.

``` r
plot(amd_df$date, amd_df$close,'l')
```

![](AMD_files/figure-gfm/plot-1.png)<!-- -->

## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should
initialize necessary variables, and loop through the dataframe to
execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns
  ‘trade_type’, ‘costs_proceeds’ and ‘accumulated_shares’.
- Change the algorithm by modifying the loop to include the cost and
  proceeds metrics for buys of 100 shares. Make sure that the algorithm
  checks the following conditions and executes the strategy for each
  one:
  - If the previous price = 0, set ‘trade_type’ to ‘buy’, and set the
    ‘costs_proceeds’ column to the current share price multiplied by a
    `share_size` value of 100. Make sure to take the negative value of
    the expression so that the cost reflects money leaving an account.
    Finally, make sure to add the bought shares to an
    `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the
    previous day, set the ‘trade_type’ to ‘buy’. Set the
    ‘costs_proceeds’ to the current share price multiplied by a
    `share_size` value of 100.
  - You will not modify the algorithm for instances where the current
    day’s price is greater than the previous day’s price or when it is
    equal to the previous day’s price.
  - If this is the last day of trading, set the ‘trade_type’ to ‘sell’.
    In this case, also set the ‘costs_proceeds’ column to the total
    number in the `accumulated_shares` variable multiplied by the price
    of the last day.

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

- Define a trading period you wanted in the past five years

``` r
start_date <- as.Date('2022-07-01')
end_date <- as.Date('2023-06-30')

amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
```

## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as
expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from
  your trades. This should be the sum of all entries in the
  ‘costs_proceeds’ column of your dataframe. This column records the
  financial impact of each trade, reflecting money spent on buys as
  negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal
  to the sum of the ‘costs_proceeds’ values for all ‘buy’ transactions.
  Since these entries are negative (representing money spent), you
  should take the negative sum of these values to reflect the total
  amount invested.
- ROI Formula:
  $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

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

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

- Option 1: Implement a profit-taking strategy that you sell half of
  your holdings if the price has increased by a certain percentage
  (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that
  you sell half of your holdings if the stock falls by a certain
  percentage (e.g., 20%) from the average purchase price. You don’t need
  to buy 100 stocks on the days that the stop-loss mechanism is
  triggered.

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

- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these
  outcomes may have occurred.

``` r
# Calculating total profit/loss, total capital invested, and ROI for strategy 2
total_profit_loss_2 <- sum(amd_df$costs_proceeds, na.rm = TRUE)
total_capital_invested_2 <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
ROI_2 <- (total_profit_loss_2 / total_capital_invested_2) * 100

plot(amd_df$date, amd_df$close, 'l')
```

![](AMD_files/figure-gfm/summary-1.png)<!-- -->

The graph of the defined period depicts AMD shares from 01/07/2022 to
30/06/2023. It is evident that the AMD shares had a large decrease from
August 2022 to October 2022 but gradually increased over the following
period.

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
