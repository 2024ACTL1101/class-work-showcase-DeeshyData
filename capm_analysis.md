ACTL1101 Assignment Part B
================
Aadeesh Singhal
2024 T2

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the
Capital Asset Pricing Model (CAPM) using historical data for AMD and the
S&P 500 index. This exercise is designed to provide a hands-on approach
to understanding how these models are used in financial analysis to
assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between
systematic risk and expected return, especially for stocks. This model
is critical for determining the theoretically appropriate required rate
of return of an asset, assisting in decisions about adding assets to a
diversified portfolio.

## Objectives

1.  **Load and Prepare Data:** Import and prepare historical price data
    for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2.  **CAPM Implementation:** Focus will be placed on applying the CAPM
    to examine the relationship between AMD’s stock performance and the
    overall market as represented by the S&P 500.
3.  **Beta Estimation and Analysis:** Calculate the beta of AMD, which
    measures its volatility relative to the market, providing insights
    into its systematic risk.
4.  **Results Interpretation:** Analyse the outcomes of the CAPM
    application, discussing the implications of AMD’s beta in terms of
    investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data
  from Yahoo Finance without the need to manually download and read from
  a CSV file.
- `quantmod` stands for “Quantitative Financial Modelling Framework”. It
  was developed to aid the quantitative trader in the development,
  testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running
  `install.packages("quantmod")` in the R console before proceeding.

``` r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing

``` r
colSums(is.na(df))
```

    ## Date  AMD GSPC   RF 
    ##    0    0    0    9

``` r
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that
describes the relationship between systematic risk and expected return
for assets, particularly stocks. It is widely used to determine a
theoretically appropriate required rate of return of an asset, to make
decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula

The formula for CAPM is given by:

$$E(R_i) = R_f + \beta_i (E(R_m) - R_f)$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic
  risk of the security,
- $E(R_m)$ is the expected return of the market.

#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD
  and the S&P 500 from their adjusted closing prices. This should be
  done by dividing the difference in prices between two consecutive days
  by the price at the beginning of the period.

$$ \text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}} $$

``` r
df <- df %>%
  mutate(AMD_Return = (AMD - lag(AMD)) / lag(AMD),
         GSPC_Return = (GSPC - lag(GSPC)) / lag(GSPC)) %>%
  na.omit() # Omit first row which is NA
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by
  conversion of annual risk-free Rate. This conversion accounts for the
  compounding effect over the days of the year and is calculated using
  the formula:

$$\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1$$

``` r
df <- df %>%
  mutate(Daily_RF = (1 + RF / 100) ^ (1 / 360) - 1)
```

- **Calculate Excess Returns**: Compute the excess returns for AMD and
  the S&P 500 by subtracting the daily risk-free rate from their
  respective returns.

``` r
df <- df %>%
  mutate(AMD_Excess = AMD_Return - Daily_RF,
         GSPC_Excess = GSPC_Return - Daily_RF)
```

- **Perform Regression Analysis**: Using linear regression, we estimate
  the beta ($\beta$) of AMD relative to the S&P 500. Here, the dependent
  variable is the excess return of AMD, and the independent variable is
  the excess return of the S&P 500. Beta measures the sensitivity of the
  stock’s returns to fluctuations in the market.

``` r
model <- lm(AMD_Excess ~ GSPC_Excess, data = df)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = AMD_Excess ~ GSPC_Excess, data = df)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.095781 -0.014735 -0.001152  0.012276  0.173632 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.0011041  0.0007243   1.524    0.128    
    ## GSPC_Excess 1.5699987  0.0540654  29.039   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02567 on 1256 degrees of freedom
    ## Multiple R-squared:  0.4017, Adjusted R-squared:  0.4012 
    ## F-statistic: 843.3 on 1 and 1256 DF,  p-value: < 2.2e-16

#### Interpretation

What is your $\beta$? Is AMD more volatile or less volatile than the
market?

The calculated $\beta$ is 1.5699987. Since $\beta>1$, the AMD stock is
expected to increase by approximately 57%. This suggests that the AMD
stock is more sensitive to fluctuations in the market and indicates
higher risk. Therefore, AMD is more volatile than the market, leading to
higher risk but also potential higher returns.

#### Plotting the CAPM Line

Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM
regression line.

``` r
ggplot(df, aes(x = GSPC_Excess, y = AMD_Excess)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "AMD Excess Return vs. S&P 500 Excess Return",
       x = "S&P 500 Excess Return",
       y = "AMD Excess Return")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![capm](capm.png)<!-- -->

### Step 3: Prediction Interval

Suppose the current risk-free rate is 5.0%, and the annual expected
return for the S&P 500 is 13.3%. Determine a 90% prediction interval for
AMD’s annual expected return.

*Hint: Calculate the daily standard error of the forecast (*$s_f$), and
assume that the annual standard error for prediction is
$s_f \times \sqrt{252}$. Use the simple return average method to convert
annual stock returns to daily returns if needed.

``` r
beta_i <- summary(model)$coefficients[2, 1]

annual_Rf <- 0.05
annual_Rm <- 0.133

# Calculate the annual expected return on the capital asset using CAPM formula
Ri <- annual_Rf + beta_i * (annual_Rm - annual_Rf)

# Convert from annual to daily
daily_Rf <- (1 + annual_Rf / 100) ^ (1 / 360) - 1
daily_Rm <- annual_Rm / 252 # Using simple average method
Xf <- daily_Rm - daily_Rf

# Number of observations
n <- length(df$GSPC_Excess)

# Mean excess return of S&P 500
mean_GSPC <- mean(df$GSPC_Excess)

# Standard error of the estimate
se <- sqrt(sum(residuals(model)^2) / (n - 1 - 1))

# Sum of squares of AMD's annual return
SSX <- sum((df$GSPC_Excess - mean_GSPC)^2)

# Calculate the daily standard error of the forecast
sf <- se * sqrt(1 + 1/n + (Xf - mean_GSPC)^2 / SSX)

# Annual standard error for prediction
sf_annual <- sf * sqrt(252)

# 90% confidence interval
alpha <- 0.1

# t value for 90% confidence interval
t_value <- qt(1 - alpha/2, df = n - 2)

# Prediction interval
lower_bound <- Ri - t_value * sf_annual
upper_bound <- Ri + t_value * sf_annual
```

The estimated annual expected return for AMD is 18.03% with a 90%
prediction interval of \[-49.07%, 85.13%\].

## Appendix

### Data Frame

The following table provides data for each column and the first six rows
of the data frame.

``` r
head(df)
```

    ##         Date   AMD    GSPC   RF   AMD_Return  GSPC_Return     Daily_RF
    ## 2 2019-05-21 27.35 2864.36 2.33  0.025112446  0.008495836 6.398177e-05
    ## 3 2019-05-22 27.41 2856.27 2.32  0.002193765 -0.002824396 6.371028e-05
    ## 4 2019-05-23 26.36 2822.24 2.34 -0.038307160 -0.011914150 6.425322e-05
    ## 5 2019-05-24 26.44 2826.06 2.33  0.003034898  0.001353559 6.398177e-05
    ## 6 2019-05-28 29.05 2802.39 2.31  0.098714019 -0.008375677 6.343877e-05
    ## 7 2019-05-29 28.09 2783.02 2.31 -0.033046441 -0.006911912 6.343877e-05
    ##     AMD_Excess  GSPC_Excess
    ## 2  0.025048465  0.008431854
    ## 3  0.002130054 -0.002888107
    ## 4 -0.038371413 -0.011978403
    ## 5  0.002970917  0.001289577
    ## 6  0.098650580 -0.008439116
    ## 7 -0.033109880 -0.006975351

**Note:** Row 1 is not included since this row contains NA values.
