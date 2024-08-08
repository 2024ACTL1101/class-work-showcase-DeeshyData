
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
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
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
df <- df %>%
  mutate(AMD_Return = (AMD - lag(AMD)) / lag(AMD),
         GSPC_Return = (GSPC - lag(GSPC)) / lag(GSPC)) %>%
  na.omit() # Omit first row which is NA
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
df <- df %>%
  mutate(Daily_RF = (1 + RF / 100) ^ (1 / 360) - 1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
df <- df %>%
  mutate(AMD_Excess = AMD_Return - Daily_RF,
         GSPC_Excess = GSPC_Return - Daily_RF)
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
model <- lm(AMD_Excess ~ GSPC_Excess, data = df)
summary(model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The calculated $\beta$ is `r summary(model)$coefficients[2, 1]`. Since $\beta>1$, the AMD stock is expected to increase by approximately `r sprintf("%.0f%%", (summary(model)$coefficients[2, 1] - 1) * 100)`. This suggests that the AMD stock is more sensitive to fluctuations in the market and indicates higher risk. Therefore, AMD is more volatile than the market, leading to higher risk but also potential higher returns.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
ggplot(df, aes(x = GSPC_Excess, y = AMD_Excess)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "AMD Excess Return vs. S&P 500 Excess Return",
       x = "S&P 500 Excess Return",
       y = "AMD Excess Return")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
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
The estimated annual expected return for AMD is `r sprintf("%.2f%%", Ri * 100)` with a 90% prediction interval of [`r sprintf("%.2f%%", lower_bound * 100)`, `r sprintf("%.2f%%", upper_bound * 100)`].

## Appendix

### Data Frame

The following table provides data for each column and the first six rows of the data frame.

```r
head(df)
```

**Note:** Row 1 is not included since this row contains NA values.
