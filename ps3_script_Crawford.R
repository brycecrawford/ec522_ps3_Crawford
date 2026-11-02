
pacman::p_load(tidyverse,fpp3)

# Loading in Retail and Sales data from Fred
# retail_raw <- read_csv("data/retail_sales_raw.csv")
# 
# head(retail_raw)

# Verifying columns are classified correctly
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# Converting data into long format
retail_raw_long <- retail_raw |> 
  select(observation_date, RSAFS, RSAFSNA) |> 
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# Building tsibble 
retail_ts <- retail_raw_long |> 
  mutate( Month = yearmonth(observation_date)) |> 
  as_tsibble( index = Month, key = series_id)

head(retail_ts)

# Confirming no gaps in data
has_gaps(retail_ts)

key_vars(canadian_gas)
index_var(canadian_gas)

# Uploading data set of monthly production of Canadian gas
can_gas <- canadian_gas |> 
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# Filtering Canadian gas data frame to include data from Jan 1990 onward
can_gas_recent <- can_gas |> 
  filter(Month >= yearmonth( "1990 Jan"))

head(can_gas_recent)

# Organizing data to reflect month, quarter, and year
can_gas_recent <- can_gas_recent |> 
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quater = quarter(Month))

# Table of average volume per month
can_gas_recent |> 
  as_tibble() |> 
  group_by(month) |> 
  summarize(avg_volume = mean(Volume))
# December(17.1 BCM) has the highest average sales  while June(14.9 BCM) has the lowest. 

#
can_gas_recent |> 
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |> 
  head()

# Changing Canadian gas production to quarterly from monthly
can_gas_q <- can_gas |> 
  filter( Month <= yearmonth("2004 Dec")) |> 
  index_by(Quarter = yearquarter(Month)) |> 
  summarize(Volume = sum(Volume))

head(can_gas_q)

#Plotting quarterly Canadian gas production
can_gas_q |> autoplot(Volume)
# The quarterly series appear to be seasonal. Volume tends to decrease from December to June and increase from June to December. The reason we sum the volume instead of taking the mean is because volume is the level of quantity that is produced and not a variable to show the level at a period of time.

#plotting sales
retail_ts |>  autoplot(sales)

#
retail_ts |> 
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. no seaonally adjusted",
    x = NULL, 
    y = "Millions of Dollars", 
    color = NULL) +
  theme_minimal()
#In the non-seasonally adjusted data, there is a quarterly season incline and drop every year. This is consistent throughout the entire series until covid where there are noticeably bigger drops. 
# The two data sets follow the same trend throughout the series, while the non-seasonally adjusted data has higher peaks and lower troughs. 

#Plotting Canadian gas all observations
can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = NULL,
    y = "Billions of Cubic Meters (CMB)") +
  theme_minimal(
  )

# Plot of retail sales with a linear trend line
retail_ts |> 
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL,
    y = "Retail Sales in Billions ($)") +
  scale_y_continuous(labels = scales::label_dollar(scale = 1/1000) )
# I chose to scale the y axis to it easier to interpret the scale of retail sales. Before the transformation it would $400,000 million dollars which is not intuitive to read. I changed it to reflect an easier interpretation of $400 Billion. 

# Yes the season swings get larger as the series grow. There is a shock during 2021 but returns to  the trending seasonality. 

# Adding log volume to data frame
can_gas <-  can_gas |> 
  mutate( log_volume = log(Volume))

# Plotting log volume
can_gas |> autoplot(log_volume)
# Instead of treading upward, the log transformation has a almost downward trending slope. As the series continues, there seems to be a top out of around a 3% change. 
# I do not think this log transformation is a good candidate for a model to capture a linear trend. The linear model would not do a good job predicting the change post 2000 since it seems to be leveling off. 

# Adding a log sales variable to RSAFSNA dataframe
retail_log_ts <- retail_ts |> 
  mutate( log_sales = log(sales)) |> 
  filter( series_id == "RSAFSNA")

# Plotting log sales from RSAFSNA
retail_log_ts |> autoplot(log_sales) +
  labs( title = " Log Sales of U.S. Retail and Services")

#Plotting sales levels from RSAFSNA 
retail_log_ts |> autoplot(sales) + 
  labs(title = "Sales of U.S. Retail and Services")
# After making a log transformation, the data set is a better candidate for applying the log and using a model to capture the linear trend than trying to fit eh model to original level of sales. 
# Looking at the plot, the log sales look to be more linear than the levels.

# Computing first differences to Canadian gas production data frame
can_gas <- can_gas |> 
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log(Volume)))

# Plotting first differences of volume
can_gas |> autoplot(growth_direct)

# Plotting first difference of growth of volume
can_gas <- can_gas |> 
  mutate( growth_gap = abs(growth_direct - growth_logdiff))

#Plotting differences between growth direct and growth log diff
can_gas |> autoplot(growth_gap)
# Plotting growth_gap makes the seasonality but does show a general decrease over time of the growth between variables meaning that growth find a a near stationary point in the long run.
## Type here ##

# Computing month over month change to retail sales
retail_ts <- retail_ts |> 
  group_by(series_id) |> 
  mutate( growth_mom = 100* difference(log(sales))) |> 
  ungroup()

# Plotting month over month retail sales
retail_ts |> 
  autoplot(growth_mom) +
  labs(
    title = " Month-Over-Month Growth: SA vs NSA Retail Sales",
    x = NULL, 
    y = "Growth Rate (%)",
    color = NULL) +
  theme_minimal()
# The non-seasonal sales data is much noisier than the seasonally adjusted data. This is consistent with the 2.7b. This is noisier than the seasonally adjusted because the month over month growth is accounting for the seasonal growth and not the unweighted levels. 

# Plotting seasonality of sales
retail_ts |> 
  filter(series_id == "RSAFSNA") |> 
  gg_season(sales) + 
  labs(
    title = "Season Pattern in NSA Retail Sales",
    y = "Millions of Dollars") +
  theme_minimal()

# Showing levels of sales seaonally adjusted
retail_ts |> 
  filter(series_id == "RSAFS") |> 
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of Dollars") +
  theme_minimal()
  
#
can_gas |> 
  gg_season(log_volume) +
  labs(
    title = " Seasonal Pattern in Gas Production in Canada",
    y = "Millions of Cubic Meters") + 
  theme_minimal()
# There is a clear seasonal patter in the gas production. From around may, gas productions decline until it hits its trough, then increases until December where it hits its peak. 
# This is consistent with how gas is used. People use more gas to heat their home in the colder months so gas production naturally increases in the fall and decreases in the spring. 

# Creating sub series plot for log of volume
can_gas |> 
  gg_subseries(log_volume) +
  labs( 
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)") +
  theme_minimal()

# seasonally adjusting Canadian gas production
retail_nsa_13 <- retail_ts |> 
  filter(series_id == "RSAFSNA") |> 
  model(x13 = X_13ARIMA_SEATS(sales)) |> 
  components()

# Graphing seasonally adjusted data
retail_nsa_13 |> autoplot()
# The season adjust column highest is around 1.2 which means that his month is around 20% above the trend. The highest months is December, and started with a value of 1.1 and grew to the 1.2. The gas production in December seems to grow on average every year. 

# Comparing the seasonally adjusted data
sa_compare <- retail_nsa_13 |> 
  as_tibble() |> 
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |> 
      filter(series_id == "RSAFS") |> 
      as_tibble() |> 
      transmute(Month, census = sales),
    by = "Month"
  ) |> 
  mutate(pct_dif = 100 * (log(mine) - log(census)))

# Plotting the difference between my seasonally adjusted data and the Census Bureau's
sa_compare |> 
  ggplot(aes(x = Month, y = pct_dif)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL,
    y = "Difference (%)") +
  theme_minimal()
# Using the default options for seasonally adjusting the data does a fairly good job in the first quarter of the series. After that the difference starts to gets bigger as the series continues, especially around covid. Looks like the default inst good for adjusting for global pandemics. 

# Time series linear model for Canadian gas production with seasonal dummies
can_gas_fit <- can_gas |> 
  model(TSLM(log_volume ~ trend() + season()))

# Table of model coefficients
report(can_gas_fit)

# Better table of model coefficients
can_gas_fit |> 
  tidy() |> 
  select(term, estimate, std.error, statistic, p.value) |> 
  knitr::kable(digits = 3)

# Comparing fitted and actual values of Canadian gas production
can_gas_fit |> 
  augment() |> 
  ggplot(aes(x = Month)) +
  geom_line(aes( y = log_volume, color = "Actual")) +
  geom_line(aes( y = .fitted, color = "Fitted")) +
  labs(
    title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
    y = "log(volume)", color = NULL) +
  theme_minimal()
# The fitted trend does not fit the actual data in the beginning and middle of series. The fitted trend tracks the actual better during the last third of the series. Overall, I think the fitted does an okay job, but I don't think it has predictive abilities past the year 2000.

# Checking residuals of Canadian gas production model
can_gas_fit |> gg_tsresiduals()

# Using the formal Ljung-Box test to check for autocorrelation in the residuals
can_gas_fit |> 
  augment() |> 
  features(.innov, ljung_box, lag = 24)
# The ljung-box test displayed a p_value of 0, which means we have enough evidence to reject the null that the residuals are independent and are white noise. This means that the residuals are correlated and there is more to be modeled. 

# Forecasting Canadian gas production using the TSLM model with and expanding window
gas_cv_fc <- can_gas |> 
  select(Month, Volume) |> 
  slice(-n()) |> 
  stretch_tsibble(.init = 240, .step =1) |> 
  model(snaive = SNAIVE(Volume),
        tslm = TSLM(log(Volume) ~ trend() + season())) |> 
  forecast(h = 1)

# Comparing the accuracy of the two forecasting models
gas_cv_fc |> 
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# First observation date: Jan 1980
# Last observation date: Feb 2005
#The model that has the lowest MAE and RMSE is teh seasonal random walk model. THis means that the seasonal random walk model is the better model to use for forecasting Canadian gas production.

# Diebold-Marino Test
errs <- gas_cv_fc |> 
  as_tibble() |> 
  left_join(as_tibble(can_gas) |> 
              select(Month, actual = Volume),
            by = "Month") |> 
  mutate( e = actual - .mean) |> 
  select(Month, .model, e) |> 
  pivot_wider(names_from = .model, values_from = e) |> 
  na.omit() 

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)
# The diebold-marino test has a p-value of 2.2e-16 which is statistically significant, meaning I can reject the null hypotheis that the two models are equally accurate. With a DM = -31.278, means that the seasonal random walk model is more accurate than the time series linear model. 

# Forecasting using a rolling window
gas_cv_fc_slide <- can_gas |> 
  select(Month, Volume) |> 
  slice(-n()) |> 
  slide_tsibble(.size = 240, .step = 1) |> 
  model(snaive = SNAIVE(Volume),
        tslm = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_slide |> 
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
# The rolling window model has a lower MAE and RMSE than the expanding model. This is consistent with the forecasting techniques, becaues the fitted values from before show a consistent growth over the period of the series. I know that if a series changes over time than a rolling window will supply a more accurate forecast than an expanding window.

