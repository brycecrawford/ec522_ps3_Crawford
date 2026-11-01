
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



