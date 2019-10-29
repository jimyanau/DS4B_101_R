# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    "tidyverse"
    , "lubridate"
    , "tidyquant"
)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# 1.0 Date & Lubridate Basics ----
# 1.1 Character vs Date/Datetime
order_date_tbl <- bike_orderlines_tbl %>%
    select(order_date)

order_date_tbl %>%
    pull(order_date) %>%
    class()

# 1.2 Date Classes
order_date_tbl %>%
    mutate(order_date_char = as.character(order_date)) %>%
    mutate(order_date_char2 = order_date_char %>% str_c(" 00:00:00")) %>%
    
    # ymd
    mutate(order_date_date = order_date_char %>% ymd()) %>%
    mutate(order_date_dttm = order_date_char2 %>% ymd_hms())


# 1.3 Lubridate Functions
# Conversion
"06/01/18" %>% mdy() %>% class()
"06/01/18 12:30:15" %>% mdy_hms()
"January 1, 1985" %>% mdy()

# Extractor



# Helpers



# Periods & Durations - Add/subract time to/from a date



# Intervals - Calculate time-based distance 



# 2.0 Time-Based Data Grouping ----







# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----





# 3.2 Difference from first observation ----





# 4.0 Cumulative Calculations ----




# 5.0 Rolling Calculations ----



# 6.0 Filtering Date Ranges ---- 



