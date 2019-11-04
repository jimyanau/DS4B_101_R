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
"2011-01-01" %>% ymd() %>% year()

"2011-01-01" %>% ymd() %>% month()

"2011-01-01" %>% ymd() %>% day()
"2011-01-01" %>% ymd() %>% wday(label = T, abbr = T)

# Helpers
now()
today()

# Periods & Durations - Add/subract time to/from a date
today() + days(12)  # period
today() + ddays(12) # duration
today() + years(4)  # period
today() + dyears(4) # duration

# Intervals - Calculate time-based distance 
i <- interval(today(), today() + ddays(12))
i / ddays(1)
i / dminutes(1)

# 2.0 Time-Based Data Grouping ----
bike_sales_y_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    
    # lubridate funcs
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    # group by and summarize
    group_by(year) %>%
    summarize(
        Sales = sum(total_price)
        , Sales_Dollars = Sales %>% scales::dollar()
    ) %>%
    ungroup()

bike_sales_y_tbl

bike_orderlines_tbl %>%
    tibbletime::as_tbl_time(index = order_date) %>%
    select(order_date, total_price) %>%
    tibbletime::collapse_by("yearly") %>%
    group_by(order_date) %>%
    summarize(Sales = sum(total_price)) %>%
    ungroup()

# Monthly
bike_sales_m_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    
    # lubridate
    mutate(order_date = ymd(order_date)) %>%
    mutate(
        year = year(order_date)
        , month = month(order_date, label = T)
    ) %>%
    group_by(year, month) %>%
    summarize(
        Sales = sum(total_price)
        , Sales_Text = Sales %>% scales::dollar()
    ) %>%
    ungroup() %>%
    set_names(
        "Sales Year"
        , "Sales Month"
        , "Sales"
        , "Sales Text"
    )

bike_sales_m_tbl

# Floor Date
bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    
    # lubridate
    mutate(order_date = ymd(order_date)) %>%
    mutate(year_month = floor_date(x = order_date, unit = "month")) %>%
    
    group_by(year_month) %>%
    summarize(
        Sales = sum(total_price) 
    ) %>%
    ungroup()

# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----
bike_sales_y_tbl %>%
    mutate(sales_lag_1 = lag(Sales, n = 1)) %>%
    
    # handle na's
    mutate(sales_lag_1 = case_when(
        is.na(sales_lag_1) ~ Sales
        , TRUE ~ sales_lag_1
    )) %>%
    
    # Diffs and pct diffs
    mutate(
        diff_1 = Sales - sales_lag_1
        , pct_diff_1 = diff_1 / sales_lag_1
        , pct_diff_1_text = scales::percent(pct_diff_1)
    )

calculate_pct_diff <- function(data){
    
    data %>%
        mutate(sales_lag_1 = lag(Sales, n = 1)) %>%
        
        # handle na's
        mutate(sales_lag_1 = case_when(
            is.na(sales_lag_1) ~ Sales
            , TRUE ~ sales_lag_1
        )) %>%
        
        # Diffs and pct diffs
        mutate(
            diff_1 = Sales - sales_lag_1
            , pct_diff_1 = diff_1 / sales_lag_1
            , pct_diff_1_text = scales::percent(pct_diff_1)
        )
    
}

bike_sales_m_tbl %>%
    calculate_pct_diff()

# 3.2 Difference from first observation ----
bike_sales_y_tbl %>%
    mutate(
        sales_2011 = first(Sales)
        , diff_2011 = Sales - sales_2011
        , pct_diff_2011 = diff_2011 / sales_2011
        , pct_diff_2011_text = scales::percent(pct_diff_2011)
    )

bike_sales_m_tbl %>%
    group_by(`Sales Year`) %>%
    mutate(sales_jan = first(Sales)) %>%
    mutate(
        difference_jan = Sales - sales_jan
        , pct_diff_jan = difference_jan / sales_jan
        , pct_diff_text = scales::percent(pct_diff_jan)
    ) %>%
    ungroup()


# 4.0 Cumulative Calculations ----
bike_sales_y_tbl %>%
    mutate(
        cumulative_sales = cumsum(Sales)
        #, cumulative_sales_avg = cummean(Sales)
    ) %>%
    mutate(cumulative_sales_pct = cumulative_sales / sum(Sales)) %>%
    mutate(cumulative_sales_pct_chg = cumulative_sales_pct %>% scales::percent())

bike_sales_m_tbl %>%
    group_by(`Sales Year`) %>%
    mutate(cumulative_sales = cumsum(Sales)) %>%
    mutate(cumulative_sales_pct = cumulative_sales / sum(Sales)) %>%
    mutate(cumulative_sales_pct_chr = cumulative_sales_pct %>% scales::percent())


# 5.0 Rolling Calculations ----
# i.e. moving average etc.
bike_sales_m_tbl %>%
    mutate(roll_mean_3 = rollmean(x = Sales, k = 3, na.pad = T, align = "right", fill = 0)) %>%
    mutate(roll_mean_6 = rollmean(x = Sales, k = 6, na.pad = T, align = "right", fill = NA))


# 6.0 Filtering Date Ranges ---- 
bike_orderlines_tbl %>%
    mutate(order_date = ymd(order_date)) %>%
    filter(
        order_date %>% between(left = ymd("2012-01-01"), right = ymd("2013-12-31"))
    )

bike_orderlines_tbl %>%
    mutate(order_date = ymd(order_date)) %>%
    filter(year(order_date) %in% c(2012, 2013))
    
    