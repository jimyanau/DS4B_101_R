# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    "tidyverse"
    , "readxl"
)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bikes_tbl
orderlines_tbl
bike_orderlines_tbl %>% glimpse()

# 1.0 Selecting Columns with select() ----
bike_orderlines_tbl %>%
    select(order_date, order_id, order_line)

bike_orderlines_tbl %>%
    select(1:3)


bike_orderlines_tbl %>%
    select(starts_with("order_"))

# reduce columns
bike_orderlines_tbl %>%
    select(order_date, total_price, category_1, category_2)

# rearange colums
bike_orderlines_tbl %>%
    select(bikeshop_name:state, everything())

bike_orderlines_tbl %>%
    select(ends_with("price"))

# pull()
bike_orderlines_tbl %>%
    select(total_price) %>%
    pull() %>%
    mean()

# select_if()
bike_orderlines_tbl %>%
    select_if(is.numeric)

bike_orderlines_tbl %>%
    select_if(~ !is.numeric(.))

# 2.0 Arranging with arrange() and desc() ----
bikes_tbl %>%
    select(model, price) %>%
    arrange(desc(price))




# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----
bikes_tbl %>%
    select(model, price) %>%
    filter(price > mean(price))

bikes_tbl %>%
    select(model, price) %>%
    filter((price > 5000) | (price < 1000)) %>%
    arrange(desc(price))

bikes_tbl %>%
    select(model, price) %>%
    filter(price > 6000, model %>% str_detect("Supersix")) 

bike_orderlines_tbl %>%
    filter(category_2 %in% c("Over Mountain","Trail"))

bike_orderlines_tbl %>%
    filter(!(category_2 %in% c("Over Mountain","Trail"))) %>% select(category_2) %>% unique()

# 3.2 slice(): filtering with row number(s) ----
bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(1:5)

bikes_tbl %>%
    arrange(price) %>%
    slice(1:5)

# 3.3 Distinct: Unique Values ----
bike_orderlines_tbl %>%
    distinct(category_1)
    
bike_orderlines_tbl %>%
    distinct(category_1, category_2) %>%
    arrange(category_1)

# 4.0 Adding Columns with mutate() ----
bike_orderlines_prices <- bike_orderlines_tbl %>%
    select(order_date, model, quantity, price) %>%
    mutate(total_price = quantity * price)

# Transformations
bike_orderlines_prices %>%
    mutate(total_price_log = log(total_price)) %>%
    mutate(total_price_sqrt = sqrt(total_price))

# Adding a flag
bike_orderlines_prices %>%
    mutate(
        is_supersix = model %>% 
            str_to_lower() %>% 
            str_detect("supersix")
        ) %>%
    filter(is_supersix)

bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 3))

# If-then inside of mutate case_when() binning more flexible
bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned_2 = case_when(
        total_price > quantile(total_price, 0.66) ~ "High"
        , total_price > quantile(total_price, 0.33) ~ "Medium"
        , T ~ "Low"
    )) %>%
    mutate(total_price_binned_3 = case_when(
        total_price > quantile(total_price, 0.75) ~ "High"
        , total_price > quantile(total_price, 0.25) ~ "Medium"
        , T ~"Low"
    ))

bike_orderlines_prices %>%
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix"
        , model %>% str_to_lower() %>% str_detect("Jekyll") ~ "Jekyll"
        , T ~ "Not Supersix or Jekyll"
    ))

# 5.0 Grouping & Summarizing with group_by() and summarize() ----
# Basics
bike_orderlines_prices %>%
    summarise(
        revenue = sum(total_price)
    )

bike_orderlines_tbl %>%
    group_by(category_1) %>%
    summarise(revenue = sum(total_price))

bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(revenue))

# Summary functions
bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(
        count = n()
        , avg = mean(total_price)
        , median = median(total_price)
        , sd = sd(total_price)
        , min = min(total_price)
        , max = max(total_price)
        ) %>%
    ungroup() %>%
    arrange(desc(count))

bike_orderlines_missing <- bike_orderlines_tbl %>%
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

bike_orderlines_missing %>%
    summarise_all(~ sum(is.na(.)))
 
bike_orderlines_missing %>%
    summarise_all(~ sum(is.na(.)) / length(.))

bike_orderlines_missing %>%
    filter(!is.na(total_price))

# 6.0 Renaming columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----


# 6.2 set_names: All columns at once ---




# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----

# 7.1 spread(): Long to Wide ----


# 7.2 gather(): Wide to Long ----




# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----




# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----




# 9.2 bind_rows() ----



