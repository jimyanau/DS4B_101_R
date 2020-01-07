# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
excel_paths_tbl <- fs::dir_info("00_data/bike_sales/data_raw/")

paths_chr <- excel_paths_tbl %>%
    pull(path)

# What Not To Do: Don't use for loops
excel_list <- list()
for (path in paths_chr) {
    print(path)
    excel_list[[path]] <- read_excel(path = path)
}

# What to Do: Use map()
?map

# Method 1
excel_list2 <- paths_chr %>%
    map(.f = read_excel) %>%
    set_names(paths_chr)

# Method 2 w anonymous func
paths_chr %>%
    map(~ read_excel(.)) %>%
    set_names(paths_chr)

# Method 3 using secified func
paths_chr %>%
    map(function(x) read_excel(path = x)) %>%
    set_names(paths_chr)

# Reading Excel Sheets
excel_sheets("00_data/bike_sales/data_raw/bikes.xlsx") %>%
    map(~ read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx", sheet = .))



# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----
bike_orderlines_tbl %>% is.list()

bike_orderlines_tbl %>%
    map(~ class(.)[1])

# Char vector
bike_orderlines_tbl %>%
    map_chr(~ class(.)[1])

# data frame
bike_orderlines_tbl %>%
    map_df(~ class(.)[1]) %>%
    #glimpse()
    gather()

bike_orderlines_tbl %>%
    map_df(~ sum(is.na(.)) / length(.)) %>%
    gather()

# 2.2 Map Variants ----

# 2.3 Row-wise Map ----
excel_tbl <- excel_paths_tbl %>%
    select(path) %>%
    mutate(data = path %>% map(read_excel))

# good format
excel_list
# best format
excel_tbl


# 3.0 NESTED DATA ----

# Unnest
excel_tbl$data

excel_tbl$data[[3]]

excel_tbl_unnested <- excel_tbl %>%
    unnest(cols = c(data))

excel_tbl_unnested

# Nest
excel_tbl_nested <- excel_tbl_unnested %>%
    group_by(path) %>%
    nest()

excel_tbl_nested

# Mapping Nested List Columns
excel_tbl_nested$data[[1]] %>%
    select_if(~ !is.na(.) %>% all())

# Method 1: Creating a function outside of purrr::map()
# Step 1: Create a function that can be mapped to one element
select_non_na_columns <- function(data) {
    
    data %>%
        select_if(~ !is.na(.) %>% all())
    
}
# Step 2: extract an element and test function
excel_tbl_nested$data[[1]] %>%
    select_non_na_columns()
# Step 3: use mutate() + map()
excel_tbl_nested_fixed <- excel_tbl_nested %>%
    mutate(data_fix = data %>% map(select_non_na_columns))

# Step 4: check results
excel_tbl_nested_fixed$data_fix[[1]]

# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))




# 4.2 Modeling Primer ----

# Data Preparation
sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>%
    filter(category_2 == 'Cross Country Race') %>%
    select(month_end, total_price) %>%
    mutate(month_end_numeric = as.numeric(month_end))

sales_by_m_cross_country_tbl %>%
    ggplot(
        mapping = aes(
            x = month_end_numeric
            , y = total_price
        )
    ) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.2, se = FALSE)

# Making a loess model
fit_loess_cross_country <- sales_by_m_cross_country_tbl %>%
    loess(total_price ~ month_end_numeric, data = ., span = 0.2)

fit_loess_cross_country

# Working With Broom
fit_loess_cross_country %>%
    broom::augment() %>%
    
# Visualizing Results
    ggplot(
        mapping = aes(
            x = month_end_numeric
            , y = total_price
        )
    ) +
    geom_point() +
    geom_line(
        mapping = aes(
            y = .fitted
        )
        , color = "blue"
    )
    

# 4.3 Function To Return Fitted Results ----
# Step 1: develop function to return fitted results
rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>%
    group_by(category_1, category_2) %>%
    nest()


data <- rolling_avg_3_tbl_nested$data[[1]]

tidy_loess <- function(data, span = 0.2) {
    
    data_formatted <- data %>%
        select(month_end, total_price) %>%
        mutate(month_end_numeric = as.numeric(month_end))
    
    fit_loess <- loess(
        formula = total_price ~ month_end_numeric
        , data = data_formatted
        , span = span
    )
    
    output_tbl <- fit_loess %>%
        broom::augment() %>%
        select(.fitted)
    
    return(output_tbl)
    
}

# 4.4 Test Function on Single Element ----
rolling_avg_3_tbl_nested$data[[1]] %>%
    tidy_loess()

# 4.5 Map Function to All Categories ----

# Map Functions
loess_tbl_nested <- rolling_avg_3_tbl_nested %>%
    mutate(fitted = data %>% map(tidy_loess))

loess_tbl_nested$fitted[[1]]

loess_tbl_nested %>%
    unnest(cols = c(data, fitted))

# Visualize Results
loess_tbl_nested %>%
    unnest(cols = c(data, fitted)) %>%
    ggplot(
        mapping = aes(
            x = month_end
            , y = total_price
            , color = category_2
        )
    ) +
    geom_point() +
    geom_line(
        mapping = aes(
            y = .fitted
        )
        , color = "blue"
        , size = 2
    ) +
    facet_wrap(~ category_2, scales = "free_y") +
    theme_tq() +
    theme(
        legend.position = "none"
    ) +
    labs(
        x = "Month End"
        , y = "Total Price"
    )


