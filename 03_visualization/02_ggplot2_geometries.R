# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
    select(order_id, order_line, total_price, quantity) %>%
    group_by(order_id) %>%
    summarize(
        total_quantity = sum(quantity)
        , total_price = sum(total_price)
    ) %>%
    ungroup()

# Scatter Plot
order_value_tbl %>%
    ggplot(
        mapping = aes(
            x = total_quantity
            , y = total_price
        )
    ) +
    geom_point(alpha = 0.312, position = "jitter", size = 2) +
    geom_smooth(method = "lm", se = F)



# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
    group_by(year_month) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

# Line Plot
revenue_by_month_tbl %>%
    ggplot(
        mapping = aes(
            x = year_month
            , y = revenue
        )
    ) +
    geom_line() +
    geom_smooth(span = 0.2)



# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>% 
    select(category_2, total_price) %>%
    group_by(category_2) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%
    mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
    ggplot(
        mapping = aes(
            x = category_2
            , y = revenue
        )
    ) +
    geom_col(
        fill = "#2c3d50"
    ) +
    coord_flip()
    

# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable
bike_orderlines_tbl %>%
    distinct(model, price) %>%
    ggplot(
        mapping = aes(
            x = price
        )
    ) +
    geom_histogram(
        bins = 25
        , fill = "blue"
        , color = "white"
    ) + 
    tidyquant::theme_tq()

# Goal: Unit price of bicycles
# Histogram
bike_orderlines_tbl %>%
    distinct(price, model, frame_material) %>%
    ggplot(
        mapping = aes(
            x = price
            , fill = frame_material
        )
    ) +
    geom_histogram() +
    facet_wrap(~ frame_material, ncol = 1) +
    tidyquant::theme_tq() +
    tidyquant::scale_fill_tq()

# Goal: Unit price of bicylce, segmenting by frame material
# Histogram


# Density
bike_orderlines_tbl %>%
    distinct(price, model, frame_material) %>%
    ggplot(
        mapping = aes(
            x = price
            , fill = frame_material
        )
    ) +
    geom_density(
        alpha = 0.618
    ) +
    tidyquant::scale_fill_tq() +
    tidyquant::theme_tq()



# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation
unit_price_by_cat2_tbl <- bike_orderlines_tbl %>%
    distinct(category_2, model, price) %>%
    mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(price))

# Box Plot
unit_price_by_cat2_tbl %>%
    ggplot(
        mapping = aes(
            x = category_2
            , y = price
        )
    ) +
    geom_boxplot() +
    coord_flip() +
    tidyquant::theme_tq()

# Violin Plot & Jitter Plot
unit_price_by_cat2_tbl %>%
    ggplot(
        mapping = aes(
            x = category_2
            , y = price
        )
    ) +
    geom_violin() +
    geom_jitter(width = 0.2, color = "#2c3e50") +
    coord_flip() +
    tidyquant::theme_tq()






# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation


# Adding text to bar chart


# Filtering labels to highlight a point





