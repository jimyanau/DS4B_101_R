# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# FORMATTING GGPLOTS ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(category_2 = fct_reorder2(category_2, year, revenue))

sales_by_year_category_2_tbl

sales_by_year_category_2_tbl %>%
    mutate(category_2_num = as.numeric(category_2)) %>%
    arrange(category_2_num)


# 1.0 Working with Colors ----
# 1.1 Color Conversion ----
# Named Colors
colors()

# To RGB
col2rgb("slateblue")

col2rgb("#2c3e50")

# To HEX
rgb(106,90,205, maxColorValue = 255) 

# 1.2 Color Palettes ----

# tidyquant
tidyquant::palette_light()

palette_light()[1] %>% col2rgb()

# Brewer
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal(n = 100, name = "Blues") %>% col2rgb()

# Viridis
viridisLite::viridis(n = 20)



# 2.0 Aesthetic Mappings ----

# 2.1 Color  -----
# - Used with line and points, Outlines of rectangular objects
sales_by_year_category_2_tbl %>%
    ggplot(
        mapping = aes(
            x = year
            , y = revenue
            , color = category_2 # global
        )
    ) +
    geom_line() + # can define color here as local
    geom_point()


# 2.2 Fill  -----
# - Used with fill of rectangular objects 
sales_by_year_category_2_tbl %>%
    ggplot(
        mapping = aes(
            x = year
            , y = revenue
            , fill = category_2
        )
    ) +
    geom_col() +
    scale_y_continuous(
        labels = scales::dollar_format(scale = 1/1e6, suffix = "M")
    )


# 2.3 Size ----
# - Used with points

sales_by_year_category_2_tbl %>%
    ggplot(
        mapping = aes(
            x = year
            , y = revenue
        )
    ) +
    geom_line(
        mapping = aes(
            color = category_2
            , size = 1
        )
    ) +
    geom_point(
        mapping = aes(
            size = revenue
        )
    )



# 3.0 Faceting ----
# - Great way to tease out variation by category

# Goal: Sales annual sales by category 2
sales_by_year_category_2_tbl %>%
    ggplot(
        mapping = aes(
            x = year
            , y = revenue
            , color = category_2
        )
    ) +
    geom_line(size = 1, color = "black") +
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~ category_2, scales = "free_y") +
    scale_y_continuous(
        labels = scales::dollar_format(
            scale = 1/1e6
            , suffix = "M"
        )
    ) +
    expand_limits(
        y = 0
    ) +
    labs(
        title = "Sales by Category 2"
        , color = "Category 2"
        , x = ""
        , y = "Revenue"
    )



# 4.0 Position Adjustments (Stack & Dodge) ----

# Stacked Bars & Side-By-Side Bars
sales_by_year_category_2_tbl %>%
    ggplot(
        mapping = aes(
            x = year
            , y = revenue
            , fill = category_2
        )
    ) +
    geom_col(
        #position = "dodge" # default is "stack"
        position = position_dodge(
            width =   0.9
        )
        , color = "white"
    ) 
    

# Stacked Area




# 5.0 Scales (Colors, Fills, Axis) ----

# 5.1 Plot Starting Points ----
# - Continuous (e.g. Revenue): Changes color via gradient palette
# - Categorical (e.g. ): Changes color via discrete palette

# Plot 1: Faceted Plot, Color = Continuous Scale


# Plot 2: Faceted Plot, Color = Discrete Scale


# Plot 3: Stacked Area Plot




# 5.2 Scale Colors & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

# Color by Revenue (Continuous Scale)




# Color by Category 2 (Discrete Scale)



# Fill by Category 2

 


# 5.3 Axis Scales ----





# 6.0 Labels ----




# 7.0 Themes  ----




# 8.0 Putting It All Together ----







