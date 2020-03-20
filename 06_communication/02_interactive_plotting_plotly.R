# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# INTERACTIVE PLOTS ----

# GOAL: DEVELOP INTERACTIVE PLOTS FOR A SALES REPORT


# LIBRARIES & DATA ----

# Main
library(tidyverse)
library(lubridate)

# Visualization
library(tidyquant)
library(plotly)


bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 1.0 TOTAL SALES BY MONTH ----

# 1.1 Preparing Time Series Data ----
total_sales_m_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
    group_by(date_rounded) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

# Formatting Dates
# - strftime: https://devhints.io/strftime



# 1.2 Interactive Plot ----

# Step 1: Create ggplot with text feature
g1 <- total_sales_m_tbl %>%
    ggplot(
        mapping = aes(
            x = date_rounded,
            y = total_sales
        )
    ) +
    # Geoms
    geom_point(
        mapping = aes(
            text = label_text
        )
    ) +
    geom_smooth(method = "loess", span = 0.2) +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    expand_limits(y = 0)

g1

# Step 2: Use ggplotly()
ggplotly(g1, tooltip = "text")



# 1.3 Plot Total Sales Function ----
plot_total_sales <- function(unit = "month",
                             date_format = "%B %Y",
                             interactive = TRUE){
    
    # Body
    # span
    span <- case_when(
        unit == "day" ~ 1/30,
        unit == "week" ~ 1/12,
        unit == "month" ~ 1/3,
        unit == "quarter" ~ 1/2,
        unit == "year" ~ 1,
        TRUE ~ 1
    )
    
    # Handle Data
    data_tbl <- bike_orderlines_tbl %>%
        select(order_date, total_price) %>%
        mutate(date_rounded = floor_date(order_date, unit = unit)) %>%
        group_by(date_rounded) %>%
        summarise(total_sales = sum(total_price)) %>%
        ungroup() %>%
        mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}"))
    
    # Make Plot
    g1 <- data_tbl %>%
        ggplot(
            mapping = aes(
                x = date_rounded,
                y = total_sales
            )
        ) +
        # Geoms
        geom_point(
            mapping = aes(
                text = label_text
            )
        ) +
        geom_smooth(
            method = "loess", 
            span = span,
            se = FALSE
        ) +
        theme_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        expand_limits(y = 0)
    
    # Static vs Interactive Logic
    if(interactive) {
        return(ggplotly(g1, tooltip = "text"))
    } else {
        return(g1)
    }
    
}


# 1.4 Test Our Function ----
plot_total_sales(unit = "month", date_format = "%b %Y", interactive = TRUE)





# 2.0 CATEGORY 2 SALES BY MONTH ----

# 2.1 Preparing Time Series Data ----


# 2.2 Interactive Plot ----



# Step 2: Use ggplotly()



# 2.3 Plot Categories Function ----



# 2.4 Test Our Function ----




# 3.0 SAVE FUNCTIONS ----






