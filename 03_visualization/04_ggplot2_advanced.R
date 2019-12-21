# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# ADVANCED BUSINESS PLOTS ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <- 15

# Data Manipulation
top_customers_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, total_price) %>%
    mutate(bikeshop_name = bikeshop_name %>% as_factor() %>% fct_lump(n = n, w = total_price)) %>%
    group_by(bikeshop_name) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup() %>%
    mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>%
    mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after = 0)) %>%
    arrange(desc(bikeshop_name)) %>%
    # revenue text
    mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = "M")) %>%
    # cumulative percent
    mutate(cum_pct = cumsum(revenue)/sum(revenue)) %>%
    mutate(cum_pct_text = scales::percent(cum_pct)) %>%
    # Get a rank
    mutate(rank = row_number()) %>%
    mutate(rank = case_when(
        rank == max(rank) ~ NA_integer_
        , TRUE ~ rank
    )) %>%
    # Label Text
    mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))

# Data Visualization
top_customers_tbl %>%
    ggplot(
        mapping = aes(
            x = revenue
            , y = bikeshop_name
        )
    ) +
    geom_segment(
        mapping = aes(
            xend = 0
            , yend = bikeshop_name
        )
        , color = palette_light()[1]
        , size = 1
    ) +
    geom_point(
        color = palette_light()[1]
        , mapping = aes(
            size = revenue
        )
    ) +
    geom_label(
        mapping = aes(
            label = label_text
        )
        , hjust = "inward"
        , size = 3
        , color = palette_light()[1]
    ) +
    # Formatting
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    labs(
        title = str_glue("Top {n} Customers")
        , subtitle = str_glue(
            "Start: {year(min(bike_orderlines_tbl$order_date))}
            End:  {year(max(bike_orderlines_tbl$order_date))}"
        )
        , x = "Revenue ($M)"
        , y = "Customer"
        , caption = str_glue("Top 6 customers contribute
                             51% of purchasing power")
    ) +
    theme_tq() +
    theme(
        legend.position = "none"
        , plot.title = element_text(face = "bold")
        , plot.caption = element_text(face = "bold")
    )




# 2.0 Heatmaps ----
# - Great for showing details in 3 dimensions

# Question: Do specific customers have a purchasing prefernce?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

# Data Manipulation
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, category_1, category_2, quantity) %>%
    group_by(bikeshop_name, category_1, category_2) %>%
    summarise(total_quantity = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(bikeshop_name) %>%
    mutate(pct = total_quantity/sum(total_quantity)) %>%
    ungroup() %>%
    # List shops by alpha
    mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>%
    mutate(bikeshop_name_num = bikeshop_name %>% as.numeric())

# Data Visualization
pct_sales_by_customer_tbl %>%
    ggplot(
        mapping = aes(
            x = category_2
            , y = bikeshop_name
        )
    ) +
    # Geometries
    geom_tile(
        mapping = aes(
            fill = pct
        )
        #, color = "black"
    ) +
    geom_text(
        mapping = aes(
            label = scales::percent(pct, accuracy = .01)
        )
        , size = 3
    ) +
    facet_wrap(~ category_1, scales = "free_x") +
    # Formatting
    scale_fill_gradient(
        low = "white"
        , high = palette_light()[1]
    ) +
    labs(
        title = "Heatmap of Purchasing Habits"
        , x = "Bike Type (Cateogry 2)"
        , y = "Customer"
        , caption = str_glue(
            "Customers that prefer Road: 
            Ann Arbor Speed, Austin Cruisers, & Indianapolis Velocipedes
                                 
            Customers that prefer Mountain: 
            Ithica Mountain Climbers, Pittsburgh Mountain Machines, & Tampa 29ers"
        )
    ) +
    theme_tq() +
    theme(
        legend.position = "none"
        , axis.text.x = element_text(angle = 45, hjust = 1)
        , plot.caption = element_text(face = "bold.italic")
        , plot.title = element_text(face = "bold")
    )



