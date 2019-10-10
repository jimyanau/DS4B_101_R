# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    # Work horse packages
    "tidyverse",
    "lubridate",
    
    # theme_tq()
    "tidyquant",
    
    # Excel Files
    "readxl",
    "writexl"    
)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")
bikeshops_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikeshops.xlsx")
orderlines_tbl <- read_excel(path = "00_data/bike_sales/data_raw/orderlines.xlsx")


 # 3.0 Examining Data ----
glimpse(bikes_tbl)
bikes_tbl

glimpse(bikeshops_tbl)
bikeshops_tbl

glimpse(orderlines_tbl)
orderlines_tbl



# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
    # Separate description field into it's sub componenets
    # Category 1, Category 2 and frame.material
    separate(
        col = description
        , into = c('category.1','category.2','frame.material')
        , sep = ' - '
        , remove = T
        ) %>%
    # separate location column
    separate(
        col = location
        , into = c("city","state")
        , sep = ', '
        , remove = F
    ) %>%
    # Get total order price qty * unit price
    mutate(
        total.price = quantity * price
    ) %>%
    # Reorg with select()
    select(-...1, -location) %>%
    select(-ends_with(".id")) %>%
    # Add order.id back in
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    # Reorder columns
    select(
        contains("date")
        , contains("id")
        , contains("order")
        , quantity
        , price
        , total.price
        , everything()
    ) %>%
    # change column names with rename()
    set_names(
        names(.) %>% str_replace_all("\\.", "_")
    )

bike_orderlines_wrangled_tbl %>% glimpse()

# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    # Get columns we want
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    # groupings
    group_by(year) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    # get dollar text
    mutate(sales_text = scales::dollar(sales))



# Step 2 - Visualize
sales_by_year_tbl %>%
    ggplot(
        mapping = aes(
            x = year
            , y = sales
        )
    ) +
    geom_col(
        fill = palette_light()[[1]]
    ) +
    geom_label(
        aes(
            label = sales_text
        )
    )
    theme_tq() +
    labs(
        title = "Sales by Year"
    )


# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate




# Step 2 - Visualize




# 7.0 Writing Files ----


# 7.1 Excel ----


# 7.2 CSV ----


# 7.3 RDS ----