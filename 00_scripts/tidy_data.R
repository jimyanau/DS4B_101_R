# Tidy data example ----
pacman::p_load(
    "tidyverse",
    "readxl"
)

bikeshop_data_wide <- read_excel("bikeshop_revenue_formatted_wide.xlsx")

# wide format ----
bikeshop_data_wide

# make data into long format ----
bikeshop_data_long <- bikeshop_data_wide %>%
    select(-Total) %>%
    gather(key = "category_1", value = "sales", Mountain, Road)

# linear model ----
model <- lm(sales ~ ., data = bikeshop_data_long)
model
