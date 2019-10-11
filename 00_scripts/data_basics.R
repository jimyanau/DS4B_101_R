# Data Basics ----

library(tidyverse)

# Data Types ----
a <- c(1,2,3)
a
a %>% class()

b <- c("low","medium","high")
b
class(b)

# Data Structure ----
as_tbl <- tibble(
    a,
    b
)

as_tbl

read_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")
