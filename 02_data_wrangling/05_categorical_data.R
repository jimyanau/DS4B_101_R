# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl



# 1.0 Factor Basics ----

# What is a Factor?
# A way of managing categorical data

# Why do we want factors? 
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much eaiser (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type for categorical data. 

# 2.0 Motivating Example -----

# Manipulation


# Plotting



# 3.0 Forcats Basics ----


# 3.1 Inspecting Factors ----

# Vector


# Tibble



# 3.2 Creating Factors: as_factor() vs as.factor() ----





# 3.3 Reording Factors: fct_reorder() and fct_rev() ----





# 3.4 Time-Based Reordering: fct_reorder2() ----





# 3.5 Creating "Other" Category - fct_lump() & fct_shift() ----





