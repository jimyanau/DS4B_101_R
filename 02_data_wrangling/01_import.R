# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----


# 1.0 Load libraries ----
if(!require(pacman)) install.packages("pacman")

pacman::p_load(
    #containes readr
    "tidyverse",
    # Excel connection
    "readxl",
    "writexl",
    # DB Connection
    "odbc",
    "RSQLite"
)

# 2.0 readr ----

# 2.1 CSV ----
bike_orders_csv_tbl <- read_csv("00_data/bike_sales/data_wrangled/bike_orderlines.csv")
problems(bike_orders_csv_tbl)

# 2.2 RDS ----




# 3.0 Excel ----




# 4.0 Databases  ----


