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

read_csv("00_data/bike_sales/data_wrangled/bike_orderlines.csv",
         col_types = cols(
             order_id = col_double()
             )
         )

# 2.2 RDS ----
bike_orders_rds_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")
bike_orders_rds_tbl


 # 3.0 Excel ----
bike_orders_xlsx_tbl <- read_excel("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx")
excel_sheets("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx")


# 4.0 Databases  ----


