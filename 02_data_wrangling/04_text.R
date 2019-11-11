# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TEXT MANIPULATION ----

library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl


# 1.0 Basics ----

# 1.1 Detection: Used with filter() ----

# Vector
c("Supersix Evo Black Inc.", "Supersix Evo Hi-Mod Team") %>%
    str_detect(pattern = "Supersix")

# Tibble
bikes_tbl %>%
    select(model) %>%
    mutate(supersix = model %>% str_detect(pattern = "Supersix") %>% as.numeric()) %>%
    mutate(black = model %>% str_detect(pattern = "Black") %>% as.numeric())


# 1.2 Case & Concatenation ----


# Case
bikeshop_name <- "Ithaca Mountain Climbers"
str_to_upper(bikeshop_name)
str_to_lower(bikeshop_name)
str_to_sentence(bikeshop_name)
str_to_title(bikeshop_name)

# Concatenation

# Vector
order_id <- 1
order_line <- 1

str_c(
    "Order Line: ", order_line, ". ", "Order ID: ", order_id, 
    " sent to Customer: ", bikeshop_name
)

str_glue("Order Line: {order_id}, with Order ID: {order_id}, was sent to Customer: {str_to_upper(bikeshop_name)}")

# Tibble
bike_orderlines_tbl %>%
    select(bikeshop_name, order_id, order_line) %>%
    mutate(purchase_statement = str_glue(
        "Order Line: {order_id}, with Order ID: {order_id}, was sent to Customer: {str_to_upper(bikeshop_name)}"
    ) %>% as.character())

# 1.3 Separating Text: See tidyr::separate() ----

# Vector
c("Road - Elite Road - Carbon", "Road - Elite Road") %>%
    str_split(pattern = " - ", simplify = T)

# Tibble
bikes_tbl %>%
    select(description) %>%
    separate(
        col      = description
        , into   = c(
            "Category_1"
            , "Category_2"
            ,"Frame_Material"
        )
        , sep    = " - "
        , remove = F
    )


# 1.4 Trimming Text ----
" text with spaces    " %>% 
    str_trim(side = "both")


# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
c("CAAD12","CAAD","CAAD8") %>%
    str_replace(pattern = "[0-9]", replacement = "")
c("CAAD12","CAAD","CAAD8") %>%
    str_replace_all(pattern = "[0-9]", replacement = "")

# Tibble
bikes_tbl %>%
    select(model) %>%
    mutate(
        model_num_removed = model %>% 
            str_replace_all(pattern = "[0-9]",replacement = "") %>% 
            str_trim()
    )


# 1.6 Formatting Numbers ----

# values
1e6 %>% scales::number(prefix = "$", big.mark = ",", suffix = "M")
value <- 1e6
(value / 1e6) %>% scales::number(prefix = "$", suffix = "M")
value %>% scales::dollar()

# percents
pct <- 0.15
pct %>% scales::number(scale = 100, suffix = "%")
pct %>% scales::percent()

# 1.7 Formatting Column Names ----

# Replacing text in column names
bike_orderlines_tbl %>%
    set_names(names(.) %>% str_replace(pattern = "_",".") %>% str_to_upper())


# Appending text to column names
bike_orderlines_tbl %>%
    set_names(str_glue("{names(.)}_bike☼"))

# Appending text to specific column names
bike_orderlines_colnames_tbl <- bike_orderlines_tbl %>%
    rename_at(
        .vars = vars(model:frame_material)
        , .funs = ~ str_c("prod_", .)
    ) %>%
    rename_at(
        .vars = vars(bikeshop_name:state)
        , .funs = ~ str_c("cust_", .)
    )

bike_orderlines_colnames_tbl %>%
    select(
        contains("cust_")
        , total_price
    )

# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features
bikes_tbl %>%
    
    select(model) %>%
    
    # fix typo(s)
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra"
        , model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra"
        , TRUE ~ model
    )) %>%
    
    # separate using the spaces
    separate(
        col      = model
        , into   = str_c(
            "model_", 1:7
        )
        , sep    = " "
        , remove = F
        , fill   = "right"
        , extra  = "drop"
    ) %>%
    
    # create base model feture
    mutate(
        model_base = case_when(
            # Supersix Evo fix
            str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " ")
            
            # Beast of the east
            , str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " ")
            
            # Fat CAAD
            , str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " ")
            
            # Bad Habit
            , str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " ")
            
            # Scalpel 29
            , str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " ")
            
            # Catch all
            , TRUE ~ model_1
        )
    ) %>%
    
    # Get tier
    mutate(model_tier = model %>% str_replace(pattern = model_base, replacement = "") %>% str_trim()) %>%
    
    # remove unnecesary columns
    select(-matches("[0-9]"))
