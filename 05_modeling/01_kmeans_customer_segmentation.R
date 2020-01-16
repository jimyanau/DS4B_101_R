# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection

# 1.1 Get Customer Trends ----
customer_trends_tbl <- bike_orderlines_tbl %>%
    # Customer and product attributes
    select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity) %>%
    # Group by and summarize
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>%
    summarize(
        quantity_purchased = sum(quantity, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Normalize
    group_by(bikeshop_name) %>%
    mutate(prop_of_total = quantity_purchased / sum(quantity_purchased)) %>%
    ungroup()
customer_trends_tbl

# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----
customer_product_tbl <- customer_trends_tbl %>%
    select(bikeshop_name, model, prop_of_total) %>%
    pivot_wider(
        names_from = model
        , values_from = prop_of_total
        , values_fill = list(prop_of_total = 0)
    )
customer_product_tbl


# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ----
kmeans_obj <- customer_product_tbl %>%
    # Want all numeric columns
    select(-bikeshop_name) %>%
    kmeans(
        centers = 5
        , nstart = 100
    )

# 2.2 Tidying a K-Means Object ----
kmeans_obj %>% tidy() %>% glimpse()
kmeans_obj %>% glance() %>% glimpse()
kmeans_obj %>% augment(customer_product_tbl) %>% select(bikeshop_name, .cluster)


# 2.3 How many centers (customer groups) to use? ----
kmeans_mapper <- function(centers = 3) {
    
    # Body
    customer_product_tbl %>%
        select(-bikeshop_name) %>%
        kmeans(
            centers = centers
            , nstart = 100
        )
    
}
kmeans_mapper(3) %>% glance()

# Mapping the function to many elements
kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
    mutate(k_means = centers %>% map(kmeans_mapper)) %>%
    mutate(glance = k_means %>% map(glance))

kmeans_mapped_tbl %>%
    unnest(glance) %>%
    select(centers, tot.withinss)

# 2.4 Skree Plot ----
kmeans_mapped_tbl %>%
    unnest(glance) %>%
    select(centers, tot.withinss) %>%
    ggplot(
        mapping = aes(
            x = centers
            , y = tot.withinss
        )
    ) +
    geom_point() +
    geom_line() +
    ggrepel::geom_label_repel(mapping = aes(label = centers)) +
    theme_tq() +
    labs(
        title = "Skree Plot"
        , subtitle = "Measures the distance each of the customers are from the clesest k-means cluster"
        , caption = "Based on the Skree Plot we will select 4 clusters"
    )



# 3.0 VISUALIZATION: UMAP ----

# 3.1 Use UMAP to get 2-D Projection ----


# 3.2 Use K-Means to Add Cluster Assignments ----


# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----




# 4.0 ANALYZE PURCHASING TRENDS ----




