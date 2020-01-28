# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----
library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)
library(patchwork)

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

na_values <- function(data){
    # create empty matrix to store NA values
    na_vals <- matrix(0, nrow =1, ncol = ncol(data))
    
    # populate each column of empty matrix with count of NA values
    for(i in 1:ncol(data)){
        na_vals[, i] = sum(is.na(data[, i]))
    }
    
    # convert the matrix to data frame and assign column names to the data frame
    na_vals <- as.data.frame(na_vals)
    colnames(na_vals) <- colnames(data)
    
    # create data frame containing variable names, NA value count and proportion
    # save the data frame to global environment
    na_vals <<- data.frame(variable = colnames(na_vals), na_val = t(na_vals),
                           prop_na = t(100*na_vals/nrow(data)), row.names = NULL)
}

#na_values(nbclust_data)

cluster_vi <- function(dataset, target){
    
    # # target variable
    # target <- c("RESPONSE_FLAG")
    #
    # data for clustering
    data_clust <- dataset%>%select(-target)
    
    # cluster validity indices
    indices <- list("silhouette","ch", "dunn", "hartigan")
    
    # empty list to store cluster validity indices
    cl_vi <- list()
    
    for(i in 1:length(indices)){
        index <- toString(i)
        n_clust <- NbClust(data = data_clust, index = indices[[i]], method = "kmeans", max.nc = 20)$Best.nc[["Number_clusters"]]
        cl_vi[[index]] <- append(cl_vi[[index]], n_clust)
    }
    
    assign(paste0("clvi"), cl_vi, inherits = T)
}
cluster_vi(customer_product_tbl, "bikeshop_name")

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
umap_obj <- customer_product_tbl %>%
    select(-bikeshop_name) %>%
    umap()

umap_results_tbl <- umap_obj$layout %>%
    as_tibble() %>%
    set_names("x","y") %>%
    bind_cols(
        customer_product_tbl %>% select(bikeshop_name)
    )

uwot_obj <- customer_product_tbl %>%
    select(-bikeshop_name) %>%
    uwot::umap()

uwot_results_tbl <- uwot_obj  %>%
    as_tibble() %>%
    set_names("x","y") %>%
    bind_cols(
        customer_product_tbl %>% select(bikeshop_name)
    )

umap_results_plt <- umap_results_tbl %>%
    ggplot(
        mapping = aes(
            x = x
            , y = y
        )
    ) +
    geom_point(color = "red") +
    geom_label_repel(aes(label = bikeshop_name), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "UMAP 2D Projection"
        , subtitle = "UMAP R Package"
    )

uwot_results_plt <- uwot_results_tbl %>%
    ggplot(
        mapping = aes(
            x = x
            , y = y
        )
    ) +
    geom_point(color = "red") +
    geom_label_repel(aes(label = bikeshop_name), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "UMAP 2D Projection"
        , subtitle = "UWOT R Package"
    )
    
umap_results_plt + uwot_results_plt

# 3.2 Use K-Means to Add Cluster Assignments ----
kmeans_4_obj <- kmeans_mapped_tbl %>%
    pull(k_means) %>%
    pluck(4)

kmeans_4_clusters_tbl <- kmeans_4_obj %>% 
    augment(customer_product_tbl) %>%
    select(bikeshop_name, .cluster)

umap_kmeans_4_results_tbl <- umap_results_tbl %>%
    left_join(kmeans_4_clusters_tbl, by = c("bikeshop_name" = "bikeshop_name"))


uwot_kmeans_4_results_tbl <- uwot_results_tbl %>%
    left_join(kmeans_4_clusters_tbl, by = c("bikeshop_name" = "bikeshop_name"))

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----
umap_kmeans_4_results_tbl %>%
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster: {.cluster}")
           ) %>%
    ggplot(
        mapping = aes(
            x = x
            , y = y
            , color = .cluster
        )
    ) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation: 2D Projection"
        , subtitle = "UMAP 2D Projection with K-Means Cluster Assignment"
        , x = ""
        , y = ""
    ) +
    theme(
        legend.position = "none"
    )


# 4.0 ANALYZE PURCHASING TRENDS ----
cluster_trends_tbl <- customer_trends_tbl %>%
    # join cluster assignment by bikeshop name
    left_join(umap_kmeans_4_results_tbl, by = c("bikeshop_name" = "bikeshop_name")) %>%
    mutate(price_bin = case_when(
        price <= 2240 ~ "low"
        , price <= 4260 ~ "medium"
        , TRUE ~ "high"
    )) %>%
    select(.cluster, model, contains("price"), category_1:quantity_purchased) %>%
    # aggregate quant purchased by clsuter and product attribute
    group_by_at(.vars = vars(.cluster:frame_material)) %>%
    summarise(total_quantity = sum(quantity_purchased, na.rm = TRUE)) %>%
    ungroup() %>%
    # Calculate proportion of total
    group_by(.cluster) %>%
    mutate(prop_of_total = total_quantity / sum(total_quantity)) %>%
    ungroup()

cluster_trends_tbl %>%
    filter(.cluster == 1) %>%
    arrange(desc(prop_of_total)) %>%
    mutate(cum_prop = cumsum(prop_of_total))

get_cluster_trends <- function(cluster = 1) {
    cluster_trends_tbl %>%
        filter(.cluster == cluster) %>%
        arrange(desc(prop_of_total)) %>%
        mutate(cum_prop = cumsum(prop_of_total))
}

get_cluster_trends(cluster = 4)

# Update viz
cluster_label_tbl <- tibble(
    .cluster = 1:4
    , .cluster_label = c(
        "Medium High Price Road"
        , "Low to Medium Price Point, Moutain, Aluminum"
        , "Low to Medium Price Point, Road, Aluminum/Carbon"
        , "High Price, Moutain, Carbon"
    )
) %>%
    mutate(.cluster = as_factor(as.character(.cluster)))

umap_kmeans_4_results_tbl %>%
    left_join(cluster_label_tbl, by = c(".cluster" = ".cluster")) %>%
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster: {.cluster}
                                 {.cluster_label}")
    ) %>%
    ggplot(
        mapping = aes(
            x = x
            , y = y
            , color = .cluster
        )
    ) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation: 2D Projection"
        , subtitle = "UMAP 2D Projection with K-Means Cluster Assignment"
        , x = ""
        , y = ""
    ) +
    theme(
        legend.position = "none"
    )
