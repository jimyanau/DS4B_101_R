get_customer_segments <-
function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    customer_trends_tbl <- bike_orderlines_tbl %>%
        select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity) %>%
        group_by_at(.vars = vars(bikeshop_name:frame_material)) %>%
        summarise(total_qty = sum(quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(bikeshop_name) %>%
        mutate(pct = total_qty / sum(total_qty, na.rm = TRUE)) %>%
        ungroup()
    
    customer_product_tbl <- customer_trends_tbl %>%
        select(bikeshop_name, model, pct) %>%
        pivot_wider(
            id_cols = bikeshop_name,
            names_from = model,
            values_from = pct,
            values_fill = list(pct = 0)
        )
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    set.seed(seed = seed)
    k_means_obj <- customer_product_tbl %>%
        select(-bikeshop_name) %>%
        kmeans(centers = k, nstart = 100)
    
    kmeans_tbl <- k_means_obj %>%
        augment(customer_product_tbl) %>%
        select(bikeshop_name, .cluster)
    
    # 3.0 UWOT
    umap_obj <- customer_product_tbl %>%
        select(-bikeshop_name) %>%
        #as.matrix() %>%
        uwot::umap()
    
    umap_tbl <- umap_obj %>%
        as_tibble() %>%
        set_names(c("x","y")) %>%
        bind_cols(
            customer_product_tbl %>%
                select(bikeshop_name)
        )
    
    # 4.0 COMBINE UMAP & K-MEANS
    combined_tbl <- umap_tbl %>%
        left_join(kmeans_tbl, by = "bikeshop_name") %>%
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Cluster: {.cluster}"))
    
    return(combined_tbl)
}
plot_customer_segments <-
function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    combined_tbl <- get_customer_segments(k = k, seed = seed)
    
    # VISUALIZATION
    g <- combined_tbl %>%
        ggplot(
            mapping = aes(
                x = x,
                y = y,
                color = .cluster
            )
        ) +
        # Geometry
        geom_point(
            mapping = aes(
                text = label_text
            )
        ) +
        # Formatting
        theme_tq() +
        scale_color_tq() +
        labs(
            title = "Customer Segementation: 2D Projection",
            subtitle = "UWOT::UMAP 2D Projection with K-Means Cluster Assignment"
        ) +
        theme(
            legend.position = "none"
        )
    
    # INTERACTIVE VS STATIC
    if(interactive) {
        return(ggplotly(g, tooltip = "text"))
    } else {
        g <- g +
            geom_label_repel(
                mapping = aes(
                    label = label_text
                ),
                size = 2
            )
        return(g)
    }
    
}
plot_customer_heatmap <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
        select(bikeshop_name, category_1, category_2, quantity) %>%
        group_by(bikeshop_name, category_1, category_2) %>%
        summarise(total_qty = sum(quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(bikeshop_name) %>%
        mutate(pct = total_qty / sum(total_qty, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>%
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Category = {category_1}
                                     Sub-Category = {category_2}
                                     Quantity Purchased: {total_qty}
                                     Percent of Sales: {scales::percent(pct)}"))
    
    # VISUALIZATION
    g <- pct_sales_by_customer_tbl %>%
        ggplot(
            mapping = aes(
                x = category_2,
                y = bikeshop_name
            )
        ) +
        # Geometry
        geom_tile(
            mapping = aes(
                fill = pct
            )
        ) +
        geom_text(
            mapping = aes(
                label = scales::percent(pct),
                text = label_text
            ),
            size = 3
        ) +
        facet_wrap(~ category_1, scales = "free_x") +
        
        # Formatting
        scale_fill_gradient(low = "white", high = "#2c3e50") +
        theme_tq() +
        theme(
            axis.text.x = element_text(
                angle = 45,
                hjust = 1
            ),
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            strip.text.x = element_text(margin = margin(5,5,5,5, unit = "pt"))
        ) +
        labs(
            y = "",
            x = "",
            title = "Heatmap of Purchasing Habits"
        )
    
    # INTERACTIVE VS STATIC
    if(interactive) {
        g <- g +
            labs(x = "", y = "")
        return(ggplotly(g, tooltip = "text"))
    } else {
        g <- g +
                labs(x = "Bike Type (Category 2)", y = "Customer")
        
        return(g)
    }
    
}
plot_customer_behavior_by_cluster <-
function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}
