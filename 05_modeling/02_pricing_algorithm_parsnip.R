# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# REGRESSION MODELS ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM


# LIBRARIES & DATA ----

pkgs <- c("parsnip", "glmnet", "rpart", "rpart.plot", "ranger", "randomForest", "xgboost", "kernlab")
# If any of these packages are not installed, run this: install.packages(pkgs)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    # Standard
    "readxl",
    "tidyverse",
    "tidyquant",
    
    # Modeling
    "parsnip",
    
    # Preprocessing and Sampling
    "recipes",
    "rsample",
    
    # Model Error Metrics
    "yardstick",
    
    # Plot Decision Trees
    "rpart.plot",
    "ggrepel"
)

# Source Scripts
source("00_scripts/separate_bikes_and_outier_detection.R")

# Read Data
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PROBLEM DEFINITION ----
# - Which Bike Categories are in high demand?
# - Which Bike Categories are under represented?
# - GOAL: Use a pricing algorithm to determine a new product price in a category gap

model_sales_tbl <- bike_orderlines_tbl %>%
    select(total_price, model, category_2, frame_material) %>%
    
    group_by(model, category_2, frame_material) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(total_sales))

model_sales_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(total_sales, .fun = max) %>% 
               fct_rev()) %>%
    
    ggplot(aes(frame_material, total_sales)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    #coord_flip() +
    facet_wrap(~ category_2) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
    theme_tq() +
    labs(
        title = "Total Sales for Each Model",
        x = "Frame Material", y = "Revenue"
    )


# 2.0 TRAINING & TEST SETS ----
bike_features_tbl <- bike_orderlines_tbl %>%
    select(price, model, category_2, frame_material) %>%
    distinct() %>%
    mutate(id = row_number()) %>%
    select(id, everything()) %>%
    separate_bike_model(keep_model_column = TRUE, append = TRUE)

set.seed(seed = 1113)
split_obj <- rsample::initial_split(
    data = bike_features_tbl,
    prop = 0.80,
    strata = "model_base"
    )

split_obj %>%
    training()

split_obj %>%
    testing()

train_tbl <- training(split_obj)
test_tbl <- testing(split_obj)

# 3.0 LINEAR METHODS ----
?linear_reg
?set_engine
?fit


# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----
model_01_linear_lm_simple <- linear_reg(mode = "regression") %>%
    set_engine(engine = "lm") %>%
    fit(
        formula = price ~ category_2 + frame_material,
        data = train_tbl
    )

model_01_linear_lm_simple %>%
    predict(
        new_data = test_tbl
    ) %>%
    bind_cols(test_tbl %>% select(price)) %>%
    # Yardstick
    yardstick::metrics(
        truth = price,
        estimate = .pred
    )
    # mutate(res = price - .pred) %>%
    # # MAE
    # summarize(
    #     MAE = abs(res) %>% mean(na.rm = TRUE),
    #     RMSE = sqrt(mean(res^2, na.rm = TRUE))
    # )

# 3.1.2 Feature Importance ----
model_01_linear_lm_simple$fit %>% 
        tidy() %>%
        arrange(p.value) %>%
        mutate(term = as_factor(term) %>% fct_rev()) %>%
        ggplot(
            mapping = aes(
                x = estimate,
                y = term
            )
        ) +
        geom_point() +
        geom_label_repel(
            mapping = aes(
                label = scales::dollar(estimate, accuracy = 1)
            )
        ) +
        labs(
            title = "Linear Regression Feature Importance",
            subtitle = "Model 01 Simple LM Model"
        ) +
        theme_tq()

# 3.1.3 Function to Calculate Metrics ----
calc_metrics <- function(model, new_data = test_tbl){
    
    model %>%
        predict(new_data = new_data) %>%
        bind_cols(new_data %>% select(price)) %>%
        metrics(truth = price, estimate = .pred)
}
model_01_linear_lm_simple %>% calc_metrics()

# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

# 3.2.1 Model ----
train_tbl

model_02_linear_lm_complex <- linear_reg(mode = "regression") %>%
    set_engine(engine = "lm") %>%
    fit(
        data = train_tbl %>% select(-id, -model, -model_tier),
        formula = price ~ .
    )
model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)

# 3.2.2 Feature importance ----
model_02_linear_lm_complex$fit %>% 
    tidy() %>%
    arrange(p.value) %>%
    mutate(term = as_factor(term) %>% fct_rev()) %>%
    ggplot(
        mapping = aes(
            x = estimate,
            y = term
        )
    ) +
    geom_point() +
    geom_label_repel(
        mapping = aes(
            label = scales::dollar(estimate, accuracy = 1)
        )
    ) +
    labs(
        title = "Linear Regression Feature Importance",
        subtitle = "Model 02 Complex LM Model"
    ) +
    theme_tq()


# 3.3 PENALIZED REGRESSION ----

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet

model_03_linear_glmnet <- linear_reg(
    mode = "regression", 
    penalty = 100, 
    mixture = 0.2
    ) %>%
    set_engine(engine = "glmnet") %>%
    fit(
        data = train_tbl %>% select(-id, -model, -model_tier),
        formula = price ~ .
    )

model_03_linear_glmnet %>% calc_metrics(new_data = test_tbl)

# 3.3.2 Feature Importance ----
model_03_linear_glmnet$fit %>%
    tidy() %>%
    filter(lambda >= 10, lambda < 11) %>%
    arrange(desc(estimate)) %>%
    mutate(term = as_factor(term) %>% fct_rev()) %>%
    ggplot(
        mapping = aes(
            x = estimate,
            y = term
        )
    ) +
    geom_point() +
    geom_label_repel(
        mapping = aes(
            label = scales::dollar(estimate, accuracy = 1)
        )
    ) +
    scale_x_continuous(labels = scales::dollar) +
    labs(
        title = "Linear Regression Feature Importance",
        subtitle = "Model 03 Complex GLMNET Model"
    ) +
    theme_tq()
    


# 4.0 TREE-BASED METHODS ----

# 4.1 DECISION TREES ----

# 4.1.1 Model ----
?decision_tree
?rpart::rpart

model_04_tree_decision_tree <- decision_tree(
    mode = "regression",
    cost_complexity = 0.1,
    tree_depth = 5,
    min_n = 10
) %>%
    set_engine(
        engine = "rpart"
    ) %>%
    fit(
        formula = price ~ .,
        data = train_tbl %>% select(-id, -model, -model_tier)
    )

model_04_tree_decision_tree %>%
    calc_metrics(new_data = test_tbl)

# 4.1.2 Decision Tree Plot ----
model_04_tree_decision_tree$fit %>%
    rpart.plot(
        roundint = FALSE,
        type = 1,
        extra = 101,
        fallen.leaves = FALSE,
        cex = 0.8,
        main = "Model 04 Decision Tree",
        box.palette = "Grays"
    )




# 4.2 RANDOM FOREST ----

# 4.2.1 Model: ranger ----
?rand_forest()
?ranger::ranger

model_05_rand_forest_ranger <- rand_forest(
    mode = "regression",
    mtry = 8,
    trees = 5000
    ) %>%
    set_engine(
        engine = "ranger",
        importance = "impurity"
    ) %>%
    fit(
        formula = price ~ ., 
        data = train_tbl %>% select(-id, -model, -model_tier)
    )

model_05_rand_forest_ranger %>% 
    calc_metrics(new_data = test_tbl)

# 4.2.2 ranger: Feature Importance ----
model_05_rand_forest_ranger$fit %>%
    ranger::importance() %>%
    enframe() %>%
    arrange(desc(value)) %>%
    mutate(name = as_factor(name) %>% fct_rev()) %>%
    ggplot(
        mapping = aes(
            x = value,
            y = name
        )
    ) +
    geom_point() +
    labs(
        title = "Ranger Feature Importance",
        subtitle = "Model 05 Ranger Random Forest Model",
        x = "",
        y = ""
    ) +
    theme_tq()



# 4.2.3 Model randomForest ----
?rand_forest()
?randomForest::randomForest

set.seed(1234)
model_06_rand_forest_randomForest <- rand_forest(
        mode = "regression",
        trees = 1000
    ) %>%
    set_engine(
        engine = "randomForest"
    ) %>%
    fit(
        formula = price ~ .,
        data = train_tbl %>% select(-id, -model, -model_tier)
    )
model_06_rand_forest_randomForest %>%
    calc_metrics(new_data = test_tbl)

# 4.2.4 randomForest: Feature Importance ----
model_06_rand_forest_randomForest$fit %>%
    randomForest::importance() %>%
    as_tibble(
        rownames = "name"
    ) %>%
    arrange(desc(IncNodePurity)) %>%
    mutate(name = as_factor(name) %>% fct_rev()) %>%
    ggplot(
        mapping = aes(
            x = IncNodePurity,
            y = name
        )
    ) +
    geom_point() +
    labs(
        title = "randomForest: Variable Importance",
        subtitle = "Model 06: randomForest Random Forest"
    ) +
    theme_tq()


# 4.3 XGBOOST ----

# 4.3.1 Model ----
?boost_tree
?xgboost::xgboost

set.seed(1234)
model_07_boost_tree_xgboost <- boost_tree(
        mode = "regression",
        learn_rate = 0.275,
        min_n = 4,
        tree_depth = 7
    ) %>%
    set_engine(
        engine = "xgboost"
    ) %>%
    fit(
        formula = price ~ .,
        data = train_tbl %>% select(-id, -model, -model_tier),
        
    )
model_07_boost_tree_xgboost %>%
    calc_metrics(new_data = test_tbl)

# 4.3.2 Feature Importance ----
model_07_boost_tree_xgboost$fit %>%
    xgboost::xgb.importance(model = .) %>%
    as_tibble() %>%
    arrange(desc(Gain)) %>%
    mutate(Feature = as_factor(Feature) %>% fct_rev()) %>%
    ggplot(
        mapping = aes(
            x = Gain,
            y = Feature
        )
    ) +
    geom_point() +
    labs(
        title = "XGBoost: Feature Importance",
        subtitle = "Model 07 XGBoost Model"
    ) +
    theme_tq()




# 5.0 TESTING THE ALGORITHMS OUT ----

g1 <- bike_features_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(price)) %>%
    
    ggplot(aes(category_2, price)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    coord_flip() +
    facet_wrap(~ frame_material) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    labs(
        title = "Unit Price for Each Model",
        y = "", x = "Category 2"
    )

# 5.1 NEW JEKYLL MODEL ----

new_over_mountain_jekyll <- tibble(
    model = "Jekyll Al 1",
    frame_material = "Aluminum",
    category_2 = "Over Mountain",
    model_base = "Jekyll",
    model_tier = "Aluminum 1",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


# Linear Methods ----
predict(model_03_linear_glmnet, new_data = new_over_mountain_jekyll)


# Tree-Based Methods ----
predict(model_07_boost_tree_xgboost, new_data = new_over_mountain_jekyll)

# Iteration
models_tbl <- tibble(
    model_id = str_c("Model 0",1:7),
    model = list(
        model_01_linear_lm_simple,
        model_02_linear_lm_complex,
        model_03_linear_glmnet,
        model_04_tree_decision_tree,
        model_05_rand_forest_ranger,
        model_06_rand_forest_randomForest,
        model_07_boost_tree_xgboost
    )
)
models_tbl

# Add predictions
predictions_new_over_mountain_tbl <- models_tbl %>%
    mutate(
        predictions = map(
            model, 
            predict, 
            new_data = new_over_mountain_jekyll
            )
        ) %>%
    unnest(cols = predictions) %>%
    mutate(category_2 = "Over Mountain") %>%
    left_join(
        new_over_mountain_jekyll,
        by = "category_2"
    )
    
predictions_new_over_mountain_tbl

# Update plot
g2 <- g1 +
    geom_point(
        data = predictions_new_over_mountain_tbl,
        mapping = aes(
            y = .pred
        ),
        color = "red",
        alpha = 0.5
    ) +
    ggrepel::geom_text_repel(
        data = predictions_new_over_mountain_tbl,
        mapping = aes(
            label = model_id,
            y = .pred
        ),
        size = 3
    )

g2

# 5.2 NEW TRIATHALON MODEL ----

new_triathalon_slice_tbl <- tibble(
    model = "Slice Al 1",
    frame_material = "Aluminum",
    category_2 = "Triathalon",
    model_base = "Slice",
    model_tier = "Ultegra",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


# Linear Methods ----
predict(model_02_linear_lm_complex, new_data = new_triathalon_slice_tbl)

# Tree-Based Methods ----
predict(model_06_rand_forest_randomForest, new_data = new_triathalon_slice_tbl)

# Add predictions
predictions_new_triathalon_slice_tbl <- models_tbl %>%
    mutate(
        predictions = map(
            model, 
            predict, 
            new_data = new_triathalon_slice_tbl
        )
    ) %>%
    unnest(cols = predictions) %>%
    mutate(category_2 = "Triathalon") %>%
    left_join(
        new_triathalon_slice_tbl,
        by = "category_2"
    )

predictions_new_over_mountain_tbl

# Vizualize
g3 <- g2 +
    geom_point(
        data = predictions_new_triathalon_slice_tbl,
        mapping = aes(
            y = .pred
        ),
        color = "red",
        alpha = 0.5
    ) +
    ggrepel::geom_text_repel(
        data = predictions_new_triathalon_slice_tbl,
        mapping = aes(
            label = model_id,
            y = .pred
        ),
        size = 3
    )
    
g3

# 6.0 ADDITIONAL ADVANCED CONCEPTS ----

# - CLASSIFICATION - Binary & Multi-Class
# - ADVANCED ALGORITHMS
#   - SVMs - svm_poly() and svm_rbf() - Must be normalized
#   - Neural Networks - keras - Must be normalized
#   - Stacking Models 
# - PREPROCESSING - recipes 
# - HYPERPARAMETER TUNING - purrr
# - SAMPLING & CROSS VALIDATION - rsample 
# - AUTOMATIC MACHINE LEARNING - H2O


# 7.0 Bonus Pre-processing and SVM Regression ----
library(recipes)

?recipe
?step_dummy
?prep
?bake

train_tbl
recipe_obj <- recipe(
    formula = price ~ .,
    data = train_tbl
    ) %>%
    step_rm(id, model, model_tier) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_log(price) %>%
    step_center(price) %>%
    step_scale(price) %>%
    prep()

train_transformed_tbl <- bake(recipe_obj, train_tbl)
test_transformed_tbl <- bake(recipe_obj, test_tbl)
tidy(recipe_obj)

scale <- tidy(recipe_obj, 5)
center <- tidy(recipe_obj, 4)
log <- tidy(recipe_obj, 3)

# SVM Radial Basis
?svm_rbf
?kernlab::ksvm
model_08_svm_rbf <- svm_rbf(
    mode = "regression",
    cost = 13,
    margin = 0.2
    ) %>%
    set_engine(
        "kernlab",
        scaled = FALSE
    ) %>%
    fit(
        price ~ .,
        data = train_transformed_tbl
    )

model_08_svm_rbf %>%
    predict(new_data = test_transformed_tbl) %>%
    mutate(.pred = .pred * scale$value) %>%
    mutate(.pred = .pred + center$value) %>%
    mutate(.pred = exp(.pred)) %>%
    bind_cols(test_tbl %>% select(price)) %>%
    metrics(
        truth = price,
        estimate = .pred
    )

# Prediction
bake(recipe_obj, new_over_mountain_jekyll) %>%
    predict(
        object = model_08_svm_rbf,
        new_data = .
    ) %>%
    mutate(
        .pred = .pred * scale$value,
        .pred = .pred + center$value,
        .pred = exp(.pred)
    )

bake(recipe_obj, new_triathalon_slice_tbl) %>%
    predict(
        object = model_08_svm_rbf,
        new_data = .
    ) %>%
    mutate(
        .pred = .pred * scale$value,
        .pred = .pred + center$value,
        .pred = exp(.pred)
    )

# 8.0 Saving and Loading Models ----
fs::dir_create("00_models")

models_tbl <- list(
    "MODEL_01__LM_SIMPLE" = model_01_linear_lm_simple,
    "MODEL_02__LM_COMPLEX" = model_02_linear_lm_complex,
    "MODEL_03__LM_GLMNET" = model_03_linear_glmnet,
    "MODEL_04__DECISION_TREE" = model_04_tree_decision_tree,
    "MODEL_05__RF_RANGER" = model_05_rand_forest_ranger,
    "MODEL_06__RF_RANDOMFOREST" = model_06_rand_forest_randomForest,
    "MODEL_07__RF_XGBOOST" = model_07_boost_tree_xgboost,
    "MODEL_08__SVM" = model_08_svm_rbf
) %>%
    enframe(
        name = "model_id",
        value = "model"
    )

models_tbl %>%
    write_rds("00_models/parsnip_models_tbl.rds")

recipes_tbl <- list(
    "RECIPE_01" = recipe_obj
) %>%
    enframe(
        name = "recipe_id",
        value = "recipe"
    )
recipes_tbl %>%
    write_rds("00_models/recipes_tbl.rds")

calc_metrics %>%
    write_rds("00_scripts//calc_metrics.rds")
