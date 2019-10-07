# DS4B 101 R: R FOR BUSINESS ANALYSIS
# PACKAGE INSTALLATION
# UPDATED 10-07-2019 TO USE pacman::p_load() BY STEVEN SANDERSON

if(!require(pacman)) install.packages("pacman")

pacman::p_load(
    # File System
    "fs",         # working with the file system
    
    # Import
    "readxl",     # reading excel files
    "tidycells",  # Read tabular data from diverse sources and easily make them tidy
    "writexl",    # saving data as excel files
    "odbc",       # connecting to databases
    "RSQLite",    # connecting to SQLite databases
    
    # Tidy, Transform, & Visualize
    "tidyverse",  # dplyr, ggplot2, tibble, tidyr, readr, purrr, stringr, forcats
    "lubridate",  # working with dates and times
    "tidyquant",  # used mainly for the ggplot plotting theme
    
    # Model
    "tidymodels", # installs broom, infer, recipes, rsample, & yardstick
    "umap",       # used for visualizing clusters
    
    # Other
    "devtools"    # used to install non-CRAN packages
)
