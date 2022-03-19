# data-raw/brownian.R
# Data import and processing pipeline

brownian <- readr::read_csv("./data-raw/brownian.csv", col_names = TRUE)

usethis::use_data(brownian, overwrite = TRUE)
