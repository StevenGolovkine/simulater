# data-raw/powerconsumption.R
# Data import and processing pipeline

library(readr)

powerconsumption <- read_csv("./data-raw/householdpowerconsumption.csv",
                             col_names = FALSE)

usethis::use_data(powerconsumption, overwrite = TRUE)
