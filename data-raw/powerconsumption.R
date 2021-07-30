# data-raw/powerconsumption.R
# Data import and processing pipeline

powerconsumption <- readr::read_csv("./data-raw/householdpowerconsumption.csv",
                                    col_names = FALSE)
powerconsumption <- tidyr::drop_na(powerconsumption)

usethis::use_data(powerconsumption, overwrite = TRUE)
