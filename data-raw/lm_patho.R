## use for creating `lm_patho` dataset
lm_patho <- read.csv("data-raw/lm_patho.csv", as.is = TRUE)

usethis::use_data(lm_patho, overwrite = TRUE)
