library(here)
library(dplyr)
here()
analyticaldata <- read.csv(here("analytical_data", "analyticaldata.csv"), header = TRUE)
names(analyticaldata)
analyticaldata %>%
  dplyr::filter(country_name == "Canada")
analyticaldata %>%
  dplyr::filter(country_name == "Ecuador")
