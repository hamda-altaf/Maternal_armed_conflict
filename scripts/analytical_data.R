library(here)
library(dplyr)
here()
covs <- read.csv(here('Original', 'covariates.csv')) 
#use source to call the R scripts that create the individual datasets
source(here('scripts', 'create_disaster.R')) 
source(here('scripts', 'derived_conflictdata.R')) 
source(here('scripts', 'mergedmort_data.R')) 
#renaming the first column in disaster and merged mortality datasets as 'year'
colnames(clean_disaster_data)[1] <- 'year'
colnames(mergedmort_data)[1] <- 'year' 

alllist <- list(derived_conflictdata, mergedmort_data, clean_disaster_data)
#country and year variables will be used as keys to join the data
final_merged_data <- reduce(alllist, full_join, by = c("ISO", "year"))

analytical_data <- left_join(covs, final_merged_data, by = c("ISO", "year"))

analytical_data<- analytical_data %>%
  mutate(drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totaldeath = replace_na(totaldeath, 0))

write.csv(analytical_data, file = here("analytical_data", "analyticaldata.csv"), row.names = FALSE)

