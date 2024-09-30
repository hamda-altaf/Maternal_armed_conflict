library(here)
library(dplyr)
library(purrr)
library(tidyr)
#read in conflict data
conflictdata <- read.csv(here('Original', 'conflictdata.csv'), header = TRUE)

#derive the binary armed conflict variable that was lagged by a year in the analysis
derived_conflictdata <- conflictdata %>% 
  group_by(year, ISO) %>%
  summarise(totaldeath = max(best))  %>%
  mutate(conflict = if_else(totaldeath < 25, 0, 1))  %>%
  mutate(year = year + 1)

