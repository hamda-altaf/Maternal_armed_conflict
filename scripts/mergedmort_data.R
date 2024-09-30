library(here)
library(tidyverse)
library(tidyr)
here()
#read all the files
maternal_rawdat <- read.csv(here("Original", "maternalmortality.csv"), header = TRUE)
raw_infant <- read.csv(here("Original", "infantmortality.csv"), header = TRUE)
raw_neonatal <- read.csv(here("Original", "neonatalmortality.csv"), header = TRUE)
raw_under5 <-read.csv(here("Original", "under5mortality.csv"), header = TRUE)
#Write a general function to clean the data
clean_data <- function(data,datamor) {
cleandata <- data %>% select(Country.Name,X2000:X2019) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Year", names_prefix = "X", values_to = datamor)
cleandata$Year <- as.numeric(cleandata$Year) 
return(cleandata)}
#apply the function
maternaldata <- clean_data(maternal_rawdat, datamor="Matmor")
infantdata <- clean_data(raw_infant, datamor="Infantmor")
neonataldata <- clean_data(raw_neonatal, datamor="Neonatmor")
under5data <- clean_data(raw_under5, datamor="Under5mor")
#merge the 4 datasets by country and year
list_data <- list(maternaldata, infantdata, neonataldata, under5data) # to create a list
lapply(list_data, FUN = summary) # to summarize the list
mergedmort_data <- reduce(list_data, full_join, by = c("Country.Name", "Year"))
#add the ISO-3 country code variable to the new data set and remove the Country name variable
install.packages("countrycode")
library(countrycode)
mergedmort_data$ISO <- countrycode(mergedmort_data$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")
#what is this for? 
mergedmort_data <- mergedmort_data[,-1]
View(mergedmort_data)
#read in the disaster dataset
disasterdata <- read.csv(here("Original", "disaster.csv"), header = TRUE)
# Filter the data for the years 2000 to 2019 and disaster types "Earthquake" and "Drought"
subset_data <- disasterdata %>% filter(Year >= 2000 & Year <= 2019 & Disaster.Type %in% c("Earthquake", "Drought"))
names(disasterdata)
subset_data <- subset_data %>% select(Year, ISO, Disaster.Type)
subset_data <- subset_data %>% mutate(drought = ifelse(Disaster.Type == "Drought", 1, 0),earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0))
clean_disaster_data <- subset_data %>% group_by(Year, ISO) %>% summarize(drought = max(drought),earthquake = max(earthquake)) %>% ungroup()

conflictdata <- read.csv(here('Original', 'conflictdata.csv'), header = TRUE)




