matmordata <- read.csv(here("matmor_data", "maternalmortality.csv"), header = TRUE)

# Filter the dataset for the year 2017
data_2017 <- maternalmortality[maternalmortality$year == 2017, ]

# Find the countries where 'Matmor' is missing
missing_matmor <- data_2017[is.na(data_2017$Matmor), ]$ISO

# Count the unique number of countries with missing 'Matmor'
num_missing_matmor <- length(unique(missing_matmor))

# Print the result
cat("Number of countries with missing Matmor in 2017:", num_missing_matmor, "\n")

library(readr)
maternalmortality <- read_csv("Original/maternalmortality.csv")
View(maternalmortality)

# Filter the dataset for the year 2017
data_2017 <- analyticaldata[analyticaldata$year == 2017, ]

# Exclude countries with missing Matmor data
data_2017_valid <- data_2017[!is.na(data_2017$Matmor), ]

# Filter for countries exposed to armed conflict (conflict == 1)
conflict_data_2017 <- data_2017_valid[data_2017_valid$conflict == 1, ]

# Calculate the median of Matmor for countries exposed to armed conflict
median_matmor_conflict <- median(conflict_data_2017$Matmor)

# Print the result
cat("Sample median of maternal mortality rate in 2017 for countries exposed to conflict:", median_matmor_conflict, "\n")
set.seed(123)  # For reproducibility

# Filter the dataset for the year 2017
data_2017 <- analyticaldata[analyticaldata$year == 2017, ]

# Exclude countries with missing Matmor data
data_2017_valid <- data_2017[!is.na(data_2017$Matmor), ]

# Filter for countries exposed to armed conflict
conflict_data_2017 <- data_2017_valid[data_2017_valid$conflict == 1, ]

# Number of bootstrap samples
B <- 100

# Store bootstrap medians
bootstrap_medians <- numeric(B)

# Perform bootstrap resampling
for (i in 1:B) {
  # Resample with replacement
  resample <- conflict_data_2017[sample(1:nrow(conflict_data_2017), replace = TRUE), ]
  
  # Compute the median of Matmor for the resample
  bootstrap_medians[i] <- median(resample$Matmor)
}

# Calculate the bootstrap standard error
bootstrap_se <- sd(bootstrap_medians)

# Print the result
cat("Bootstrap standard error for the median:", bootstrap_se, "\n")


set.seed(123)  # For reproducibility

# Filter the dataset for the year 2017
data_2017 <- analyticaldata[analyticaldata$year == 2017, ]

# Exclude countries with missing Matmor data
data_2017_valid <- data_2017[!is.na(data_2017$Matmor), ]

# Filter for countries exposed to armed conflict
conflict_data_2017 <- data_2017_valid[data_2017_valid$conflict == 1, ]

# Number of bootstrap samples
B <- 100

# Initialize vectors to store results
bootstrap_countries <- numeric(B)
bootstrap_observations <- numeric(B)

# Perform bootstrap resampling
for (i in 1:B) {
  # Resample with replacement
  resample <- conflict_data_2017[sample(1:nrow(conflict_data_2017), replace = TRUE), ]
  
  # Number of unique countries sampled
  bootstrap_countries[i] <- length(unique(resample$country))  # Assuming 'country' is the column name
  
  # Number of observations sampled
  bootstrap_observations[i] <- nrow(resample)
}

# Print results
cat("Number of countries sampled in each bootstrap sample (average):", mean(bootstrap_countries), "\n")
cat("Number of observations sampled in each bootstrap sample (average):", mean(bootstrap_observations), "\n")

##Question 2
# Filter the dataset for the year 2017
data_2017 <- analyticaldata[analyticaldata$year == 2017, ]

# Exclude countries with missing Matmor data
data_2017_valid <- data_2017[!is.na(data_2017$Matmor), ]

# Count countries exposed to armed conflict (conflict == 1)
countries_exposed_to_conflict <- length(unique(data_2017_valid[data_2017_valid$conflict == 1, ]$country))

# Count countries not exposed to armed conflict (conflict == 0)
countries_not_exposed_to_conflict <- length(unique(data_2017_valid[data_2017_valid$conflict == 0, ]$country))

# Print the results
cat("In 2017, there were", countries_exposed_to_conflict, "countries exposed to armed conflict, and", 
    countries_not_exposed_to_conflict, "countries not exposed to armed conflict.\n")

##Question 3
# Filter the dataset for the year 2017
data_2017 <- analyticaldata[analyticaldata$year == 2017, ]

# Exclude countries with missing Matmor data
data_2017_valid <- data_2017[!is.na(data_2017$Matmor), ]

# Filter for countries exposed to armed conflict (conflict == 1)
conflict_data_2017 <- data_2017_valid[data_2017_valid$conflict == 1, ]

# Calculate the median of the Matmor variable for countries exposed to armed conflict
sample_median <- median(conflict_data_2017$Matmor)

# Print the result
cat("The sample median of maternal mortality rate in 2017 for countries exposed to armed conflict is:", sample_median, "\n")

##Question 4
set.seed(123)  # For reproducibility

# Filter the dataset for the year 2017
data_2017 <- analyticaldata[analyticaldata$year == 2017, ]

# Exclude countries with missing Matmor data
data_2017_valid <- data_2017[!is.na(data_2017$Matmor), ]

# Filter for countries exposed to armed conflict (conflict == 1)
conflict_data_2017 <- data_2017_valid[data_2017_valid$conflict == 1, ]

# Number of countries exposed to armed conflict
num_countries_exposed_to_conflict <- length(unique(conflict_data_2017$country))

# Number of observations (rows) in the conflict data
num_observations_exposed_to_conflict <- nrow(conflict_data_2017)

# Print the results to fill in the blanks
cat("To compute the bootstrap standard error for the median 2017 maternal mortality rate in the countries exposed to armed conflict, we sample",
    num_observations_exposed_to_conflict, "observations with replacement from the", 
    num_countries_exposed_to_conflict, "countries.\n")
