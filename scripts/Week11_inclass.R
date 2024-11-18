library(here)
here() 
library(dplyr) 
analyticaldata <- read.csv(here("analytical_data", "analyticaldata.csv"), header = TRUE)
infdata2017 <- analyticaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(Infantmor)) 

infdata2017 |>
  group_by(conflict) |>
  summarise(n = n(),
            median.Infantmor = median(Infantmor, na.rm = T))
obs.infmormed.diff <- median(infdata2017[infdata2017$conflict == 1,]$Infantmor) -
  median(infdata2017[infdata2017$conflict == 0,]$Infantmor)
obs.infmormed.diff
#Using a stratified bootstrap
Infantmor.conflict1 <- analyticaldata |>
  dplyr::filter(year == 2017 & !is.na(Infantmor) & conflict == 1) |>
  dplyr::select(ISO, Infantmor)
Infantmor.conflict0 <- analyticaldata |>
  dplyr::filter(year == 2017 & !is.na(Infantmor) & conflict == 0) |>
  dplyr::select(ISO, Infantmor)

set.seed(2024)
B <- 1000
infmed.diff <- rep(NA, B)
for(b in 1:B){
  resamp.conflict1 <- Infantmor.conflict1[sample(nrow(Infantmor.conflict1), size = nrow(Infantmor.conflict1), replace = TRUE),]
  resamp.conflict0 <- Infantmor.conflict0[sample(nrow(Infantmor.conflict0), size = nrow(Infantmor.conflict0), replace = TRUE),]
  infmed.diff[b] <- median(resamp.conflict1$Infantmor) - median(resamp.conflict0$Infantmor)
}
head(resamp.conflict1, 12)
#Histogram of the bootstrap statistic
hist(infmed.diff, main = "Distribution of bootstrap statistic for Infant Mortality")
#bootstrap function
library(boot)

getinfmeddiff <- function(analyticaldata, indices) {
  sample_infdata <- analyticaldata[indices, ]
  group_infmeds <- tapply(sample_infdata$Infantmor, sample_infdata$conflict, FUN = function(x) median(x,na.rm=TRUE))
  infmeddiff <- group_infmeds[2] - group_infmeds[1]
  return(infmeddiff)
}

infbootout <- boot(infdata2017, statistic = getinfmeddiff, strata = infdata2017$conflict, R = 1000)
infbootout
#Bootstrap CI
infantbootCI <- boot.ci(boot.out = infbootout, conf = 0.95, type = c("basic", "perc", "bca"))

##Under-5 mortality
un5data2017 <- analyticaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(Under5mor)) 

un5data2017 |>
  group_by(conflict) |>
  summarise(n = n(),
            median.Under5mor = median(Under5mor, na.rm = T))
obs.un5mormed.diff <- median(un5data2017[un5data2017$conflict == 1,]$Under5mor) -
  median(un5data2017[un5data2017$conflict == 0,]$Under5mor)
obs.un5mormed.diff
#setting the var for a stratified bootstrap
un5mor.conflict1 <- analyticaldata |>
  dplyr::filter(year == 2017 & !is.na(Under5mor) & conflict == 1) |>
  dplyr::select(ISO, Under5mor)
un5mor.conflict0 <- analyticaldata |>
  dplyr::filter(year == 2017 & !is.na(Under5mor) & conflict == 0) |>
  dplyr::select(ISO, Under5mor)
#using stratified bootstrap
set.seed(2024)
B <- 1000
un5med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.conflict1 <- un5mor.conflict1[sample(nrow(un5mor.conflict1), size = nrow(un5mor.conflict1), replace = TRUE),]
  resamp.conflict0 <- un5mor.conflict0[sample(nrow(un5mor.conflict0), size = nrow(un5mor.conflict0), replace = TRUE),]
  un5med.diff[b] <- median(resamp.conflict1$Under5mor) - median(resamp.conflict0$Under5mor)
}
#bootstrap function
library(boot)

getun5meddiff <- function(analyticaldata, indices) {
  sample_un5data <- analyticaldata[indices, ]
  group_un5meds <- tapply(sample_un5data$Under5mor, sample_un5data$conflict, FUN = function(x) median(x,na.rm=TRUE))
  un5meddiff <- group_un5meds[2] - group_un5meds[1]
  return(un5meddiff)
}

un5bootout <- boot(un5data2017, statistic = getun5meddiff, strata = un5data2017$conflict, R = 1000)
un5bootout
#Bootstrap CI
under5bootCI <- boot.ci(boot.out = un5bootout, conf = 0.95, type = c("basic", "perc", "bca"))

##Neonatal Mortality 
neonatdata2017 <- analyticaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(Neonatmor)) 
neonatdata2017 |>
  group_by(conflict) |>
  summarise(n = n(),
            median.Neonatmor = median(Neonatmor, na.rm = T))
obs.neonatmormed.diff <- median(neonatdata2017[neonatdata2017$conflict == 1,]$Neonatmor) -
  median(neonatdata2017[neonatdata2017$conflict == 0,]$Neonatmor)
obs.neonatmormed.diff
#setting the var for a stratified bootstrap
neonatmor.conflict1 <- analyticaldata |>
  dplyr::filter(year == 2017 & !is.na(Neonatmor) & conflict == 1) |>
  dplyr::select(ISO, Neonatmor)
neonatmor.conflict0 <- analyticaldata |>
  dplyr::filter(year == 2017 & !is.na(Neonatmor) & conflict == 0) |>
  dplyr::select(ISO, Neonatmor)
#using stratified bootstrap
set.seed(2024)
B <- 1000
neonatmed.diff <- rep(NA, B)
for(b in 1:B){
  resamp.conflict1 <- neonatmor.conflict1[sample(nrow(neonatmor.conflict1), size = nrow(neonatmor.conflict1), replace = TRUE),]
  resamp.conflict0 <- neonatmor.conflict0[sample(nrow(neonatmor.conflict0), size = nrow(neonatmor.conflict0), replace = TRUE),]
  neonatmed.diff[b] <- median(resamp.conflict1$Neonatmor) - median(resamp.conflict0$Neonatmor)
}

#bootstrap function
library(boot)

getneonatmeddiff <- function(analyticaldata, indices) {
  sample_neonatdata <- analyticaldata[indices, ]
  group_neonatmeds <- tapply(sample_neonatdata$Neonatmor, sample_neonatdata$conflict, FUN = function(x) median(x,na.rm=TRUE))
  neonatmeddiff <- group_neonatmeds[2] - group_neonatmeds[1]
  return(neonatmeddiff)
}

neonatbootout <- boot(neonatdata2017, statistic = getneonatmeddiff, strata = neonatdata2017$conflict, R = 1000)
neonatbootout
#Bootstrap CI
NeonatbootCI <- boot.ci(boot.out = neonatbootout, conf = 0.95, type = c("basic", "perc", "bca"))
#The three CI for differences in medians of the three mortality variables
infantbootCI
under5bootCI
neonatbootout
#Interpretation: If we were to repeat the bootstrap process many times, 
#95% of the (,) intervals would contain the true difference in medians of "mortality variable" 
#for armed conflict present vs absent.