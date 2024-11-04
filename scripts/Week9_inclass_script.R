library(here)
here() 
library(tidyverse)
library(dplyr) 
library(texreg)
analyticaldata <- read.csv(here("analytical_data", "analyticaldata.csv"), header = TRUE)
attach(analyticaldata)
analyticaldata$pctpopdens <- analyticaldata$popdens / 100 
loggdp <- log(gdp1000)
#visualise the missing data
library(naniar)
analyticaldata |>
  arrange(year, ISO) |>
  dplyr::select(-country_name, -ISO, -region, -year) |>
  vis_miss()
#convert ISO to numeric
finaldata_ISONUM <- analyticaldata |>
  mutate(ISOnum = as.numeric(as.factor(analyticaldata$ISO))) |>
  select(-country_name, -ISO)
finaldata_ISONUM <- finaldata_ISONUM %>% mutate(loggdp = log(gdp1000))
finaldata_ISONUM <- finaldata_ISONUM %>% mutate(pctpopdens = popdens/100)
library(mice)
mice0  <- mice(finaldata_ISONUM, seed = 100, m = 5, maxit = 0, print = F)
meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "Matmor", "Infantmor", "Neonatmor", "Under5mor", "loggdp", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "Matmor", "Infantmor", "Neonatmor", "Under5mor", "loggdp", "pctpopdens"), "ISOnum"] <- -2
mice.multi.out  <- mice(finaldata_ISONUM, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)
preds <- as.formula(" ~ conflict + loggdp + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")
Matmormodim <- with(mice.multi.out,lm(update.formula(preds, Matmor ~ .),data = finaldata_ISONUM)) 
Under5mormodim <- with(mice.multi.out, lm(update.formula(preds, Under5mor ~ .), data = finaldata_ISONUM))
Infantmormodim <- with(mice.multi.out, lm(update.formula(preds, Infantmor ~ .), data = finaldata_ISONUM))
Neonatalmormodim <- with(mice.multi.out, lm(update.formula(preds, Neonatmor ~ .), data = finaldata_ISONUM))
screenreg(list(Matmormodim,Under5mormodim,Infantmormodim,Neonatalmormodim), custom.model.names = c("Maternal mortality", "Under 5 Mortality", "Infant Mortality", "Neonatal Mortality"))
#use pool function to pool the intercepts for the 10 regression outputs 
