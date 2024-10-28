library(here)
here() 
library(texreg)
analyticaldata <- read.csv(here("analytical_data", "analyticaldata.csv"), header = TRUE)
analyticaldata$pctpopdens <- analyticaldata$popdens / 100 
lmmod <- lm(Matmor ~ -1 + conflict + gdp1000 + OECD + pctpopdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO, 
            data = analyticaldata)
library(plm)
plmmod <- plm(Matmor ~ conflict + gdp1000 + OECD + pctpopdens + urban +
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO"), model = "within", data = analyticaldata)
screenreg(list(lmmod,plmmod))
lmmod2 <- lm(Matmor ~ -1 + conflict + gdp1000 + OECD + pctpopdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = analyticaldata)
plmmod2 <- plm(Matmor ~ conflict + gdp1000 + OECD + pctpopdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "year"),
              effect = "twoways",
              model = "within",
              data = analyticaldata)
screenreg(list(lmmod2,plmmod2))
preds <- as.formula(" ~ conflict + gdp1000 + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

Matmormod <- lm(update.formula(preds, Matmor ~ .), data = analyticaldata)
Under5mormod <- lm(update.formula(preds, Under5mor ~ .), data = analyticaldata)
Infantmormod <- lm(update.formula(preds, Infantmor ~ .), data = analyticaldata)
Neonatalmormod <- lm(update.formula(preds, Neonatalmor ~ .), data = analyticaldata)
#Week 8 in-class
attach(analyticaldata) # to tell R to use this data for following commands
analyticaldata$pctpopdens <- analyticaldata$popdens / 100 
loggdp <- log(gdp1000)   
preds <- as.formula(" ~ conflict + log(gdp1000) + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")
Matmormod <- plm(update.formula(preds, Matmor ~ .), index= c("ISO", "year"), effect="twoways", model="within",data = analyticaldata) # use plm 
Under5mormod <- plm(update.formula(preds, Under5mor ~ .),index= c("ISO", "year"), effect="twoways", model="within",data = analyticaldata)
Infantmormod <- plm(update.formula(preds, Infantmor ~ .),index= c("ISO", "year"), effect="twoways", model="within", data = analyticaldata)
Neonatalmormod <- plm(update.formula(preds, Neonatmor ~ .),index= c("ISO", "year"), effect="twoways", model="within", data = analyticaldata)
screenreg(list(Matmormod,Under5mormod,Infantmormod,Neonatalmormod), custom.model.names = c("Maternal mortality", "Under 5 Mortality", "Infant Mortality", "Neonatal Mortality"))
