## set simulation parameters
n <- 500 # sample size
pz <- 0.2 # probability of Z = 1
alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
alpha1 <- 0 # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
beta0 <- -3 # logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
beta1 <- 0
beta2 <- 2
## set seed
set.seed(2024)
## generate confounder Z from a binomial distribution
z <- rbinom(n, size = 1, prob = pz)
## compute probability of observing X = 1 from the inverse logit function
px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
## randomly generate binary variable X from the above probability
x <- rbinom(n, size = 1, prob = px)
## randomly generate binary variable Y from the inverse logistic function
py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
y <- rbinom(n, size = 1, prob = py)
## combine three random variables into a data frame
dat <- data.frame(lung = y, coffee = x, smoke = z)
## fit unadjusted logistic regression model
unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
unadj.coef <- summary(unadj.mod)$coef
unadj.p <- summary(unadj.mod)$coef[2,4]
unadj.rej <- (unadj.p<0.05)*1
## fit adjusted logistic regression model
adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
adj.coef <- summary(adj.mod)$coef 
adj.p <- summary(adj.mod)$coef[2,4] #p-value is in the 2nd row and the fourth column
adj.rej <- (adj.p<0.05)*1
c(unadj.rej,adj.rej) 

library(parallel)
library(dplyr)
bias_f <- function(i){
  set.seed(i + 2024)  # set seed for reproducibility
  coffee <- rnorm(n)
  lung <- beta0 + beta1 * coffee + rnorm(n)
  bias <- coef(lm(lung ~ coffee))[2] - beta1
  return(bias)
}

tictoc::tic() # Start timing

sim_out <- mclapply(
  1:1000,         # Input for bias_f
  bias_f,      # Function to repeat
  mc.cores = 5 # Number of cores to use
) %>% unlist() # Simplify result structure

tictoc::toc()
sim_out
mean(sim_out)