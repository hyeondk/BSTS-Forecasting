### Code : BSTS and ETS Comparison
### Writer : Donghyeon Kim
### Update : 2023.11.21.

################################
### Package ###

library(tidyverse) # Data Preprocessing
library(Mcomp) # M3 Dataset
library(forecast) # TS Forecasting
library(bsts) # Bayesian Structural Time Series
library(fable) # Forecasting for Tidy Time Series
library(parallel) # Parallel Work
library(foreach) # Parallel Work
library(doSNOW) # Foreach Parallel Adaptor
library(abind) # Combine Multi-dimensional Arrays

################################
### Data ###

# load("roll_cp_result_iter10000_v3.rda")

################################
### BSTS Function ###

## BSTS : Modeling by ETS
bsts_model <- function(y) {
  
  # ETS #
  ets_model <- ets(y, model='AZZ')
  if(ets_model$components[4] == 'FALSE') {
    ets_model_call <- paste0(ets_model$components[1], ',', ets_model$components[2], ',', ets_model$components[3])
  } else {
    ets_model_call <- paste0(ets_model$components[1], ',', 'Ad', ',', ets_model$components[3])
  }
  
  # BSTS (by ETS) #
  # State Specification 1 : Form
  if(ets_model_call %in% c('A,N,A', 'A,N,N')) {
    model_ss <- AddLocalLevel(list(), y)
  } else if(ets_model_call %in% c('A,A,A', 'A,A,N')) {
    model_ss <- AddLocalLinearTrend(list(), y)
  } else if(ets_model_call %in% c('A,Ad,A', 'A,Ad,N')) {
    model_ss <- AddSemilocalLinearTrend(list(), y)
  }
  
  # State Specification 2 : Frequency
  if(frequency(y) > 1) {
    model_ss <- AddSeasonal(model_ss, y, nseasons=frequency(y))
  }
  
  # Modeling
  bsts_model_spec <- bsts(y,
                          state.specification=model_ss,
                          niter=10000)
  
  return(bsts_model_spec)
}

################################
### 1. Monthly ###
# Total Index : 1402 ~ 2829
# Chosen Index : 2051

## Historical Data
y <- M3[[2051]]$x
y %>% ts.plot(., main='M650(Industry) Monthly Plot', ylab='Value')

## Future Data
yf <- M3[[2051]]$xx

############
## ETS
ets_m <- ets(y, model='AZZ')

# Summary
# ETS(A,N,A)
ets_m %>% summary()

# Plot
ets_m %>% autoplot()

# Components
ets_m$sigma2 %>% sqrt()

# Final State
ets_m$states[dim(ets_m$states)[1], ]

############
## BSTS Modeling by ETS : Local Level Model(LL Model)
bsts_m <- bsts_model(y)

# Summary
summary(bsts_m, burn=5000)

# Components
bsts_m$sigma.obs[5001:10000] %>% mean()
bsts_m$sigma.level[5001:10000] %>% mean()
bsts_m$sigma.seasonal.12[5001:10000] %>% mean()

# Final State
bsts_m$final.state[5001:10000, ] %>% colMeans()

# State Contributions
plot(bsts_m, 'comp')


################################
### 2. Quarterly ###
# Total Index : 646 ~ 1401
# Chosen Index : 1154

## Historical Data
y <- M3[[1154]]$x
y %>% ts.plot(., main='Q509(Macro) Quarterly Plot', ylab='Value')

## Future Data
yf <- M3[[1154]]$xx

############
## ETS
ets_q <- ets(y, model='AZZ')

# Summary
# ETS(A,A,A)
ets_q %>% summary()

# Plot
ets_q %>% autoplot()

# Components
ets_q$sigma2 %>% sqrt()

# Final State
ets_q$states[dim(ets_q$states)[1], ]

############
## BSTS Modeling by ETS : Local Linear Trend Model(LLT Model)
bsts_q <- bsts_model(y)

# Summary
summary(bsts_q, burn=5000)

# Components
bsts_q$sigma.obs[5001:10000] %>% mean()
bsts_q$sigma.trend.level[5001:10000] %>% mean()
bsts_q$sigma.trend.slope[5001:10000] %>% mean()
bsts_q$sigma.seasonal.4[5001:10000] %>% mean()

# Final State
bsts_q$final.state[5001:10000, ] %>% colMeans()

# State Contributions
plot(bsts_q, 'comp')
