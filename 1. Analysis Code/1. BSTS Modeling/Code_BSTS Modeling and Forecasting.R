### Code : BSTS Modeling and Forecasting
### Writer : Donghyeon Kim
### Update : 2023.11.23.

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
### Modeling ###

## abind function with `along=3`
acomb <- function(...) abind(..., along=3)

## BSTS : Modeling by ETS
bsts_model <- function(y, h) {
  
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
  bsts_model <- bsts(y,
                     state.specification=model_ss,
                     niter=10000)
  
  # Burn-in Period
  bsts_burn <- SuggestBurn(0.5, bsts_model)
  
  # Forecasting
  bsts_model_fore <- predict.bsts(bsts_model,
                                  horizon=h,
                                  burn=bsts_burn,
                                  quantiles=c(0.025, 0.975))
  
  return(bsts_model_fore$mean)
}

## M3 Length = 3003
nseries <- length(M3)

## Forecasting horizon
h_set <- sapply(M3, FUN=function(MM){MM$h})
max_h <- max(h_set)

## Method Numbers
# 1) ETS
# 2) ARIMA
# 3) BSTS : Modeling by ETS
# 4) Hybrid : ETS + BSTS
n_method <- 4

## Setting for parallel working
c1 <- makeSOCKcluster(detectCores()-2)
registerDoSNOW(c1)

## For checking time
start_time <- Sys.time()

## Progress Bar
pb <- txtProgressBar(max=nseries, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

tempres <- foreach(i = 1:nseries,
                   .combine='acomb', .multicombine=TRUE,
                   .options.snow=opts,
                   .packages=c("Mcomp","forecast",
                               "foreach","bsts")) %dopar% {
                                 
   #for (i in seq(nseries)) {
   tryCatch({
     
      yy <- M3[[i]]
      
      y <- M3[[i]]$x
      h <- M3[[i]]$h
      fct <- matrix(nrow=n_method, ncol=max_h)

      # original methods
      fct[1, seq(h)] <- forecast(ets(y), h=h, PI=FALSE)$mean     # ETS
      fct[2, seq(h)] <- forecast(auto.arima(y), h=h)$mean        # ARIMA
      fct[3, seq(h)] <- bsts_model(y, h=h)                       # BSTS : Modeling by ETS
      fct[4, seq(h)] <- 0.5 * (fct[1, seq(h)] + fct[3, seq(h)])  # Hybrid : ETS + BSTS

      fct
      
   },error=function(e){cat(paste("error in", i))})
                                 # end of tryCatch

}

close(pb)
stopCluster(c1)

end_time <- Sys.time()
(end_time - start_time)

## Save RData
save(tempres, file="roll_cp_result_iter10000_v3.rda")

## Load RData
# load("roll_cp_result_iter10000_v3.rda")

################################
### Accuracy ###

mase <- mape <- smape <- smae <- matrix(NA, nrow=n_method, ncol=nseries)
f <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in seq(nseries)) {
  
  ty <- M3[[i]]$xx
  h <- length(ty)

  for(j in 1:n_method) f[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f[j, seq(h)]))
    mape[j, i] <- mean(e / ty) * 100
    smape[j, i] <- mean(e / (abs(ty) + abs(f[j, seq(h)]))) * 200
    mase[j, i] <- mean(e / scale)
    smae[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

################################
### Result ###

## 1. Accuracy by period ##
## Period Type
period_type <- sapply(M3, FUN=function(MM){MM$period})
table(period_type)
cate <- c(unique(period_type), "ALL")

result <- vector(mode="list", length=length(cate))
# m3.fores <- paste0("fore",c(1:n_method2))

for(i in 1:length(cate)){
  
  tind <- which(cate[i] == period_type)
  if(cate[i] == "ALL") tind <- seq(nseries)
  
  temp_mape <- mape[, tind]
  temp_smape <- smape[, tind]
  temp_mase <- mase[, tind]
  temp_smae <- smae[, tind]
  
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(temp_mape, na.rm=TRUE)
  m3table[, 2] <- rowMeans(temp_smape)
  m3table[, 3] <- rowMeans(temp_mase)
  m3table[, 4] <- rowMeans(temp_smae)
  
  rownames(m3table) <- c("ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  result[[i]]$table <- cbind(m3table, rank_m3table)
  result[[i]]$count <- length(tind)
  result[[i]]$name <- cate[i]
}

result

## 2. Accuracy by sample size ##
## Sample Size
n_type <- sapply(M3, FUN=function(MM){MM$n})
table(n_type)

cate <- c("0 < n <= 20",
          "20 < n <= 40",
          "40 < n <= 60",
          "60 < n <= 80",
          "80 < n <= 100",
          "100 < n <= 120",
          "120 < n <= 140",
          "ALL")

result2 <- vector(mode="list", length=length(cate))

for(i in 1:length(cate)){
  
  if(cate[i] == "ALL"){
    tind <- seq(nseries)
  } else{
    tind <- which((n_type <= 20*i) & (n_type > 20*(i-1)))
  }
  
  temp_mape <- mape[,tind]
  temp_smape <- smape[,tind]
  temp_mase <- mase[,tind]
  temp_smae <- smae[,tind]
  
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(temp_mape, na.rm=TRUE)
  m3table[, 2] <- rowMeans(temp_smape)
  m3table[, 3] <- rowMeans(temp_mase)
  m3table[, 4] <- rowMeans(temp_smae)
  
  rownames(m3table) <- c("ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  result2[[i]]$table <- cbind(m3table, rank_m3table)
  result2[[i]]$count <- length(tind)
  result2[[i]]$name <- cate[i]
}

result2

## 3. Accuracy by forecast horizon ##
## Forecast Horizon
h_type <- sapply(M3, FUN=function(MM){MM$h})
table(h_type)

cate <- c("6", "8", "18", "ALL")

result3 <- vector(mode="list", length=length(cate))

for(i in 1:length(cate)){
  
  tind <- which(cate[i] == h_type)
  if(cate[i] == "ALL") tind <- seq(nseries)
  
  temp_mape <- mape[,tind]
  temp_smape <- smape[,tind]
  temp_mase <- mase[,tind]
  temp_smae <- smae[,tind]
  
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(temp_mape, na.rm=TRUE)
  m3table[, 2] <- rowMeans(temp_smape)
  m3table[, 3] <- rowMeans(temp_mase)
  m3table[, 4] <- rowMeans(temp_smae)
  
  rownames(m3table) <- c("ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  result3[[i]]$table <- cbind(m3table, rank_m3table)
  result3[[i]]$count <- length(tind)
  result3[[i]]$name <- cate[i]
}

result3


## ------------------------------------------------------------------------------- ##
## Comparison with top 7 methods of M3 Comp

## ETS, ARIMA, BSTS, Hybrid Model : n_method = 4
n_method1 <- n_method

## The top 7 methods of M3 Comp
n_method2 <- 7

theta <- as.matrix(M3Forecast$THETA)
fpro <- as.matrix(M3Forecast$ForecastPro)
fcx <- as.matrix(M3Forecast$ForcX)
bjauto <- as.matrix(M3Forecast$`B-J auto`)
ab1 <- as.matrix(M3Forecast$AutoBox1)
ab2 <- as.matrix(M3Forecast$AutoBox2)
ab3 <- as.matrix(M3Forecast$AutoBox3)

## Comparisons
n_method <- n_method1 + n_method2

################################
### Accuracy ###

mase <- mape <- smape <- smae <- matrix(NA, nrow=n_method, ncol=nseries)
f <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in seq(nseries)) {
  
  ty <- M3[[i]]$xx
  h <- length(ty)
  
  f[1, seq(h)] <- theta[i, seq(h)]
  f[2, seq(h)] <- fpro[i, seq(h)]
  f[3, seq(h)] <- fcx[i, seq(h)]
  f[4, seq(h)] <- bjauto[i, seq(h)]
  f[5, seq(h)] <- ab1[i, seq(h)]
  f[6, seq(h)] <- ab2[i, seq(h)]
  f[7, seq(h)] <- ab3[i, seq(h)]
  
  for(j in 1:n_method1) f[n_method2+j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f[j, seq(h)]))
    mape[j, i] <- mean(e / ty) * 100
    smape[j, i] <- mean(e / (abs(ty) + abs(f[j, seq(h)]))) * 200
    mase[j, i] <- mean(e / scale)
    smae[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

################################
### Result ###

## 1. Accuracy by period ##
## Period Type
period_type <- sapply(M3, FUN=function(MM){MM$period})
table(period_type)
cate <- c(unique(period_type), "ALL")

result <- vector(mode="list", length=length(cate))
# m3.fores <- paste0("fore",c(1:n_method2))

for(i in 1:length(cate)){
  
  tind <- which(cate[i] == period_type)
  if(cate[i] == "ALL") tind <- seq(nseries)
  
  temp_mape <- mape[, tind]
  temp_smape <- smape[, tind]
  temp_mase <- mase[, tind]
  temp_smae <- smae[, tind]
  
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(temp_mape, na.rm=TRUE)
  m3table[, 2] <- rowMeans(temp_smape)
  m3table[, 3] <- rowMeans(temp_mase)
  m3table[, 4] <- rowMeans(temp_smae)
  
  rownames(m3table) <- c("Theta", "ForecastPro", "ForecastX", "BJauto",
                         "Autobox1", "Autobox2", "Autobox3",
                         "ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  result[[i]]$table <- cbind(m3table, rank_m3table)
  result[[i]]$count <- length(tind)
  result[[i]]$name <- cate[i]
}

result

## 2. Accuracy by sample size ##
## Sample Size
n_type <- sapply(M3, FUN=function(MM){MM$n})
table(n_type)

cate <- c("0 < n <= 20",
          "20 < n <= 40",
          "40 < n <= 60",
          "60 < n <= 80",
          "80 < n <= 100",
          "100 < n <= 120",
          "120 < n <= 140",
          "ALL")

result2 <- vector(mode="list", length=length(cate))

for(i in 1:length(cate)){
  
  if(cate[i] == "ALL"){
    tind <- seq(nseries)
  } else{
    tind <- which((n_type <= 20*i) & (n_type > 20*(i-1)))
  }
  
  temp_mape <- mape[,tind]
  temp_smape <- smape[,tind]
  temp_mase <- mase[,tind]
  temp_smae <- smae[,tind]
  
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(temp_mape, na.rm=TRUE)
  m3table[, 2] <- rowMeans(temp_smape)
  m3table[, 3] <- rowMeans(temp_mase)
  m3table[, 4] <- rowMeans(temp_smae)
  
  rownames(m3table) <- c("Theta", "ForecastPro", "ForecastX", "BJauto",
                         "Autobox1", "Autobox2", "Autobox3",
                         "ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  result2[[i]]$table <- cbind(m3table, rank_m3table)
  result2[[i]]$count <- length(tind)
  result2[[i]]$name <- cate[i]
}

result2

## 3. Accuracy by forecast horizon ##
## Forecast Horizon
h_type <- sapply(M3, FUN=function(MM){MM$h})
table(h_type)

cate <- c("6", "8", "18", "ALL")

result3 <- vector(mode="list", length=length(cate))

for(i in 1:length(cate)){
  
  tind <- which(cate[i] == h_type)
  if(cate[i] == "ALL") tind <- seq(nseries)
  
  temp_mape <- mape[,tind]
  temp_smape <- smape[,tind]
  temp_mase <- mase[,tind]
  temp_smae <- smae[,tind]
  
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(temp_mape, na.rm=TRUE)
  m3table[, 2] <- rowMeans(temp_smape)
  m3table[, 3] <- rowMeans(temp_mase)
  m3table[, 4] <- rowMeans(temp_smae)
  
  rownames(m3table) <- c("Theta", "ForecastPro", "ForecastX", "BJauto",
                         "Autobox1", "Autobox2", "Autobox3",
                         "ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  result3[[i]]$table <- cbind(m3table, rank_m3table)
  result3[[i]]$count <- length(tind)
  result3[[i]]$name <- cate[i]
}

result3
