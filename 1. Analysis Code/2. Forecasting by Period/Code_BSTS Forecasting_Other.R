### Code : BSTS Forecasting - Other
### Writer : Donghyeon Kim
### Update : 2023.11.16.

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
### Calculate accuracy by forecast horizon ###

## 'OTHER' index
idx <- c()
for(a in 1:3003) {
  if(M3[[a]]$period == 'OTHER') {
    idx <- c(idx, a)
  }
}
idx # 2830 ~ 3003

## Accuracy : h = 1
mase_o1 <- mape_o1 <- smape_o1 <- smae_o1 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 1
f_o1 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o1[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o1[j, seq(h)]))
    mape_o1[j, i] <- mean(e / ty) * 100
    smape_o1[j, i] <- mean(e / (abs(ty) + abs(f_o1[j, seq(h)]))) * 200
    mase_o1[j, i] <- mean(e / scale)
    smae_o1[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 2
mase_o2 <- mape_o2 <- smape_o2 <- smae_o2 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 2
f_o2 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o2[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o2[j, seq(h)]))
    mape_o2[j, i] <- mean(e / ty) * 100
    smape_o2[j, i] <- mean(e / (abs(ty) + abs(f_o2[j, seq(h)]))) * 200
    mase_o2[j, i] <- mean(e / scale)
    smae_o2[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 3
mase_o3 <- mape_o3 <- smape_o3 <- smae_o3 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 3
f_o3 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o3[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o3[j, seq(h)]))
    mape_o3[j, i] <- mean(e / ty) * 100
    smape_o3[j, i] <- mean(e / (abs(ty) + abs(f_o3[j, seq(h)]))) * 200
    mase_o3[j, i] <- mean(e / scale)
    smae_o3[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 4
mase_o4 <- mape_o4 <- smape_o4 <- smae_o4 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 4
f_o4 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o4[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o4[j, seq(h)]))
    mape_o4[j, i] <- mean(e / ty) * 100
    smape_o4[j, i] <- mean(e / (abs(ty) + abs(f_o4[j, seq(h)]))) * 200
    mase_o4[j, i] <- mean(e / scale)
    smae_o4[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 5
mase_o5 <- mape_o5 <- smape_o5 <- smae_o5 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 5
f_o5 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o5[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o5[j, seq(h)]))
    mape_o5[j, i] <- mean(e / ty) * 100
    smape_o5[j, i] <- mean(e / (abs(ty) + abs(f_o5[j, seq(h)]))) * 200
    mase_o5[j, i] <- mean(e / scale)
    smae_o5[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 6
mase_o6 <- mape_o6 <- smape_o6 <- smae_o6 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 6
f_o6 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o6[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o6[j, seq(h)]))
    mape_o6[j, i] <- mean(e / ty) * 100
    smape_o6[j, i] <- mean(e / (abs(ty) + abs(f_o6[j, seq(h)]))) * 200
    mase_o6[j, i] <- mean(e / scale)
    smae_o6[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 7
mase_o7 <- mape_o7 <- smape_o7 <- smae_o7 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 7
f_o7 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o7[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o7[j, seq(h)]))
    mape_o7[j, i] <- mean(e / ty) * 100
    smape_o7[j, i] <- mean(e / (abs(ty) + abs(f_o7[j, seq(h)]))) * 200
    mase_o7[j, i] <- mean(e / scale)
    smae_o7[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 8
mase_o8 <- mape_o8 <- smape_o8 <- smae_o8 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 8
f_o8 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_o8[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_o8[j, seq(h)]))
    mape_o8[j, i] <- mean(e / ty) * 100
    smape_o8[j, i] <- mean(e / (abs(ty) + abs(f_o8[j, seq(h)]))) * 200
    mase_o8[j, i] <- mean(e / scale)
    smae_o8[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

################################
### Check accuracy ###

## Function
result_table <- function(x1, x2, x3, x4, categ) {
  
  # Table
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(x1, na.rm=TRUE) # MAPE
  m3table[, 2] <- rowMeans(x2, na.rm=TRUE) # sMAPE
  m3table[, 3] <- rowMeans(x3, na.rm=TRUE) # MASE
  m3table[, 4] <- rowMeans(x4, na.rm=TRUE) # sMAE
  
  # Table names
  rownames(m3table) <- c("ETS", "ARIMA", "BSTS", "Hybrid")
  colnames(m3table) <- c("MAPE", "sMAPE", "MASE", "sMAE")
  
  # Add rank
  rank_m3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_m3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  # Result
  result <- list()
  result$table <- cbind(m3table, rank_m3table)
  result$count <- length(idx)
  result$name <- categ
  
  return(result)
  
}

################################
### Result ###

## Category
cate_y <- c("OTHER & h = 1", "OTHER & h = 2", "OTHER & h = 3", "OTHER & h = 4", "OTHER & h = 5", "OTHER & h = 6", "OTHER & h = 7", "OTHER & h = 8")

## OTHER & h = 1
result_table(mape_o1, smape_o1, mase_o1, smae_o1, cate_y[1])

## OTHER & h = 2
result_table(mape_o2, smape_o2, mase_o2, smae_o2, cate_y[2])

## OTHER & h = 3
result_table(mape_o3, smape_o3, mase_o3, smae_o3, cate_y[3])

## OTHER & h = 4
result_table(mape_o4, smape_o4, mase_o4, smae_o4, cate_y[4])

## OTHER & h = 5
result_table(mape_o5, smape_o5, mase_o5, smae_o5, cate_y[5])

## OTHER & h = 6
result_table(mape_o6, smape_o6, mase_o6, smae_o6, cate_y[6])

## OTHER & h = 7
result_table(mape_o7, smape_o7, mase_o7, smae_o7, cate_y[7])

## OTHER & h = 8
result_table(mape_o8, smape_o8, mase_o8, smae_o8, cate_y[8])
