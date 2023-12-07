### Code : BSTS Forecasting - Yearly
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

## 'YEARLY' index
idx <- c()
for(a in 1:3003) {
  if(M3[[a]]$period == 'YEARLY') {
    idx <- c(idx, a)
  }
}
idx # 1 ~ 645

## Accuracy : h = 1
mase_y1 <- mape_y1 <- smape_y1 <- smae_y1 <- matrix(NA, nrow=n_method, ncol=length(idx))
max_h = 1
f_y1 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_y1[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_y1[j, seq(h)]))
    mape_y1[j, i] <- mean(e / ty) * 100
    smape_y1[j, i] <- mean(e / (abs(ty) + abs(f_y1[j, seq(h)]))) * 200
    mase_y1[j, i] <- mean(e / scale)
    smae_y1[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 2
mase_y2 <- mape_y2 <- smape_y2 <- smae_y2 <- matrix(NA, nrow=n_method, ncol=length(idx))
max_h = 2
f_y2 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_y2[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_y2[j, seq(h)]))
    mape_y2[j, i] <- mean(e / ty) * 100
    smape_y2[j, i] <- mean(e / (abs(ty) + abs(f_y2[j, seq(h)]))) * 200
    mase_y2[j, i] <- mean(e / scale)
    smae_y2[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 3
mase_y3 <- mape_y3 <- smape_y3 <- smae_y3 <- matrix(NA, nrow=n_method, ncol=length(idx))
max_h = 3
f_y3 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_y3[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_y3[j, seq(h)]))
    mape_y3[j, i] <- mean(e / ty) * 100
    smape_y3[j, i] <- mean(e / (abs(ty) + abs(f_y3[j, seq(h)]))) * 200
    mase_y3[j, i] <- mean(e / scale)
    smae_y3[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 4
mase_y4 <- mape_y4 <- smape_y4 <- smae_y4 <- matrix(NA, nrow=n_method, ncol=length(idx))
max_h = 4
f_y4 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_y4[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_y4[j, seq(h)]))
    mape_y4[j, i] <- mean(e / ty) * 100
    smape_y4[j, i] <- mean(e / (abs(ty) + abs(f_y4[j, seq(h)]))) * 200
    mase_y4[j, i] <- mean(e / scale)
    smae_y4[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 5
mase_y5 <- mape_y5 <- smape_y5 <- smae_y5 <- matrix(NA, nrow=n_method, ncol=length(idx))
max_h = 5
f_y5 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_y5[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_y5[j, seq(h)]))
    mape_y5[j, i] <- mean(e / ty) * 100
    smape_y5[j, i] <- mean(e / (abs(ty) + abs(f_y5[j, seq(h)]))) * 200
    mase_y5[j, i] <- mean(e / scale)
    smae_y5[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 6
mase_y6 <- mape_y6 <- smape_y6 <- smae_y6 <- matrix(NA, nrow=n_method, ncol=length(idx))
max_h = 6
f_y6 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_y6[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_y6[j, seq(h)]))
    mape_y6[j, i] <- mean(e / ty) * 100
    smape_y6[j, i] <- mean(e / (abs(ty) + abs(f_y6[j, seq(h)]))) * 200
    mase_y6[j, i] <- mean(e / scale)
    smae_y6[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

################################
### Check accuracy ###

## Function
result_table <- function(x1, x2, x3, x4, categ) {
  
  # Table
  m3table <- matrix(NA, nrow=n_method, ncol=4)
  m3table[, 1] <- rowMeans(x1) # MAPE
  m3table[, 2] <- rowMeans(x2) # sMAPE
  m3table[, 3] <- rowMeans(x3) # MASE
  m3table[, 4] <- rowMeans(x4) # sMAE
  
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
cate_y <- c("YEARLY & h = 1", "YEARLY & h = 2", "YEARLY & h = 3", "YEARLY & h = 4", "YEARLY & h = 5", "YEARLY & h = 6")

## YEARLY & h = 1
result_table(mape_y1, smape_y1, mase_y1, smae_y1, cate_y[1])

## YEARLY & h = 2
result_table(mape_y2, smape_y2, mase_y2, smae_y2, cate_y[2])

## YEARLY & h = 3
result_table(mape_y3, smape_y3, mase_y3, smae_y3, cate_y[3])

## YEARLY & h = 4
result_table(mape_y4, smape_y4, mase_y4, smae_y4, cate_y[4])

## YEARLY & h = 5
result_table(mape_y5, smape_y5, mase_y5, smae_y5, cate_y[5])

## YEARLY & h = 6
result_table(mape_y6, smape_y6, mase_y6, smae_y6, cate_y[6])
