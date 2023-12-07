### Code : BSTS Forecasting - Quarterly
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

## 'QUARTERLY' index
idx <- c()
for(a in 1:3003) {
  if(M3[[a]]$period == 'QUARTERLY') {
    idx <- c(idx, a)
  }
}
idx # 646 ~ 1401

## Accuracy : h = 1
mase_q1 <- mape_q1 <- smape_q1 <- smae_q1 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 1
f_q1 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q1[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q1[j, seq(h)]))
    mape_q1[j, i] <- mean(e / ty) * 100
    smape_q1[j, i] <- mean(e / (abs(ty) + abs(f_q1[j, seq(h)]))) * 200
    mase_q1[j, i] <- mean(e / scale)
    smae_q1[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 2
mase_q2 <- mape_q2 <- smape_q2 <- smae_q2 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 2
f_q2 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q2[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q2[j, seq(h)]))
    mape_q2[j, i] <- mean(e / ty) * 100
    smape_q2[j, i] <- mean(e / (abs(ty) + abs(f_q2[j, seq(h)]))) * 200
    mase_q2[j, i] <- mean(e / scale)
    smae_q2[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 3
mase_q3 <- mape_q3 <- smape_q3 <- smae_q3 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 3
f_q3 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q3[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q3[j, seq(h)]))
    mape_q3[j, i] <- mean(e / ty) * 100
    smape_q3[j, i] <- mean(e / (abs(ty) + abs(f_q3[j, seq(h)]))) * 200
    mase_q3[j, i] <- mean(e / scale)
    smae_q3[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 4
mase_q4 <- mape_q4 <- smape_q4 <- smae_q4 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 4
f_q4 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q4[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q4[j, seq(h)]))
    mape_q4[j, i] <- mean(e / ty) * 100
    smape_q4[j, i] <- mean(e / (abs(ty) + abs(f_q4[j, seq(h)]))) * 200
    mase_q4[j, i] <- mean(e / scale)
    smae_q4[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 5
mase_q5 <- mape_q5 <- smape_q5 <- smae_q5 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 5
f_q5 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q5[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q5[j, seq(h)]))
    mape_q5[j, i] <- mean(e / ty) * 100
    smape_q5[j, i] <- mean(e / (abs(ty) + abs(f_q5[j, seq(h)]))) * 200
    mase_q5[j, i] <- mean(e / scale)
    smae_q5[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 6
mase_q6 <- mape_q6 <- smape_q6 <- smae_q6 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 6
f_q6 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q6[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q6[j, seq(h)]))
    mape_q6[j, i] <- mean(e / ty) * 100
    smape_q6[j, i] <- mean(e / (abs(ty) + abs(f_q6[j, seq(h)]))) * 200
    mase_q6[j, i] <- mean(e / scale)
    smae_q6[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 7
mase_q7 <- mape_q7 <- smape_q7 <- smae_q7 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 7
f_q7 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q7[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q7[j, seq(h)]))
    mape_q7[j, i] <- mean(e / ty) * 100
    smape_q7[j, i] <- mean(e / (abs(ty) + abs(f_q7[j, seq(h)]))) * 200
    mase_q7[j, i] <- mean(e / scale)
    smae_q7[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 8
mase_q8 <- mape_q8 <- smape_q8 <- smae_q8 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 8
f_q8 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_q8[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_q8[j, seq(h)]))
    mape_q8[j, i] <- mean(e / ty) * 100
    smape_q8[j, i] <- mean(e / (abs(ty) + abs(f_q8[j, seq(h)]))) * 200
    mase_q8[j, i] <- mean(e / scale)
    smae_q8[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
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
cate_y <- c("QUARTERLY & h = 1", "QUARTERLY & h = 2", "QUARTERLY & h = 3", "QUARTERLY & h = 4", "QUARTERLY & h = 5", "QUARTERLY & h = 6", "QUARTERLY & h = 7", "QUARTERLY & h = 8")

## QUARTERLY & h = 1
result_table(mape_q1, smape_q1, mase_q1, smae_q1, cate_y[1])

## QUARTERLY & h = 2
result_table(mape_q2, smape_q2, mase_q2, smae_q2, cate_y[2])

## QUARTERLY & h = 3
result_table(mape_q3, smape_q3, mase_q3, smae_q3, cate_y[3])

## QUARTERLY & h = 4
result_table(mape_q4, smape_q4, mase_q4, smae_q4, cate_y[4])

## QUARTERLY & h = 5
result_table(mape_q5, smape_q5, mase_q5, smae_q5, cate_y[5])

## QUARTERLY & h = 6
result_table(mape_q6, smape_q6, mase_q6, smae_q6, cate_y[6])

## QUARTERLY & h = 7
result_table(mape_q7, smape_q7, mase_q7, smae_q7, cate_y[7])

## QUARTERLY & h = 8
result_table(mape_q8, smape_q8, mase_q8, smae_q8, cate_y[8])
