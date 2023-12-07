### Code : BSTS Forecasting - All
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

## 'ALL' index
idx <- 1:3003

## Accuracy : h = 1
mase_a1 <- mape_a1 <- smape_a1 <- smae_a1 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 1
f_a1 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a1[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a1[j, seq(h)]))
    mape_a1[j, i] <- mean(e / ty) * 100
    smape_a1[j, i] <- mean(e / (abs(ty) + abs(f_a1[j, seq(h)]))) * 200
    mase_a1[j, i] <- mean(e / scale)
    smae_a1[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 2
mase_a2 <- mape_a2 <- smape_a2 <- smae_a2 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 2
f_a2 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a2[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a2[j, seq(h)]))
    mape_a2[j, i] <- mean(e / ty) * 100
    smape_a2[j, i] <- mean(e / (abs(ty) + abs(f_a2[j, seq(h)]))) * 200
    mase_a2[j, i] <- mean(e / scale)
    smae_a2[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 3
mase_a3 <- mape_a3 <- smape_a3 <- smae_a3 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 3
f_a3 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a3[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a3[j, seq(h)]))
    mape_a3[j, i] <- mean(e / ty) * 100
    smape_a3[j, i] <- mean(e / (abs(ty) + abs(f_a3[j, seq(h)]))) * 200
    mase_a3[j, i] <- mean(e / scale)
    smae_a3[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 4
mase_a4 <- mape_a4 <- smape_a4 <- smae_a4 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 4
f_a4 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a4[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a4[j, seq(h)]))
    mape_a4[j, i] <- mean(e / ty) * 100
    smape_a4[j, i] <- mean(e / (abs(ty) + abs(f_a4[j, seq(h)]))) * 200
    mase_a4[j, i] <- mean(e / scale)
    smae_a4[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 5
mase_a5 <- mape_a5 <- smape_a5 <- smae_a5 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 5
f_a5 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a5[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a5[j, seq(h)]))
    mape_a5[j, i] <- mean(e / ty) * 100
    smape_a5[j, i] <- mean(e / (abs(ty) + abs(f_a5[j, seq(h)]))) * 200
    mase_a5[j, i] <- mean(e / scale)
    smae_a5[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 6
mase_a6 <- mape_a6 <- smape_a6 <- smae_a6 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 6
f_a6 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a6[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a6[j, seq(h)]))
    mape_a6[j, i] <- mean(e / ty) * 100
    smape_a6[j, i] <- mean(e / (abs(ty) + abs(f_a6[j, seq(h)]))) * 200
    mase_a6[j, i] <- mean(e / scale)
    smae_a6[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 7
mase_a7 <- mape_a7 <- smape_a7 <- smae_a7 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 7
f_a7 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a7[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a7[j, seq(h)]))
    mape_a7[j, i] <- mean(e / ty) * 100
    smape_a7[j, i] <- mean(e / (abs(ty) + abs(f_a7[j, seq(h)]))) * 200
    mase_a7[j, i] <- mean(e / scale)
    smae_a7[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 8
mase_a8 <- mape_a8 <- smape_a8 <- smae_a8 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 8
f_a8 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a8[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a8[j, seq(h)]))
    mape_a8[j, i] <- mean(e / ty) * 100
    smape_a8[j, i] <- mean(e / (abs(ty) + abs(f_a8[j, seq(h)]))) * 200
    mase_a8[j, i] <- mean(e / scale)
    smae_a8[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 9
mase_a9 <- mape_a9 <- smape_a9 <- smae_a9 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 9
f_a9 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a9[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a9[j, seq(h)]))
    mape_a9[j, i] <- mean(e / ty) * 100
    smape_a9[j, i] <- mean(e / (abs(ty) + abs(f_a9[j, seq(h)]))) * 200
    mase_a9[j, i] <- mean(e / scale)
    smae_a9[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 10
mase_a10 <- mape_a10 <- smape_a10 <- smae_a10 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 10
f_a10 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a10[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a10[j, seq(h)]))
    mape_a10[j, i] <- mean(e / ty) * 100
    smape_a10[j, i] <- mean(e / (abs(ty) + abs(f_a10[j, seq(h)]))) * 200
    mase_a10[j, i] <- mean(e / scale)
    smae_a10[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 11
mase_a11 <- mape_a11 <- smape_a11 <- smae_a11 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 11
f_a11 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a11[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a11[j, seq(h)]))
    mape_a11[j, i] <- mean(e / ty) * 100
    smape_a11[j, i] <- mean(e / (abs(ty) + abs(f_a11[j, seq(h)]))) * 200
    mase_a11[j, i] <- mean(e / scale)
    smae_a11[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 12
mase_a12 <- mape_a12 <- smape_a12 <- smae_a12 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 12
f_a12 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a12[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a12[j, seq(h)]))
    mape_a12[j, i] <- mean(e / ty) * 100
    smape_a12[j, i] <- mean(e / (abs(ty) + abs(f_a12[j, seq(h)]))) * 200
    mase_a12[j, i] <- mean(e / scale)
    smae_a12[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 13
mase_a13 <- mape_a13 <- smape_a13 <- smae_a13 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 13
f_a13 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a13[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a13[j, seq(h)]))
    mape_a13[j, i] <- mean(e / ty) * 100
    smape_a13[j, i] <- mean(e / (abs(ty) + abs(f_a13[j, seq(h)]))) * 200
    mase_a13[j, i] <- mean(e / scale)
    smae_a13[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 14
mase_a14 <- mape_a14 <- smape_a14 <- smae_a14 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 14
f_a14 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a14[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a14[j, seq(h)]))
    mape_a14[j, i] <- mean(e / ty) * 100
    smape_a14[j, i] <- mean(e / (abs(ty) + abs(f_a14[j, seq(h)]))) * 200
    mase_a14[j, i] <- mean(e / scale)
    smae_a14[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 15
mase_a15 <- mape_a15 <- smape_a15 <- smae_a15 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 15
f_a15 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a15[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a15[j, seq(h)]))
    mape_a15[j, i] <- mean(e / ty) * 100
    smape_a15[j, i] <- mean(e / (abs(ty) + abs(f_a15[j, seq(h)]))) * 200
    mase_a15[j, i] <- mean(e / scale)
    smae_a15[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 16
mase_a16 <- mape_a16 <- smape_a16 <- smae_a16 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 16
f_a16 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a16[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a16[j, seq(h)]))
    mape_a16[j, i] <- mean(e / ty) * 100
    smape_a16[j, i] <- mean(e / (abs(ty) + abs(f_a16[j, seq(h)]))) * 200
    mase_a16[j, i] <- mean(e / scale)
    smae_a16[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 17
mase_a17 <- mape_a17 <- smape_a17 <- smae_a17 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 17
f_a17 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a17[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a17[j, seq(h)]))
    mape_a17[j, i] <- mean(e / ty) * 100
    smape_a17[j, i] <- mean(e / (abs(ty) + abs(f_a17[j, seq(h)]))) * 200
    mase_a17[j, i] <- mean(e / scale)
    smae_a17[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 18
mase_a18 <- mape_a18 <- smape_a18 <- smae_a18 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 18
f_a18 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_a18[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_a18[j, seq(h)]))
    mape_a18[j, i] <- mean(e / ty) * 100
    smape_a18[j, i] <- mean(e / (abs(ty) + abs(f_a18[j, seq(h)]))) * 200
    mase_a18[j, i] <- mean(e / scale)
    smae_a18[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
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
  rank_a3table <- apply(m3table, 2, FUN=rank)
  colnames(rank_a3table) <- c("Rank_MAPE", "Rank_sMAPE", "Rank_MASE", "Rank_sMAE")
  
  # Result
  result <- list()
  result$table <- cbind(m3table, rank_a3table)
  result$count <- length(idx)
  result$name <- categ
  
  return(result)
  
}

################################
### Result ###

## Category
cate_y <- c("ALL & h = 1", "ALL & h = 2", "ALL & h = 3", "ALL & h = 4", "ALL & h = 5", "ALL & h = 6",
            "ALL & h = 7", "ALL & h = 8", "ALL & h = 9", "ALL & h = 10", "ALL & h = 11", "ALL & h = 12",
            "ALL & h = 13", "ALL & h = 14", "ALL & h = 15", "ALL & h = 16", "ALL & h = 17", "ALL & h = 18")

## ALL & h = 1
result_table(mape_a1, smape_a1, mase_a1, smae_a1, cate_y[1])

## ALL & h = 2
result_table(mape_a2, smape_a2, mase_a2, smae_a2, cate_y[2])

## ALL & h = 3
result_table(mape_a3, smape_a3, mase_a3, smae_a3, cate_y[3])

## ALL & h = 4
result_table(mape_a4, smape_a4, mase_a4, smae_a4, cate_y[4])

## ALL & h = 5
result_table(mape_a5, smape_a5, mase_a5, smae_a5, cate_y[5])

## ALL & h = 6
result_table(mape_a6, smape_a6, mase_a6, smae_a6, cate_y[6])

## ALL & h = 7
result_table(mape_a7, smape_a7, mase_a7, smae_a7, cate_y[7])

## ALL & h = 8
result_table(mape_a8, smape_a8, mase_a8, smae_a8, cate_y[8])

## ALL & h = 9
result_table(mape_a9, smape_a9, mase_a9, smae_a9, cate_y[9])

## ALL & h = 10
result_table(mape_a10, smape_a10, mase_a10, smae_a10, cate_y[10])

## ALL & h = 11
result_table(mape_a11, smape_a11, mase_a11, smae_a11, cate_y[11])

## ALL & h = 12
result_table(mape_a12, smape_a12, mase_a12, smae_a12, cate_y[12])

## ALL & h = 13
result_table(mape_a13, smape_a13, mase_a13, smae_a13, cate_y[13])

## ALL & h = 14
result_table(mape_a14, smape_a14, mase_a14, smae_a14, cate_y[14])

## ALL & h = 15
result_table(mape_a15, smape_a15, mase_a15, smae_a15, cate_y[15])

## ALL & h = 16
result_table(mape_a16, smape_a16, mase_a16, smae_a16, cate_y[16])

## ALL & h = 17
result_table(mape_a17, smape_a17, mase_a17, smae_a17, cate_y[17])

## ALL & h = 18
result_table(mape_a18, smape_a18, mase_a18, smae_a18, cate_y[18])
