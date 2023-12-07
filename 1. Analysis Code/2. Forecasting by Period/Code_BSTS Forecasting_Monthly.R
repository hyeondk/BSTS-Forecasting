### Code : BSTS Forecasting - Monthly
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

## 'MONTHLY' index
idx <- c()
for(a in 1:3003) {
  if(M3[[a]]$period == 'MONTHLY') {
    idx <- c(idx, a)
  }
}
idx # 1402 ~ 2829

## Accuracy : h = 1
mase_m1 <- mape_m1 <- smape_m1 <- smae_m1 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 1
f_m1 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m1[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m1[j, seq(h)]))
    mape_m1[j, i] <- mean(e / ty) * 100
    smape_m1[j, i] <- mean(e / (abs(ty) + abs(f_m1[j, seq(h)]))) * 200
    mase_m1[j, i] <- mean(e / scale)
    smae_m1[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 2
mase_m2 <- mape_m2 <- smape_m2 <- smae_m2 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 2
f_m2 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m2[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m2[j, seq(h)]))
    mape_m2[j, i] <- mean(e / ty) * 100
    smape_m2[j, i] <- mean(e / (abs(ty) + abs(f_m2[j, seq(h)]))) * 200
    mase_m2[j, i] <- mean(e / scale)
    smae_m2[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 3
mase_m3 <- mape_m3 <- smape_m3 <- smae_m3 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 3
f_m3 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m3[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m3[j, seq(h)]))
    mape_m3[j, i] <- mean(e / ty) * 100
    smape_m3[j, i] <- mean(e / (abs(ty) + abs(f_m3[j, seq(h)]))) * 200
    mase_m3[j, i] <- mean(e / scale)
    smae_m3[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 4
mase_m4 <- mape_m4 <- smape_m4 <- smae_m4 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 4
f_m4 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m4[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m4[j, seq(h)]))
    mape_m4[j, i] <- mean(e / ty) * 100
    smape_m4[j, i] <- mean(e / (abs(ty) + abs(f_m4[j, seq(h)]))) * 200
    mase_m4[j, i] <- mean(e / scale)
    smae_m4[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 5
mase_m5 <- mape_m5 <- smape_m5 <- smae_m5 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 5
f_m5 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m5[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m5[j, seq(h)]))
    mape_m5[j, i] <- mean(e / ty) * 100
    smape_m5[j, i] <- mean(e / (abs(ty) + abs(f_m5[j, seq(h)]))) * 200
    mase_m5[j, i] <- mean(e / scale)
    smae_m5[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 6
mase_m6 <- mape_m6 <- smape_m6 <- smae_m6 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 6
f_m6 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m6[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m6[j, seq(h)]))
    mape_m6[j, i] <- mean(e / ty) * 100
    smape_m6[j, i] <- mean(e / (abs(ty) + abs(f_m6[j, seq(h)]))) * 200
    mase_m6[j, i] <- mean(e / scale)
    smae_m6[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 7
mase_m7 <- mape_m7 <- smape_m7 <- smae_m7 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 7
f_m7 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m7[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m7[j, seq(h)]))
    mape_m7[j, i] <- mean(e / ty) * 100
    smape_m7[j, i] <- mean(e / (abs(ty) + abs(f_m7[j, seq(h)]))) * 200
    mase_m7[j, i] <- mean(e / scale)
    smae_m7[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 8
mase_m8 <- mape_m8 <- smape_m8 <- smae_m8 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 8
f_m8 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m8[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m8[j, seq(h)]))
    mape_m8[j, i] <- mean(e / ty) * 100
    smape_m8[j, i] <- mean(e / (abs(ty) + abs(f_m8[j, seq(h)]))) * 200
    mase_m8[j, i] <- mean(e / scale)
    smae_m8[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 9
mase_m9 <- mape_m9 <- smape_m9 <- smae_m9 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 9
f_m9 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m9[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m9[j, seq(h)]))
    mape_m9[j, i] <- mean(e / ty) * 100
    smape_m9[j, i] <- mean(e / (abs(ty) + abs(f_m9[j, seq(h)]))) * 200
    mase_m9[j, i] <- mean(e / scale)
    smae_m9[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 10
mase_m10 <- mape_m10 <- smape_m10 <- smae_m10 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 10
f_m10 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m10[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m10[j, seq(h)]))
    mape_m10[j, i] <- mean(e / ty) * 100
    smape_m10[j, i] <- mean(e / (abs(ty) + abs(f_m10[j, seq(h)]))) * 200
    mase_m10[j, i] <- mean(e / scale)
    smae_m10[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 11
mase_m11 <- mape_m11 <- smape_m11 <- smae_m11 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 11
f_m11 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m11[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m11[j, seq(h)]))
    mape_m11[j, i] <- mean(e / ty) * 100
    smape_m11[j, i] <- mean(e / (abs(ty) + abs(f_m11[j, seq(h)]))) * 200
    mase_m11[j, i] <- mean(e / scale)
    smae_m11[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 12
mase_m12 <- mape_m12 <- smape_m12 <- smae_m12 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 12
f_m12 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m12[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m12[j, seq(h)]))
    mape_m12[j, i] <- mean(e / ty) * 100
    smape_m12[j, i] <- mean(e / (abs(ty) + abs(f_m12[j, seq(h)]))) * 200
    mase_m12[j, i] <- mean(e / scale)
    smae_m12[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 13
mase_m13 <- mape_m13 <- smape_m13 <- smae_m13 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 13
f_m13 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m13[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m13[j, seq(h)]))
    mape_m13[j, i] <- mean(e / ty) * 100
    smape_m13[j, i] <- mean(e / (abs(ty) + abs(f_m13[j, seq(h)]))) * 200
    mase_m13[j, i] <- mean(e / scale)
    smae_m13[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 14
mase_m14 <- mape_m14 <- smape_m14 <- smae_m14 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 14
f_m14 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m14[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m14[j, seq(h)]))
    mape_m14[j, i] <- mean(e / ty) * 100
    smape_m14[j, i] <- mean(e / (abs(ty) + abs(f_m14[j, seq(h)]))) * 200
    mase_m14[j, i] <- mean(e / scale)
    smae_m14[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 15
mase_m15 <- mape_m15 <- smape_m15 <- smae_m15 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 15
f_m15 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m15[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m15[j, seq(h)]))
    mape_m15[j, i] <- mean(e / ty) * 100
    smape_m15[j, i] <- mean(e / (abs(ty) + abs(f_m15[j, seq(h)]))) * 200
    mase_m15[j, i] <- mean(e / scale)
    smae_m15[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 16
mase_m16 <- mape_m16 <- smape_m16 <- smae_m16 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 16
f_m16 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m16[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m16[j, seq(h)]))
    mape_m16[j, i] <- mean(e / ty) * 100
    smape_m16[j, i] <- mean(e / (abs(ty) + abs(f_m16[j, seq(h)]))) * 200
    mase_m16[j, i] <- mean(e / scale)
    smae_m16[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 17
mase_m17 <- mape_m17 <- smape_m17 <- smae_m17 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 17
f_m17 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m17[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m17[j, seq(h)]))
    mape_m17[j, i] <- mean(e / ty) * 100
    smape_m17[j, i] <- mean(e / (abs(ty) + abs(f_m17[j, seq(h)]))) * 200
    mase_m17[j, i] <- mean(e / scale)
    smae_m17[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
  }
  
}

## Accuracy : h = 18
mase_m18 <- mape_m18 <- smape_m18 <- smae_m18 <- matrix(NA, nrow=n_method, ncol=3003)
max_h = 18
f_m18 <- matrix(NA, nrow=n_method, ncol=max_h)

for(i in idx) {
  
  ty <- M3[[i]]$xx[1:max_h]
  h <- max_h
  
  for(j in 1:n_method) f_m18[j, seq(h)] <- tempres[j, seq(h), i]
  
  scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(ty))))
  ybar <- mean(M3[[i]]$x)
  
  for(j in seq(n_method)) {
    e <- abs((ty - f_m18[j, seq(h)]))
    mape_m18[j, i] <- mean(e / ty) * 100
    smape_m18[j, i] <- mean(e / (abs(ty) + abs(f_m18[j, seq(h)]))) * 200
    mase_m18[j, i] <- mean(e / scale)
    smae_m18[j, i] <- mean(e / ybar) # Scaled MAE (Petropoulos and Kourentzes, 2015)
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
cate_y <- c("MONTHLY & h = 1", "MONTHLY & h = 2", "MONTHLY & h = 3", "MONTHLY & h = 4", "MONTHLY & h = 5", "MONTHLY & h = 6",
            "MONTHLY & h = 7", "MONTHLY & h = 8", "MONTHLY & h = 9", "MONTHLY & h = 10", "MONTHLY & h = 11", "MONTHLY & h = 12",
            "MONTHLY & h = 13", "MONTHLY & h = 14", "MONTHLY & h = 15", "MONTHLY & h = 16", "MONTHLY & h = 17", "MONTHLY & h = 18")

## MONTHLY & h = 1
result_table(mape_m1, smape_m1, mase_m1, smae_m1, cate_y[1])

## MONTHLY & h = 2
result_table(mape_m2, smape_m2, mase_m2, smae_m2, cate_y[2])

## MONTHLY & h = 3
result_table(mape_m3, smape_m3, mase_m3, smae_m3, cate_y[3])

## MONTHLY & h = 4
result_table(mape_m4, smape_m4, mase_m4, smae_m4, cate_y[4])

## MONTHLY & h = 5
result_table(mape_m5, smape_m5, mase_m5, smae_m5, cate_y[5])

## MONTHLY & h = 6
result_table(mape_m6, smape_m6, mase_m6, smae_m6, cate_y[6])

## MONTHLY & h = 7
result_table(mape_m7, smape_m7, mase_m7, smae_m7, cate_y[7])

## MONTHLY & h = 8
result_table(mape_m8, smape_m8, mase_m8, smae_m8, cate_y[8])

## MONTHLY & h = 9
result_table(mape_m9, smape_m9, mase_m9, smae_m9, cate_y[9])

## MONTHLY & h = 10
result_table(mape_m10, smape_m10, mase_m10, smae_m10, cate_y[10])

## MONTHLY & h = 11
result_table(mape_m11, smape_m11, mase_m11, smae_m11, cate_y[11])

## MONTHLY & h = 12
result_table(mape_m12, smape_m12, mase_m12, smae_m12, cate_y[12])

## MONTHLY & h = 13
result_table(mape_m13, smape_m13, mase_m13, smae_m13, cate_y[13])

## MONTHLY & h = 14
result_table(mape_m14, smape_m14, mase_m14, smae_m14, cate_y[14])

## MONTHLY & h = 15
result_table(mape_m15, smape_m15, mase_m15, smae_m15, cate_y[15])

## MONTHLY & h = 16
result_table(mape_m16, smape_m16, mase_m16, smae_m16, cate_y[16])

## MONTHLY & h = 17
result_table(mape_m17, smape_m17, mase_m17, smae_m17, cate_y[17])

## MONTHLY & h = 18
result_table(mape_m18, smape_m18, mase_m18, smae_m18, cate_y[18])
