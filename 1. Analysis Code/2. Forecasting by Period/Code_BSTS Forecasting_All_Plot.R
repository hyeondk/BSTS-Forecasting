### Code : BSTS Forecasting Visualization - All
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
### Result ###

## Category
cate_y <- c("ALL & h = 1", "ALL & h = 2", "ALL & h = 3", "ALL & h = 4", "ALL & h = 5", "ALL & h = 6",
            "ALL & h = 7", "ALL & h = 8", "ALL & h = 9", "ALL & h = 10", "ALL & h = (n_method-1)", "ALL & h = n_method",
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

################################
### Graph ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_ALL')
dr %>% dir.exists()

## 1. MAPE ##
mape_a_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(mape_a_df) <- c('type', 'horizon', 'value')
mape_a_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
mape_a_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_a1, smape_a1, mase_a1, smae_a1, cate_y[1])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_a2, smape_a2, mase_a2, smae_a2, cate_y[2])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_a3, smape_a3, mase_a3, smae_a3, cate_y[3])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_a4, smape_a4, mase_a4, smae_a4, cate_y[4])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_a5, smape_a5, mase_a5, smae_a5, cate_y[5])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_a6, smape_a6, mase_a6, smae_a6, cate_y[6])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_a7, smape_a7, mase_a7, smae_a7, cate_y[7])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_a8, smape_a8, mase_a8, smae_a8, cate_y[8])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_a9, smape_a9, mase_a9, smae_a9, cate_y[9])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_a10, smape_a10, mase_a10, smae_a10, cate_y[10])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_a11, smape_a11, mase_a11, smae_a11, cate_y[11])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_a12, smape_a12, mase_a12, smae_a12, cate_y[12])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_a13, smape_a13, mase_a13, smae_a13, cate_y[13])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_a14, smape_a14, mase_a14, smae_a14, cate_y[14])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_a15, smape_a15, mase_a15, smae_a15, cate_y[15])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_a16, smape_a16, mase_a16, smae_a16, cate_y[16])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_a17, smape_a17, mase_a17, smae_a17, cate_y[17])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else {
    temp <- result_table(mape_a18, smape_a18, mase_a18, smae_a18, cate_y[18])$table %>% as.data.frame()
    mape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  }
}

# Plot for all methods
mape_a_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MAPE Graphs for all methods in "ALL"') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_ALL.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 2. sMAPE ##
smape_a_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(smape_a_df) <- c('type', 'horizon', 'value')
smape_a_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
smape_a_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_a1, smape_a1, mase_a1, smae_a1, cate_y[1])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_a2, smape_a2, mase_a2, smae_a2, cate_y[2])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_a3, smape_a3, mase_a3, smae_a3, cate_y[3])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_a4, smape_a4, mase_a4, smae_a4, cate_y[4])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_a5, smape_a5, mase_a5, smae_a5, cate_y[5])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_a6, smape_a6, mase_a6, smae_a6, cate_y[6])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_a7, smape_a7, mase_a7, smae_a7, cate_y[7])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_a8, smape_a8, mase_a8, smae_a8, cate_y[8])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_a9, smape_a9, mase_a9, smae_a9, cate_y[9])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_a10, smape_a10, mase_a10, smae_a10, cate_y[10])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_a11, smape_a11, mase_a11, smae_a11, cate_y[11])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_a12, smape_a12, mase_a12, smae_a12, cate_y[12])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_a13, smape_a13, mase_a13, smae_a13, cate_y[13])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_a14, smape_a14, mase_a14, smae_a14, cate_y[14])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_a15, smape_a15, mase_a15, smae_a15, cate_y[15])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_a16, smape_a16, mase_a16, smae_a16, cate_y[16])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_a17, smape_a17, mase_a17, smae_a17, cate_y[17])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else {
    temp <- result_table(mape_a18, smape_a18, mase_a18, smae_a18, cate_y[18])$table %>% as.data.frame()
    smape_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  }
}

# Plot for all methods
smape_a_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAPE Graphs for all methods in "ALL"') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_ALL.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 3. MASE ##
mase_a_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(mase_a_df) <- c('type', 'horizon', 'value')
mase_a_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
mase_a_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_a1, smape_a1, mase_a1, smae_a1, cate_y[1])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_a2, smape_a2, mase_a2, smae_a2, cate_y[2])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_a3, smape_a3, mase_a3, smae_a3, cate_y[3])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_a4, smape_a4, mase_a4, smae_a4, cate_y[4])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_a5, smape_a5, mase_a5, smae_a5, cate_y[5])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_a6, smape_a6, mase_a6, smae_a6, cate_y[6])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_a7, smape_a7, mase_a7, smae_a7, cate_y[7])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_a8, smape_a8, mase_a8, smae_a8, cate_y[8])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_a9, smape_a9, mase_a9, smae_a9, cate_y[9])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_a10, smape_a10, mase_a10, smae_a10, cate_y[10])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_a11, smape_a11, mase_a11, smae_a11, cate_y[11])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_a12, smape_a12, mase_a12, smae_a12, cate_y[12])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_a13, smape_a13, mase_a13, smae_a13, cate_y[13])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_a14, smape_a14, mase_a14, smae_a14, cate_y[14])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_a15, smape_a15, mase_a15, smae_a15, cate_y[15])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_a16, smape_a16, mase_a16, smae_a16, cate_y[16])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_a17, smape_a17, mase_a17, smae_a17, cate_y[17])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else {
    temp <- result_table(mape_a18, smape_a18, mase_a18, smae_a18, cate_y[18])$table %>% as.data.frame()
    mase_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  }
}

# Plot for all methods
mase_a_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MASE Graphs for all methods in "ALL"') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_ALL.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 4. sMAE ##
smae_a_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(smae_a_df) <- c('type', 'horizon', 'value')
smae_a_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
smae_a_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_a1, smape_a1, mase_a1, smae_a1, cate_y[1])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_a2, smape_a2, mase_a2, smae_a2, cate_y[2])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_a3, smape_a3, mase_a3, smae_a3, cate_y[3])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_a4, smape_a4, mase_a4, smae_a4, cate_y[4])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_a5, smape_a5, mase_a5, smae_a5, cate_y[5])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_a6, smape_a6, mase_a6, smae_a6, cate_y[6])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_a7, smape_a7, mase_a7, smae_a7, cate_y[7])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_a8, smape_a8, mase_a8, smae_a8, cate_y[8])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_a9, smape_a9, mase_a9, smae_a9, cate_y[9])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_a10, smape_a10, mase_a10, smae_a10, cate_y[10])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_a11, smape_a11, mase_a11, smae_a11, cate_y[11])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_a12, smape_a12, mase_a12, smae_a12, cate_y[12])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_a13, smape_a13, mase_a13, smae_a13, cate_y[13])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_a14, smape_a14, mase_a14, smae_a14, cate_y[14])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_a15, smape_a15, mase_a15, smae_a15, cate_y[15])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_a16, smape_a16, mase_a16, smae_a16, cate_y[16])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_a17, smape_a17, mase_a17, smae_a17, cate_y[17])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else {
    temp <- result_table(mape_a18, smape_a18, mase_a18, smae_a18, cate_y[18])$table %>% as.data.frame()
    smae_a_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  }
}

# Plot for all methods
smae_a_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAE Graphs for all methods in "ALL"') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_ALL.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)
