### Code : BSTS Forecasting Visualization - Monthly
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
cate_y <- c("MONTHLY & h = 1", "MONTHLY & h = 2", "MONTHLY & h = 3", "MONTHLY & h = 4", "MONTHLY & h = 5", "MONTHLY & h = 6",
            "MONTHLY & h = 7", "MONTHLY & h = 8", "MONTHLY & h = 9", "MONTHLY & h = 10", "MONTHLY & h = (n_method-1)", "MONTHLY & h = n_method",
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

################################
### Graph ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Monthly')
dr %>% dir.exists()

## 1. MAPE ##
mape_m_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(mape_m_df) <- c('type', 'horizon', 'value')
mape_m_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
mape_m_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_m1, smape_m1, mase_m1, smae_m1, cate_y[1])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_m2, smape_m2, mase_m2, smae_m2, cate_y[2])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_m3, smape_m3, mase_m3, smae_m3, cate_y[3])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_m4, smape_m4, mase_m4, smae_m4, cate_y[4])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_m5, smape_m5, mase_m5, smae_m5, cate_y[5])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_m6, smape_m6, mase_m6, smae_m6, cate_y[6])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_m7, smape_m7, mase_m7, smae_m7, cate_y[7])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_m8, smape_m8, mase_m8, smae_m8, cate_y[8])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_m9, smape_m9, mase_m9, smae_m9, cate_y[9])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_m10, smape_m10, mase_m10, smae_m10, cate_y[10])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_m11, smape_m11, mase_m11, smae_m11, cate_y[11])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_m12, smape_m12, mase_m12, smae_m12, cate_y[12])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_m13, smape_m13, mase_m13, smae_m13, cate_y[13])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_m14, smape_m14, mase_m14, smae_m14, cate_y[14])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_m15, smape_m15, mase_m15, smae_m15, cate_y[15])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_m16, smape_m16, mase_m16, smae_m16, cate_y[16])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_m17, smape_m17, mase_m17, smae_m17, cate_y[17])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else {
    temp <- result_table(mape_m18, smape_m18, mase_m18, smae_m18, cate_y[18])$table %>% as.data.frame()
    mape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  }
}

# Plot for all methods
mape_m_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MAPE Graphs for all methods in "MONTHLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 2. sMAPE ##
smape_m_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(smape_m_df) <- c('type', 'horizon', 'value')
smape_m_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
smape_m_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_m1, smape_m1, mase_m1, smae_m1, cate_y[1])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_m2, smape_m2, mase_m2, smae_m2, cate_y[2])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_m3, smape_m3, mase_m3, smae_m3, cate_y[3])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_m4, smape_m4, mase_m4, smae_m4, cate_y[4])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_m5, smape_m5, mase_m5, smae_m5, cate_y[5])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_m6, smape_m6, mase_m6, smae_m6, cate_y[6])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_m7, smape_m7, mase_m7, smae_m7, cate_y[7])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_m8, smape_m8, mase_m8, smae_m8, cate_y[8])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_m9, smape_m9, mase_m9, smae_m9, cate_y[9])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_m10, smape_m10, mase_m10, smae_m10, cate_y[10])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_m11, smape_m11, mase_m11, smae_m11, cate_y[11])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_m12, smape_m12, mase_m12, smae_m12, cate_y[12])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_m13, smape_m13, mase_m13, smae_m13, cate_y[13])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_m14, smape_m14, mase_m14, smae_m14, cate_y[14])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_m15, smape_m15, mase_m15, smae_m15, cate_y[15])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_m16, smape_m16, mase_m16, smae_m16, cate_y[16])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_m17, smape_m17, mase_m17, smae_m17, cate_y[17])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else {
    temp <- result_table(mape_m18, smape_m18, mase_m18, smae_m18, cate_y[18])$table %>% as.data.frame()
    smape_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  }
}

# Plot for all methods
smape_m_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAPE Graphs for all methods in "MONTHLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 3. MASE ##
mase_m_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(mase_m_df) <- c('type', 'horizon', 'value')
mase_m_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
mase_m_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_m1, smape_m1, mase_m1, smae_m1, cate_y[1])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_m2, smape_m2, mase_m2, smae_m2, cate_y[2])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_m3, smape_m3, mase_m3, smae_m3, cate_y[3])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_m4, smape_m4, mase_m4, smae_m4, cate_y[4])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_m5, smape_m5, mase_m5, smae_m5, cate_y[5])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_m6, smape_m6, mase_m6, smae_m6, cate_y[6])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_m7, smape_m7, mase_m7, smae_m7, cate_y[7])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_m8, smape_m8, mase_m8, smae_m8, cate_y[8])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_m9, smape_m9, mase_m9, smae_m9, cate_y[9])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_m10, smape_m10, mase_m10, smae_m10, cate_y[10])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_m11, smape_m11, mase_m11, smae_m11, cate_y[11])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_m12, smape_m12, mase_m12, smae_m12, cate_y[12])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_m13, smape_m13, mase_m13, smae_m13, cate_y[13])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_m14, smape_m14, mase_m14, smae_m14, cate_y[14])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_m15, smape_m15, mase_m15, smae_m15, cate_y[15])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_m16, smape_m16, mase_m16, smae_m16, cate_y[16])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_m17, smape_m17, mase_m17, smae_m17, cate_y[17])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else {
    temp <- result_table(mape_m18, smape_m18, mase_m18, smae_m18, cate_y[18])$table %>% as.data.frame()
    mase_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  }
}

# Plot for all methods
mase_m_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MASE Graphs for all methods in "MONTHLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 4. sMAE ##
smae_m_df <- data.frame(matrix(NA, nrow=n_method*18, ncol=3))
names(smae_m_df) <- c('type', 'horizon', 'value')
smae_m_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=18)
smae_m_df$horizon <- rep(1:18, each=n_method)

for(i in 1:18) {
  if(i == 1) {
    temp <- result_table(mape_m1, smape_m1, mase_m1, smae_m1, cate_y[1])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_m2, smape_m2, mase_m2, smae_m2, cate_y[2])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_m3, smape_m3, mase_m3, smae_m3, cate_y[3])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_m4, smape_m4, mase_m4, smae_m4, cate_y[4])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_m5, smape_m5, mase_m5, smae_m5, cate_y[5])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_m6, smape_m6, mase_m6, smae_m6, cate_y[6])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_m7, smape_m7, mase_m7, smae_m7, cate_y[7])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 8) {
    temp <- result_table(mape_m8, smape_m8, mase_m8, smae_m8, cate_y[8])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 9) {
    temp <- result_table(mape_m9, smape_m9, mase_m9, smae_m9, cate_y[9])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 10) {
    temp <- result_table(mape_m10, smape_m10, mase_m10, smae_m10, cate_y[10])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 11) {
    temp <- result_table(mape_m11, smape_m11, mase_m11, smae_m11, cate_y[11])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 12) {
    temp <- result_table(mape_m12, smape_m12, mase_m12, smae_m12, cate_y[12])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 13) {
    temp <- result_table(mape_m13, smape_m13, mase_m13, smae_m13, cate_y[13])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 14) {
    temp <- result_table(mape_m14, smape_m14, mase_m14, smae_m14, cate_y[14])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 15) {
    temp <- result_table(mape_m15, smape_m15, mase_m15, smae_m15, cate_y[15])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 16) {
    temp <- result_table(mape_m16, smape_m16, mase_m16, smae_m16, cate_y[16])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 17) {
    temp <- result_table(mape_m17, smape_m17, mase_m17, smae_m17, cate_y[17])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else {
    temp <- result_table(mape_m18, smape_m18, mase_m18, smae_m18, cate_y[18])$table %>% as.data.frame()
    smae_m_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  }
}

# Plot for all methods
smae_m_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAE Graphs for all methods in "MONTHLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)
