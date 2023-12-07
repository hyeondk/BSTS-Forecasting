### Code : BSTS Forecasting Visualization - Quarterly
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

################################
### Graph ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Quarterly')
dr %>% dir.exists()

## 1. MAPE ##
mape_q_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(mape_q_df) <- c('type', 'horizon', 'value')
mape_q_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
mape_q_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_q1, smape_q1, mase_q1, smae_q1, cate_y[1])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_q2, smape_q2, mase_q2, smae_q2, cate_y[2])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_q3, smape_q3, mase_q3, smae_q3, cate_y[3])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_q4, smape_q4, mase_q4, smae_q4, cate_y[4])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_q5, smape_q5, mase_q5, smae_q5, cate_y[5])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_q6, smape_q6, mase_q6, smae_q6, cate_y[6])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_q7, smape_q7, mase_q7, smae_q7, cate_y[7])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else {
    temp <- result_table(mape_q8, smape_q8, mase_q8, smae_q8, cate_y[8])$table %>% as.data.frame()
    mape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  }
}

# Plot for all methods
mape_q_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MAPE Graphs for all methods in "QUARTERLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 2. sMAPE ##
smape_q_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(smape_q_df) <- c('type', 'horizon', 'value')
smape_q_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
smape_q_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_q1, smape_q1, mase_q1, smae_q1, cate_y[1])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_q2, smape_q2, mase_q2, smae_q2, cate_y[2])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_q3, smape_q3, mase_q3, smae_q3, cate_y[3])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_q4, smape_q4, mase_q4, smae_q4, cate_y[4])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_q5, smape_q5, mase_q5, smae_q5, cate_y[5])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_q6, smape_q6, mase_q6, smae_q6, cate_y[6])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_q7, smape_q7, mase_q7, smae_q7, cate_y[7])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else {
    temp <- result_table(mape_q8, smape_q8, mase_q8, smae_q8, cate_y[8])$table %>% as.data.frame()
    smape_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  }
}

# Plot for all methods
smape_q_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAPE Graphs for all methods in "QUARTERLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 3. MASE ##
mase_q_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(mase_q_df) <- c('type', 'horizon', 'value')
mase_q_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
mase_q_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_q1, smape_q1, mase_q1, smae_q1, cate_y[1])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_q2, smape_q2, mase_q2, smae_q2, cate_y[2])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_q3, smape_q3, mase_q3, smae_q3, cate_y[3])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_q4, smape_q4, mase_q4, smae_q4, cate_y[4])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_q5, smape_q5, mase_q5, smae_q5, cate_y[5])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_q6, smape_q6, mase_q6, smae_q6, cate_y[6])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_q7, smape_q7, mase_q7, smae_q7, cate_y[7])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else {
    temp <- result_table(mape_q8, smape_q8, mase_q8, smae_q8, cate_y[8])$table %>% as.data.frame()
    mase_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  }
}

# Plot for all methods
mase_q_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MASE Graphs for all methods in "QUARTERLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 4. sMAE ##
smae_q_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(smae_q_df) <- c('type', 'horizon', 'value')
smae_q_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
smae_q_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_q1, smape_q1, mase_q1, smae_q1, cate_y[1])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_q2, smape_q2, mase_q2, smae_q2, cate_y[2])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_q3, smape_q3, mase_q3, smae_q3, cate_y[3])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_q4, smape_q4, mase_q4, smae_q4, cate_y[4])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_q5, smape_q5, mase_q5, smae_q5, cate_y[5])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_q6, smape_q6, mase_q6, smae_q6, cate_y[6])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_q7, smape_q7, mase_q7, smae_q7, cate_y[7])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else {
    temp <- result_table(mape_q8, smape_q8, mase_q8, smae_q8, cate_y[8])$table %>% as.data.frame()
    smae_q_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  }
}

# Plot for all methods
smae_q_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAE Graphs for all methods in "QUARTERLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)
