### Code : BSTS Forecasting Visualization - Other
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

################################
### Graph ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Other')
dr %>% dir.exists()

## 1. MAPE ##
mape_o_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(mape_o_df) <- c('type', 'horizon', 'value')
mape_o_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
mape_o_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_o1, smape_o1, mase_o1, smae_o1, cate_y[1])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_o2, smape_o2, mase_o2, smae_o2, cate_y[2])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_o3, smape_o3, mase_o3, smae_o3, cate_y[3])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_o4, smape_o4, mase_o4, smae_o4, cate_y[4])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_o5, smape_o5, mase_o5, smae_o5, cate_y[5])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_o6, smape_o6, mase_o6, smae_o6, cate_y[6])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_o7, smape_o7, mase_o7, smae_o7, cate_y[7])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else {
    temp <- result_table(mape_o8, smape_o8, mase_o8, smae_o8, cate_y[8])$table %>% as.data.frame()
    mape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  }
}

# Plot for all methods
mape_o_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MAPE Graphs for all methods in "OTHER"') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_other.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 2. sMAPE ##
smape_o_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(smape_o_df) <- c('type', 'horizon', 'value')
smape_o_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
smape_o_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_o1, smape_o1, mase_o1, smae_o1, cate_y[1])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_o2, smape_o2, mase_o2, smae_o2, cate_y[2])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_o3, smape_o3, mase_o3, smae_o3, cate_y[3])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_o4, smape_o4, mase_o4, smae_o4, cate_y[4])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_o5, smape_o5, mase_o5, smae_o5, cate_y[5])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_o6, smape_o6, mase_o6, smae_o6, cate_y[6])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_o7, smape_o7, mase_o7, smae_o7, cate_y[7])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else {
    temp <- result_table(mape_o8, smape_o8, mase_o8, smae_o8, cate_y[8])$table %>% as.data.frame()
    smape_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  }
}

# Plot for all methods
smape_o_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAPE Graphs for all methods in "OTHER"') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_other.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 3. MASE ##
mase_o_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(mase_o_df) <- c('type', 'horizon', 'value')
mase_o_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
mase_o_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_o1, smape_o1, mase_o1, smae_o1, cate_y[1])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_o2, smape_o2, mase_o2, smae_o2, cate_y[2])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_o3, smape_o3, mase_o3, smae_o3, cate_y[3])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_o4, smape_o4, mase_o4, smae_o4, cate_y[4])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_o5, smape_o5, mase_o5, smae_o5, cate_y[5])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_o6, smape_o6, mase_o6, smae_o6, cate_y[6])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_o7, smape_o7, mase_o7, smae_o7, cate_y[7])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else {
    temp <- result_table(mape_o8, smape_o8, mase_o8, smae_o8, cate_y[8])$table %>% as.data.frame()
    mase_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  }
}

# Plot for all methods
mase_o_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MASE Graphs for all methods in "OTHER"') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_other.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 4. sMAE ##
smae_o_df <- data.frame(matrix(NA, nrow=n_method*8, ncol=3))
names(smae_o_df) <- c('type', 'horizon', 'value')
smae_o_df$type <- rep(c("ETS", "ARIMA", "BSTS", "Hybrid"), times=8)
smae_o_df$horizon <- rep(1:8, each=n_method)

for(i in 1:8) {
  if(i == 1) {
    temp <- result_table(mape_o1, smape_o1, mase_o1, smae_o1, cate_y[1])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_o2, smape_o2, mase_o2, smae_o2, cate_y[2])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_o3, smape_o3, mase_o3, smae_o3, cate_y[3])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_o4, smape_o4, mase_o4, smae_o4, cate_y[4])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_o5, smape_o5, mase_o5, smae_o5, cate_y[5])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 6) {
    temp <- result_table(mape_o6, smape_o6, mase_o6, smae_o6, cate_y[6])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 7) {
    temp <- result_table(mape_o7, smape_o7, mase_o7, smae_o7, cate_y[7])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else {
    temp <- result_table(mape_o8, smape_o8, mase_o8, smae_o8, cate_y[8])$table %>% as.data.frame()
    smae_o_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  }
}

# Plot for all methods
smae_o_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAE Graphs for all methods in "OTHER"') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_other.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)
