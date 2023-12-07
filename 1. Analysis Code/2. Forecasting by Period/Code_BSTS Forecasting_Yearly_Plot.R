### Code : BSTS Forecasting Visualization - Yearly
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

################################
### Graph ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Yearly')
dr %>% dir.exists()

## 1. MAPE ##
mape_y_df <- data.frame(matrix(NA, nrow=n_method*6, ncol=3))
names(mape_y_df) <- c('type', 'horizon', 'value')
mape_y_df$type <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), times=6)
mape_y_df$horizon <- rep(1:6, each=n_method)

for(i in 1:6) {
  if(i == 1) {
    temp <- result_table(mape_y1, smape_y1, mase_y1, smae_y1, cate_y[1])$table %>% as.data.frame()
    mape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_y2, smape_y2, mase_y2, smae_y2, cate_y[2])$table %>% as.data.frame()
    mape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_y3, smape_y3, mase_y3, smae_y3, cate_y[3])$table %>% as.data.frame()
    mape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_y4, smape_y4, mase_y4, smae_y4, cate_y[4])$table %>% as.data.frame()
    mape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_y5, smape_y5, mase_y5, smae_y5, cate_y[5])$table %>% as.data.frame()
    mape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  } else {
    temp <- result_table(mape_y6, smape_y6, mase_y6, smae_y6, cate_y[6])$table %>% as.data.frame()
    mape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MAPE
    rm(temp)
  }
}

# Plot for all methods
mape_y_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MAPE Graphs for all methods in "YEARLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 2. sMAPE ##
smape_y_df <- data.frame(matrix(NA, nrow=n_method*6, ncol=3))
names(smape_y_df) <- c('type', 'horizon', 'value')
smape_y_df$type <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), times=6)
smape_y_df$horizon <- rep(1:6, each=n_method)

for(i in 1:6) {
  if(i == 1) {
    temp <- result_table(mape_y1, smape_y1, mase_y1, smae_y1, cate_y[1])$table %>% as.data.frame()
    smape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_y2, smape_y2, mase_y2, smae_y2, cate_y[2])$table %>% as.data.frame()
    smape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_y3, smape_y3, mase_y3, smae_y3, cate_y[3])$table %>% as.data.frame()
    smape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_y4, smape_y4, mase_y4, smae_y4, cate_y[4])$table %>% as.data.frame()
    smape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_y5, smape_y5, mase_y5, smae_y5, cate_y[5])$table %>% as.data.frame()
    smape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  } else {
    temp <- result_table(mape_y6, smape_y6, mase_y6, smae_y6, cate_y[6])$table %>% as.data.frame()
    smape_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAPE
    rm(temp)
  }
}

# Plot for all methods
smape_y_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAPE Graphs for all methods in "YEARLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 3. MASE ##
mase_y_df <- data.frame(matrix(NA, nrow=n_method*6, ncol=3))
names(mase_y_df) <- c('type', 'horizon', 'value')
mase_y_df$type <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), times=6)
mase_y_df$horizon <- rep(1:6, each=n_method)

for(i in 1:6) {
  if(i == 1) {
    temp <- result_table(mape_y1, smape_y1, mase_y1, smae_y1, cate_y[1])$table %>% as.data.frame()
    mase_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_y2, smape_y2, mase_y2, smae_y2, cate_y[2])$table %>% as.data.frame()
    mase_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_y3, smape_y3, mase_y3, smae_y3, cate_y[3])$table %>% as.data.frame()
    mase_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_y4, smape_y4, mase_y4, smae_y4, cate_y[4])$table %>% as.data.frame()
    mase_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_y5, smape_y5, mase_y5, smae_y5, cate_y[5])$table %>% as.data.frame()
    mase_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  } else {
    temp <- result_table(mape_y6, smape_y6, mase_y6, smae_y6, cate_y[6])$table %>% as.data.frame()
    mase_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$MASE
    rm(temp)
  }
}

# Plot for all methods
mase_y_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('MASE Graphs for all methods in "YEARLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


## 4. sMAE ##
smae_y_df <- data.frame(matrix(NA, nrow=n_method*6, ncol=3))
names(smae_y_df) <- c('type', 'horizon', 'value')
smae_y_df$type <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), times=6)
smae_y_df$horizon <- rep(1:6, each=n_method)

for(i in 1:6) {
  if(i == 1) {
    temp <- result_table(mape_y1, smape_y1, mase_y1, smae_y1, cate_y[1])$table %>% as.data.frame()
    smae_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 2) {
    temp <- result_table(mape_y2, smape_y2, mase_y2, smae_y2, cate_y[2])$table %>% as.data.frame()
    smae_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 3) {
    temp <- result_table(mape_y3, smape_y3, mase_y3, smae_y3, cate_y[3])$table %>% as.data.frame()
    smae_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 4) {
    temp <- result_table(mape_y4, smape_y4, mase_y4, smae_y4, cate_y[4])$table %>% as.data.frame()
    smae_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else if(i == 5) {
    temp <- result_table(mape_y5, smape_y5, mase_y5, smae_y5, cate_y[5])$table %>% as.data.frame()
    smae_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  } else {
    temp <- result_table(mape_y6, smape_y6, mase_y6, smae_y6, cate_y[6])$table %>% as.data.frame()
    smae_y_df[(n_method*i - (n_method-1)):(n_method*i), 'value'] <- temp$sMAE
    rm(temp)
  }
}

# Plot for all methods
smae_y_df %>% ggplot(aes(x=horizon, y=value, color=type, group=type)) +
  geom_line() + geom_point() + ggtitle('sMAE Graphs for all methods in "YEARLY"') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)
