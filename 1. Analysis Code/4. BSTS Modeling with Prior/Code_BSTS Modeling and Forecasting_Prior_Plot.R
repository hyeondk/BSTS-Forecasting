### Code : BSTS Forecasting Visualization
### Writer : Donghyeon Kim
### Update : 2023.11.24.

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

## 1. Accuracy by period ##
result

## 2. Accuracy by sample size ##
result2

## 3. Accuracy by forecast horizon ##
result3

################################
### Graph by Sample Size ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Case1')
dr %>% dir.exists()

## 1. Sample Size : 0 < n <= 20
df_sample_1 <- data.frame(matrix(NA, nrow=n_method, ncol=5))
names(df_sample_1) <- c('Model', 'MAPE', 'sMAPE', 'MASE', 'sMAE')
df_sample_1$Model <- c('ETS', 'ARIMA', 'BSTS', 'Hybrid')

for(rn in 1:4) {
  for(cn in 1:4) {
    df_sample_1[rn, cn+1] <- result2[[1]]$table[rn, cn]
  }
}

df_sample_1_all <- data.frame(matrix(NA, nrow=n_method*4, ncol=3))
names(df_sample_1_all) <- c('Model', 'Type', 'Error')
df_sample_1_all$Model <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), each=4)
df_sample_1_all$Type <- rep(c('MAPE', 'sMAPE', 'MASE', 'sMAE'), times=4)

err_value <- c()
for(rn in 1:4) {
  err_value <- c(err_value, result2[[1]]$table[rn, 1:4])
}
df_sample_1_all$Error <- err_value

# Plot 1 : MAPE
df_sample_1 %>% ggplot(aes(x=Model, y=MAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MAPE Graphs for 0 < n <= 20') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_Case1.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 2 : sMAPE
df_sample_1 %>% ggplot(aes(x=Model, y=sMAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAPE Graphs for 0 < n <= 20') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_Case1.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 3 : MASE
df_sample_1 %>% ggplot(aes(x=Model, y=MASE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MASE Graphs for 0 < n <= 20') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_Case1.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 4 : sMAE
df_sample_1 %>% ggplot(aes(x=Model, y=sMAE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAE Graphs for 0 < n <= 20') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_Case1.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 5 : All Error
df_sample_1_all %>% ggplot(aes(x=Type, y=Error, fill=Model)) + geom_bar(stat='identity', position='dodge', alpha=0.7, width=0.5) +
  ggtitle('All Error Graphs for 0 < n <= 20') + theme_bw()

ggsave(file=paste0(dr, '/', '5. All Error_Case1.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

##---------------------------##

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Case2')
dr %>% dir.exists()

## 2. Sample Size : 20 < n <= 40
df_sample_2 <- data.frame(matrix(NA, nrow=n_method, ncol=5))
names(df_sample_2) <- c('Model', 'MAPE', 'sMAPE', 'MASE', 'sMAE')
df_sample_2$Model <- c('ETS', 'ARIMA', 'BSTS', 'Hybrid')

for(rn in 1:4) {
  for(cn in 1:4) {
    df_sample_2[rn, cn+1] <- result2[[2]]$table[rn, cn]
  }
}

df_sample_2_all <- data.frame(matrix(NA, nrow=n_method*4, ncol=3))
names(df_sample_2_all) <- c('Model', 'Type', 'Error')
df_sample_2_all$Model <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), each=4)
df_sample_2_all$Type <- rep(c('MAPE', 'sMAPE', 'MASE', 'sMAE'), times=4)

err_value <- c()
for(rn in 1:4) {
  err_value <- c(err_value, result2[[2]]$table[rn, 1:4])
}
df_sample_2_all$Error <- err_value

# Plot 1 : MAPE
df_sample_2 %>% ggplot(aes(x=Model, y=MAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MAPE Graphs for 20 < n <= 40') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_Case2.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 2 : sMAPE
df_sample_2 %>% ggplot(aes(x=Model, y=sMAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAPE Graphs for 20 < n <= 40') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_Case2.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 3 : MASE
df_sample_2 %>% ggplot(aes(x=Model, y=MASE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MASE Graphs for 20 < n <= 40') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_Case2.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 4 : sMAE
df_sample_2 %>% ggplot(aes(x=Model, y=sMAE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAE Graphs for 20 < n <= 40') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_Case2.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 5 : All Error
df_sample_2_all %>% ggplot(aes(x=Type, y=Error, fill=Model)) + geom_bar(stat='identity', position='dodge', alpha=0.7, width=0.5) +
  ggtitle('All Error Graphs for 20 < n <= 40') + theme_bw()

ggsave(file=paste0(dr, '/', '5. All Error_Case2.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)


################################
### Graph by Period ###

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Yearly')
dr %>% dir.exists()

## 1. Yearly Data
df_period_1 <- data.frame(matrix(NA, nrow=n_method, ncol=5))
names(df_period_1) <- c('Model', 'MAPE', 'sMAPE', 'MASE', 'sMAE')
df_period_1$Model <- c('ETS', 'ARIMA', 'BSTS', 'Hybrid')

for(rn in 1:4) {
  for(cn in 1:4) {
    df_period_1[rn, cn+1] <- result[[1]]$table[rn, cn]
  }
}

df_period_1_all <- data.frame(matrix(NA, nrow=n_method*4, ncol=3))
names(df_period_1_all) <- c('Model', 'Type', 'Error')
df_period_1_all$Model <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), each=4)
df_period_1_all$Type <- rep(c('MAPE', 'sMAPE', 'MASE', 'sMAE'), times=4)

err_value <- c()
for(rn in 1:4) {
  err_value <- c(err_value, result[[1]]$table[rn, 1:4])
}
df_period_1_all$Error <- err_value

# Plot 1 : MAPE
df_period_1 %>% ggplot(aes(x=Model, y=MAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MAPE Graphs for Yearly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_Yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 2 : sMAPE
df_period_1 %>% ggplot(aes(x=Model, y=sMAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAPE Graphs for Yearly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_Yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 3 : MASE
df_period_1 %>% ggplot(aes(x=Model, y=MASE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MASE Graphs for Yearly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_Yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 4 : sMAE
df_period_1 %>% ggplot(aes(x=Model, y=sMAE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAE Graphs for Yearly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_Yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 5 : All Error
df_period_1_all %>% ggplot(aes(x=Type, y=Error, fill=Model)) + geom_bar(stat='identity', position='dodge', alpha=0.7, width=0.5) +
  ggtitle('All Error Graphs for Yearly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '5. All Error_Yearly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

##---------------------------##

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Quarterly')
dr %>% dir.exists()

## 2. Quarterly Data
df_period_2 <- data.frame(matrix(NA, nrow=n_method, ncol=5))
names(df_period_2) <- c('Model', 'MAPE', 'sMAPE', 'MASE', 'sMAE')
df_period_2$Model <- c('ETS', 'ARIMA', 'BSTS', 'Hybrid')

for(rn in 1:4) {
  for(cn in 1:4) {
    df_period_2[rn, cn+1] <- result[[2]]$table[rn, cn]
  }
}

df_period_2_all <- data.frame(matrix(NA, nrow=n_method*4, ncol=3))
names(df_period_2_all) <- c('Model', 'Type', 'Error')
df_period_2_all$Model <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), each=4)
df_period_2_all$Type <- rep(c('MAPE', 'sMAPE', 'MASE', 'sMAE'), times=4)

err_value <- c()
for(rn in 1:4) {
  err_value <- c(err_value, result[[2]]$table[rn, 1:4])
}
df_period_2_all$Error <- err_value

# Plot 1 : MAPE
df_period_2 %>% ggplot(aes(x=Model, y=MAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MAPE Graphs for Quarterly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_Quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 2 : sMAPE
df_period_2 %>% ggplot(aes(x=Model, y=sMAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAPE Graphs for Quarterly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_Quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 3 : MASE
df_period_2 %>% ggplot(aes(x=Model, y=MASE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MASE Graphs for Quarterly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_Quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 4 : sMAE
df_period_2 %>% ggplot(aes(x=Model, y=sMAE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAE Graphs for Quarterly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_Quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 5 : All Error
df_period_2_all %>% ggplot(aes(x=Type, y=Error, fill=Model)) + geom_bar(stat='identity', position='dodge', alpha=0.7, width=0.5) +
  ggtitle('All Error Graphs for Quarterly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '5. All Error_Quarterly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

##---------------------------##

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_Monthly')
dr %>% dir.exists()

## 3. Monthly Data
df_period_3 <- data.frame(matrix(NA, nrow=n_method, ncol=5))
names(df_period_3) <- c('Model', 'MAPE', 'sMAPE', 'MASE', 'sMAE')
df_period_3$Model <- c('ETS', 'ARIMA', 'BSTS', 'Hybrid')

for(rn in 1:4) {
  for(cn in 1:4) {
    df_period_3[rn, cn+1] <- result[[3]]$table[rn, cn]
  }
}

df_period_3_all <- data.frame(matrix(NA, nrow=n_method*4, ncol=3))
names(df_period_3_all) <- c('Model', 'Type', 'Error')
df_period_3_all$Model <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), each=4)
df_period_3_all$Type <- rep(c('MAPE', 'sMAPE', 'MASE', 'sMAE'), times=4)

err_value <- c()
for(rn in 1:4) {
  err_value <- c(err_value, result[[3]]$table[rn, 1:4])
}
df_period_3_all$Error <- err_value

# Plot 1 : MAPE
df_period_3 %>% ggplot(aes(x=Model, y=MAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MAPE Graphs for Monthly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_Monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 2 : sMAPE
df_period_3 %>% ggplot(aes(x=Model, y=sMAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAPE Graphs for Monthly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_Monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 3 : MASE
df_period_3 %>% ggplot(aes(x=Model, y=MASE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MASE Graphs for Monthly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_Monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 4 : sMAE
df_period_3 %>% ggplot(aes(x=Model, y=sMAE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAE Graphs for Monthly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_Monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 5 : All Error
df_period_3_all %>% ggplot(aes(x=Type, y=Error, fill=Model)) + geom_bar(stat='identity', position='dodge', alpha=0.7, width=0.5) +
  ggtitle('All Error Graphs for Monthly Data') + theme_bw()

ggsave(file=paste0(dr, '/', '5. All Error_Monthly.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

##---------------------------##

## Prior Settings ##
dr <- paste0(getwd(), '/', 'Plot_All')
dr %>% dir.exists()

## 4. All Data
df_period_4 <- data.frame(matrix(NA, nrow=n_method, ncol=5))
names(df_period_4) <- c('Model', 'MAPE', 'sMAPE', 'MASE', 'sMAE')
df_period_4$Model <- c('ETS', 'ARIMA', 'BSTS', 'Hybrid')

for(rn in 1:4) {
  for(cn in 1:4) {
    df_period_4[rn, cn+1] <- result[[5]]$table[rn, cn]
  }
}

df_period_4_all <- data.frame(matrix(NA, nrow=n_method*4, ncol=3))
names(df_period_4_all) <- c('Model', 'Type', 'Error')
df_period_4_all$Model <- rep(c('ETS', 'ARIMA', 'BSTS', 'Hybrid'), each=4)
df_period_4_all$Type <- rep(c('MAPE', 'sMAPE', 'MASE', 'sMAE'), times=4)

err_value <- c()
for(rn in 1:4) {
  err_value <- c(err_value, result[[5]]$table[rn, 1:4])
}
df_period_4_all$Error <- err_value

# Plot 1 : MAPE
df_period_4 %>% ggplot(aes(x=Model, y=MAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MAPE Graphs for All Data') + theme_bw()

ggsave(file=paste0(dr, '/', '1. MAPE_All.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 2 : sMAPE
df_period_4 %>% ggplot(aes(x=Model, y=sMAPE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAPE Graphs for All Data') + theme_bw()

ggsave(file=paste0(dr, '/', '2. sMAPE_All.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 3 : MASE
df_period_4 %>% ggplot(aes(x=Model, y=MASE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('MASE Graphs for All Data') + theme_bw()

ggsave(file=paste0(dr, '/', '3. MASE_All.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 4 : sMAE
df_period_4 %>% ggplot(aes(x=Model, y=sMAE)) + geom_bar(aes(fill=Model), stat='identity', alpha=0.7, width=0.5) +
  ggtitle('sMAE Graphs for All Data') + theme_bw()

ggsave(file=paste0(dr, '/', '4. sMAE_All.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)

# Plot 5 : All Error
df_period_4_all %>% ggplot(aes(x=Type, y=Error, fill=Model)) + geom_bar(stat='identity', position='dodge', alpha=0.7, width=0.5) +
  ggtitle('All Error Graphs for All Data') + theme_bw()

ggsave(file=paste0(dr, '/', '5. All Error_All.jpg'),
       width=24, height=20, units=c('cm'), dpi=300)
