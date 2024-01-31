# Stat_Testing(15-16)
setwd('/Users/danilavtonoskin/Desktop/Study/3 Semester/Empirical finance/HW/Codes')
getwd()
# Loading libraries
library("readxl")
library(dplyr)
library(stats)
library(tidyverse)
library(quantmod)
library(xts)
library(ggplot2)
# load data
stat_testing = read.csv('EMP_Finance_15-16_Avtonoshkin.csv', header = TRUE, sep = ';', dec = ',')
SP500 = read.csv('EMP_Finance_15-16_SP500_Avtonoshkin.csv', header = TRUE, sep = ';', dec = ',')
#summary(portfolio)
stat_testing$Date = as.Date(stat_testing$Date, format = '%Y-%m-%d')
stat_testing.xts <- xts(stat_testing[,2:16], order.by = stat_testing$Date)
# no NAs
which (is.na(stat_testing))
# training period
stat_testing
training_period <- stat_testing %>%
  filter(Date < '2011-01-04')
tail(training_period)
# out-of-sample period
out_of_sample <- stat_testing %>%
  filter(Date >= '2011-01-04')
tail(out_of_sample)
# check PCA eligibility
cor(stat_testing.xts)
mean(cor(stat_testing.xts))
# mean corr > 0.3 so eligible
#View(training_period)
# important libraries
install.packages('MASS')
install.packages('factoextra')
library(MASS)
library(factoextra)
# run PCA on training period data
training_period_pca <- prcomp(training_period[,2:16], scale = T)
summary(training_period_pca)
#str(training_period)
# Elements of PCA
names(training_period_pca)
# stdv of components
training_period_pca$sdev
#  Eigenvectors
training_period_pca$rotation
# STD and mean of variables
training_period_pca$center
training_period_pca$scale
# Principal component Scores
training_period_pca$x
# Scree Plot of variance
fviz_eig(training_period_pca, addlabels = T, ylim = c(0,70))
# so we gonna stick to 4 factors
# Biplot with labeled Variables
fviz_pca_biplot(training_period_pca, label = 'var')
# perform a PCA with 4 factors and varimax rotation
install.packages('psych')
library(psych)
training_period_pca_4f <- principal(training_period[,2:16], rotate = 'varimax',
                                           nfactors = 4)
?principal
training_period_pca_4f$loadings
training_period_pca_4f
# findings weights
RC1 <-c(0.77,0.21,0.16,0.30,0.11,0.28,0.16,0.04,0.67,0.10,0.09,0.80,0.15,0.79,0.21)
RC2 <-c(0.22,0.51,0.60,0.16,0.76,0.21,0.18,0.67,0.08,0.75,0.39,0.14,0.10,0.10,0.19)
RC3 <-c(0.17,0.23,0.19,0.13,0.09,0.79,0.88,0.12,0.13,0.07,0.09,0.16,0.19,0.18,0.85)
RC4 <-c(0.11,0.20,0.10,0.62,0.11,0.18,0.14,0.21,0.16,0.05,0.55,0.13,0.75,0.17,0.82)
RC <- cbind(RC1, RC2, RC3, RC4)
rownames(RC) <- names(training_period[2:16])
RC <- as.data.frame(RC)

# finding weights
RC <- RC %>%
  mutate(RC1 = RC1/sum(RC1),
         RC2 = RC2/sum(RC2),
         RC3 = RC3/sum(RC3),
         RC4 = RC4/sum(RC4))
RC <- RC %>%
  mutate(RC1 = round(RC1,2),
         RC2 = round(RC2,2),
         RC3 = round(RC3,2),
         RC4 = round(RC4,2))
RC
# leave only >5%
RC <-RC %>%
  mutate(ifelse(RC1 > 0.05, RC1, 0),
         ifelse(RC2 > 0.05, RC2,0),
         ifelse(RC3 > 0.05, RC3,0),
         ifelse(RC4>0.05, RC4,0))
RC <- RC[,5:8]
names(RC) <- c('RC1', 'RC2', 'RC3', 'RC4')
# reweighting 
RC <- RC %>%
  mutate(RC1 = RC1/sum(RC1),
         RC2 = RC2/sum(RC2),
         RC3 = RC3/sum(RC3),
         RC4 = RC4/sum(RC4))
RC <- RC %>%
  mutate(RC1 = round(RC1,2),
         RC2 = round(RC2,2),
         RC3 = round(RC3,2),
         RC4 = round(RC4,2))
# cumulative the out of sample cumulative returns for each portfolio
# first portfolio
head(out_of_sample)
RC
pf1 <- out_of_sample
for(i in 1:length(names(pf1)[2:16])){
  pf1[,i+1] = pf1[,i+1] * RC$RC1[i] 
}
pf1 <- pf1 %>%
  mutate(Port_Ret = rowSums(pf1[,2:16]))
pf1 <- pf1 %>%
  mutate(Cum_Port_Ret = cumsum(pf1$Port_Ret))

# second portfolio
pf2<- out_of_sample
for(i in 1:length(names(pf2)[2:16])){
  pf2[,i+1] = pf2[,i+1] * RC$RC2[i] 
}
pf2 <- pf2 %>%
  mutate(Port_Ret = rowSums(pf2[,2:16]))
pf2 <- pf2 %>%
  mutate(Cum_Port_Ret = cumsum(pf2$Port_Ret))
# third portfolio
pf3<- out_of_sample
for(i in 1:length(names(pf3)[2:16])){
  pf3[,i+1] = pf3[,i+1] * RC$RC3[i] 
}
pf3 <- pf3 %>%
  mutate(Port_Ret = rowSums(pf3[,2:16]))
pf3 <- pf3 %>%
  mutate(Cum_Port_Ret = cumsum(pf3$Port_Ret))
pf3
head(pf3,10)
# fourth portfolio
pf4<- out_of_sample
for(i in 1:length(names(pf4)[2:16])){
  pf4[,i+1] = pf4[,i+1] * RC$RC4[i] 
}
pf4 <- pf4 %>%
  mutate(Port_Ret = rowSums(pf4[,2:16]))
pf4 <- pf4 %>%
  mutate(Cum_Port_Ret = cumsum(pf4$Port_Ret))
pf4
head(pf4,10)
# sum of the portfolios (equal weights)
equal_weight_portfolio <- pf1[,c('Date', 'Cum_Port_Ret')]
head(equal_weight_portfolio)
names(equal_weight_portfolio)[2] <- c('Cum_Port_Ret_1')
equal_weight_portfolio <- equal_weight_portfolio %>%
  mutate(Cum_Port_Ret_2 = pf2$Cum_Port_Ret,
         Cum_Port_Ret_3 = pf3$Cum_Port_Ret,
         Cum_Port_Ret_4 = pf4$Cum_Port_Ret)
# find a weighted cumulative return 25% each
equal_weight_portfolio <- equal_weight_portfolio %>%
  mutate(Weighted_Cum_Return = rowSums(equal_weight_portfolio[,2:5])/4)
equal_weight_portfolio
# finally S&P 500
SP500$Date = as.Date(SP500$Date, format = '%Y-%m-%d')
SP500.xts <- xts(SP500[,2:3], order.by = SP500$Date)
# build a graph
comparison <- data.frame()

comparison <- cbind(SP500$Date,pf1$Cum_Port_Ret, pf2$Cum_Port_Ret,
                    pf3$Cum_Port_Ret, pf4$Cum_Port_Ret,
                    equal_weight_portfolio$Weighted_Cum_Return,
                    SP500$Cum_Return)
colnames(comparison) <- c('Date', 'P1', 'P2', 'P3', 'P4',
                      'Mix', 'S&P500')
comparison <- data.frame(comparison)
comparison$Date <- as.Date(comparison$Date, format = '%Y-%m-%d')
head(comparison)
# library
graph <- ggplot(comparison, aes(Date)) + geom_line(aes(Date,P1, colour = 'P1')) +
  geom_line(aes(Date,P2, colour = 'P2')) + geom_line(aes(Date,P3, colour = 'P3')) +
  geom_line(aes(Date,P4, colour = 'P4')) + geom_line(aes(Date,Mix, colour = 'Mix')) + 
  geom_line(aes(Date,S.P500, colour = 'SP500')) + ylab('Accumulated_Returns')
                                                        
graph

  


