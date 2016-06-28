#install.packages("devtools")
#install_github('quandl/R-package')

#Title : Gold price prediction using naive linear least square
rm(list=ls())
#setwd("/Users/ChangikChoi/Desktop/Gold Price Rcode/GoldPricePrediction")
load('WGC_GOLD_DAILY_USD.RData')
source('header.R')
source('MainProgram.R')


#data organizing
WGDU = WGC_GOLD_DAILY_USD[nrow(WGC_GOLD_DAILY_USD):1, ]
WGDU = WGDU[(which(WGDU[,1] == "2012-01-02")) : nrow(WGDU), ]
WGDU = as.matrix(WGDU)

#set up winLen, cash, gold balance (user input parameter)
winLen = 8
cashBalance = 1000000
goldBalance = 0
growthRate = 0.71 #in percentage
cashRate = 1
resultTable = MainProgram(WGDU, winLen, cashBalance, goldBalance, growthRate, cashRate)
graph = DrawCompareTable(resultTable[,2:3], winLen)
balance_result = resultTable[nrow(resultTable),5] + resultTable[nrow(resultTable),6] * resultTable[nrow(resultTable),2]


#result overview printing
print (paste("Investment starting :  ", format(cashBalance,scientific=FALSE)))
print (paste(paste(paste("investment time : ", resultTable[1,1]) , "to "),resultTable[nrow(resultTable),1]))
print (paste("Investment result : ", balance_result))
print (paste("Investment Profit : ", balance_result - cashBalance))


