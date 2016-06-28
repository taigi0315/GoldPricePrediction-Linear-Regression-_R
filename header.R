#load data
#install.packages("devtools")
#install_github('quandl/R-package')
library(devtools)
library(Quandl)
#WGC_GOLD_DAILY_USD=Quandl("WGC/GOLD_DAILY_USD")
#save(WGC_GOLD_DAILY_USD,file='WGC_GOLD_DAILY_USD.RData')

#Calculate prediction 
GetPrediction <- function(prices, winLen){
  #parameter : price, window length
  windowMat = matrix(1, ncol = 2, nrow = winLen )
  windowMat[,1] = 1:winLen
  #windowMat = A, price = b in Least Square
  C = t(windowMat) %*% windowMat
  #calculating x
  x = solve(C) %*% t(windowMat) %*% prices
  #prediction
  y = windowMat %*% x
  prediction =   as.vector(x)  %*% c((winLen+1),1)
  return (prediction)
}

LinearRegression <- function(x,y,x_test)
{
  x=cbind(x,matrix(1,nrow=length(x),ncol=1))
  a=solve(t(x) %*% x) %*% (  t(x) %*% y ) 
  y_pred=x_test%*%a;
  return (y_pred)
}
#Get predictions for each days
PredictionTable <- function(WGDU, winLen){
  predictTable = matrix(0, nrow=nrow(WGDU), ncol=2)
  colnames(predictTable) = c("realPrice", "predictPrice")
  predictTable[1:winLen,2] = WGDU[1:winLen,2]
  predictTable[,1] = WGDU[,2]
  for(i in winLen:(nrow(WGDU)-1)){
    prices = WGDU[(i-winLen+1):i,2]
    predictPrice = GetPrediction(prices, winLen)
    predictTable[(i+1),2] = predictPrice
  }
  return (predictTable)
}
#calculate growth rate, return 0 if needed
CalculateGrowth <- function(oriPrice, prePrice){
  diff = (prePrice - oriPrice)
  if(diff == 0){
    return(0)
  }
  else{
    growth = (diff / oriPrice) * 100
    return (round(growth, 3))
  }
}

BuyDecider <- function(goldPrice, expectGrowth, cashBalance, growthRate, cashRate){
  if(expectGrowth < growthRate){
    #predicting price is decreasing sell
    return (0)
  }
  else{
    #predicting price is increasing buy
    budget = (cashBalance * cashRate)
    maxAmount = 0
    while(budget > goldPrice){
      budget = (budget - goldPrice)
      maxAmount = maxAmount + 1
    }
    return (maxAmount)
  }
}

SellDecider <- function(goldPrice, expectGrowth, growthRate, goldBalance){
  if(expectGrowth > growthRate){
    #predicting price is increasing, buy
    return (0)
  }
  else if(expectGrowth < 0){
    #predicting price is decreasing, sell
    #for now simply just sell all gold we have
    moneyEarn = goldPrice * goldBalance
    return (goldBalance)
  }
  else{
    return (0)
  }
}

CashBalanceTracker <- function(goldPrice, cashBalance, buy, sell){
  afterCash = cashBalance - (goldPrice*buy) + (goldPrice*sell)
  return (afterCash)
}

GoldBalanceTracker <- function(goldBalance, buy, sell){
  afterGold = goldBalance + buy - sell
  return (afterGold)
}

#draw two lines plot
DrawCompareTable <- function(compareTable, winLen){
  x = c((winLen):nrow(WGDU))
  plot( x, compareTable[winLen:nrow(compareTable),1], type="l", col="red", xlab = "Date", ylab = "Gold Price")
  par(new=TRUE)
  plot( x, compareTable[(winLen-1):(nrow(compareTable)-1),2], type="l", col="blue", xlab = "Date", ylab = "Gold Price")
  legend("topright", legend = c("real price", "prediction"), col=c("red", "blue"), cex=.75, lty=1:3)
  
  dev.copy(jpeg,filename="compareGraph.jpg");
  dev.off ();
}
