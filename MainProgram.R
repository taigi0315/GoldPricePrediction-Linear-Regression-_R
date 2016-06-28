MainProgram <- function(WGDU, winLen, cashBalance, goldBalance, growthRate, cashRate){
  #set up result table
  resultTable = data.frame(matrix(0, ncol=10, nrow= nrow(WGDU)))
  colnames(resultTable) <- c("Date","Real Price", "Next Predict Price", "Expecting Growth", "Pre-Cash-Bal", "Pre-Gold-Bal", "Buy", "Sell", "After-Cash-Bal", "After-Gold-Bal")
  resultTable[1:winLen,c(5,9)] = cashBalance
  resultTable[1:winLen,c(6,10)] = goldBalance
  
  #fill up result table
  resultTable[,1] = WGDU[,1]
  resultTable[,2] = WGDU[,2]
  resultTable[,2] <- as.numeric(resultTable[,2])
  resultTable[1:(winLen)-1,3] = resultTable[1:(winLen)-1,2] 
  for(i in winLen:nrow(resultTable)){
    prices = as.numeric(resultTable[(i-winLen+1):i,2])
    resultTable[i,3] = GetPrediction(prices, winLen)
    resultTable[i,4] = CalculateGrowth(resultTable[i,2],resultTable[i,3])
    resultTable[i,7] = BuyDecider(resultTable[i,2],resultTable[i,4],cashBalance,growthRate,cashRate)
    resultTable[i,8] = SellDecider(resultTable[i,2],resultTable[i,4],growthRate,goldBalance)
    cashBalance = CashBalanceTracker(resultTable[i,2], cashBalance, resultTable[i,7], resultTable[i,8])
    goldBalance = GoldBalanceTracker(goldBalance, resultTable[i,7], resultTable[i,8])
    resultTable[i,9] = cashBalance
    resultTable[i,10] = goldBalance
    if(i < nrow(resultTable)){
      resultTable[(i+1),5] = cashBalance
      resultTable[(i+1),6] = goldBalance
    }
  }
  #return(cashBalance + (resultTable[nrow(resultTable),2] * goldBalance) )
  return (resultTable)
}