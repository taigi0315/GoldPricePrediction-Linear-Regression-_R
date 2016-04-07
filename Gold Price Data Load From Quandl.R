#install.packages("devtools")
#install_github('quandl/R-package')

library(devtools)
library(Quandl)
Quandl.api_key("b8FKnSAiCQ7okzdFsqgz")

WGC_GOLD_DAILY_USD=Quandl("WGC/GOLD_DAILY_USD")
save(WGC_GOLD_DAILY_USD,file='WGC_GOLD_DAILY_USD.RData')
