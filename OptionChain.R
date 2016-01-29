# Get Option Chain from QuantMod
# 
# 
rm(list=ls())
library("quantmod")
# All expiries
AAPL.OPTS <- getOptionChain("AAPL", NULL)
# All 2015 and 2016 expiries
AAPL.2016 <- getOptionChain("AAPL", "2016")


do.call(rbind, lapply(AAPL.2016, function(x) do.call(rbind, x)))
