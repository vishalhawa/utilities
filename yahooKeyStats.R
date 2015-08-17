#######################################################################
##To download all key stats using XML and x_path – PREFERRED WAY
#######################################################################

require(XML)
require(plyr)
getKeyStats <- function(symbol) {
 # print(symbol)
  yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
  html_text <- htmlParse(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")

  #search for <td> nodes anywhere that have class ‘yfnc_tablehead1′
  nodes <- getNodeSet(html_text, "/*//td[@class='yfnc_tablehead1']")
  
  if(length(nodes) > 0 ) {
    measures <- sapply(nodes, xmlValue)
    
    #Clean up the column name
    measures <- gsub( "*[0-9]*:", "", gsub(" \\(.*?\\)[0-9]*:","", measures))   
    
    #Remove dups
    dups <- which(duplicated(measures))
    #print(dups) 
    for(i in 1:length(dups)) 
      measures[dups[i]] = paste(measures[dups[i]], i, sep=" ")
    
    #use siblings function to get value
    values <- sapply(nodes, function(x)  xmlValue(getSibling(x)))
    
    df <- data.frame(t(values),"symbol"=symbol)
    colnames(df) <- c(measures,"symbol")
    return(df)
  }  #if 
}


library(RODBC)
ch <- odbcConnect("portfolio")
res <- sqlFetch(ch, "USStocks") #Fetch Query results to DF
#res2 <- sqlQuery(ch, paste("SELECT symbol, name FROM USStocks"))
tickers <- res[,"symbol"]
#tickers <- c("SAP","AA","AAPL")
stats <- (ldply(tickers, getKeyStats))
#stats$symbol <- tickers
stats$LastUpdated <- format(Sys.time(), "%m/%d/%Y")
sqlUpdate(ch, dat=as.data.frame(stats), tablename = "USStocksKS",index=c("symbol"))
#sqlSave(ch, dat=as.data.frame(stats), tablename = "USStocksKS",append = TRUE)

close(ch)



#write.csv(stats, "FinancialStats_updated.csv",row.names=TRUE)  

# -----------------EOF ----------------------------