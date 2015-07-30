#######################################################################
##To download all key stats using XML and x_path – PREFERRED WAY
#######################################################################

require(XML)
require(plyr)
getKeyStats <- function(symbol) {
 # symbol="SAP"
  yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
  html_text <- htmlParse(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")
  
  #search for <td> nodes anywhere that have class ‘yfnc_tablehead1′
  nodes <- getNodeSet(html_text, "/*//td[@class='yfnc_tablehead1′]")
  
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
    
    df <- data.frame(t(values))
    colnames(df) <- measures
    return(df)
  } 
}

tickers <- c("AAPL","SAP","PFE")
stats <- ldply(tickers, getKeyStats)
rownames(stats) <- tickers
write.csv(t(stats), "FinancialStats_updated.csv",row.names=TRUE)  

# -----------------EOF ----------------------------