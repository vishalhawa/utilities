

source('https://github.com/vishalhawa/utilities/raw/master/util.R')

lookback = 10
idxrange = 20
asset = c("baba","pfe")

endDate = Sys.Date()
startDate = endDate -2*idxrange- 2*(lookback)

st = mapply(SIMPLIFY=FALSE ,as.character(asset)  ,FUN=function(x)tryCatch(getStockHist(stk=x,sdate=startDate,edate=endDate,pricetype="Adj.Close") ,error=function(e)return(NA)))

stkval= do.call("cbind",st)
asset.vec.rv = names(st)
stkval= stkval[order(stkval[1],decreasing=T),]


rollsd = sapply(c(1:idxrange),function(x){apply(stkval[x:(x+lookback-1),paste0(asset.vec.rv,".price")], 2, sd)})


rollmean = (sapply(c(1:idxrange),function(x){apply(stkval[x:(x+lookback-1),paste0(asset.vec.rv,".price")], 2, mean)}))

stkprices = as.matrix(sapply(c(1:idxrange),function(x){stkval[x,paste0(asset.vec.rv,".price")]}))