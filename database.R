
 
library(RODBC)

ch <- odbcConnect("portfolio")
 
res <- sqlFetch(ch, "US Executed Yr 2010") #Fetch Query results to D

sqlQuery(ch, paste("SELECT symbol, name FROM USStocks"))


close(ch)
 

#  # Connecting Excel files
# 
# require(RODBC)
# conn = odbcConnectExcel("C:\\Users\\vhawa\\Dropbox\\Projects\\FirmFinancials\\Finance\\Finance-GSI.xlsx") # open a connection to the Excel file
# sqlTables(conn)$TABLE_NAME # show all sheets
# df = sqlFetch(conn, "Sheet1") # read a sheet
# df = sqlQuery(conn, "select * from [Sheet1 $]") # read a sheet (alternative SQL sintax)
# close(conn) # close the connection to the file
# 
# 
# require(xlsx)
# USPortfolio = read.xlsx("C:\\Users\\vhawa\\Dropbox\\Projects\\FirmFinancials\\Finance\\Finance-GSI.xlsx", sheetName = "USPortfolio")
# read.xlsx2("myfile.xlsx", sheetName = "Sheet1")
# 
# 
# 
# source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")
# xlsxToR = function("C:\\Users\\vhawa\\Dropbox\\Projects\\FirmFinancials\\Finance\\Finance-GSI.xlsx", header = TRUE)
#   