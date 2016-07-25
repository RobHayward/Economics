#Code to re-create John Hussman's Recession warning index
#http://www.hussmanfunds.com/wmc/wmc110801.htm
#R code by Zach Mayer

rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
library(quantmod)

#################################################
# 1. Credit spreads
#################################################

getSymbols('CPF3M',src='FRED') #3-Month Financial Commercial Paper 
getSymbols('GS3M',src='FRED') #3-Month Treasury
CS <- na.omit(CPF3M-GS3M)

#6 month increase
CS <- na.omit(CS-Lag(CS,6))
names(CS) <- 'CS'

#################################################
# 2. Stock Prices
#################################################
getSymbols('SP500',src='FRED')
SP500 <- Cl(to.monthly(SP500))

#Re-index to start of month
library(lubridate)
index(SP500) <- as.Date(ISOdate(year(index(SP500)),month(index(SP500)),1))

#6 month increase
SP500 <- na.omit(SP500-Lag(SP500,6))
names(SP500) <- 'SP500'

#################################################
# 3. ISM Purchasing Managers index
#################################################

#A. PMI
getSymbols('NAPM',src='FRED') #Non-farm emploment
PMI <- NAPM
names(PMI) <- 'PMI'

#B. Employment
getSymbols('PAYEMS',src='FRED') #Non-farm emploment
PAYEMS <- na.omit((PAYEMS-Lag(PAYEMS,12))/Lag(PAYEMS,12)) #12 month increase
names(PAYEMS) <- 'PAYEMS'

#################################################
# 4. Yield Curve
#################################################
getSymbols('GS10',src='FRED') #10-year Treasury
YC <- na.omit(GS10-GS3M)
names(YC) <- 'YC'

#################################################
# Put it all together
#################################################

P.A <-(CS>0) & #1. Credit spreads widening over 6 months
  (SP500<0) &	#2. Stocks falling over 6 months
  (PMI<50) &		#3. PMI below 50
  (YC<2.5)		#4. 10 year vs 3 year yields below 2.5%
P.B <- (CS>0) & #1. Credit spreads widening over 6 months
  (SP500<0) &	#2. Stocks falling over 6 months
  (PMI<54) &		#3. PMI below 54
  (PAYEMS<1.3) &	#3.B 1Y employment growth below 1.3%
  (YC<3.1) 		#4. 10 year vs 3 year yields below 2.5%

P.Rec <- P.A | P.B
names(P.Rec) <- 'P.Rec'
P.Rec$P.Rec <- as.numeric(P.Rec$P.Rec)

#Actual Recessions
getSymbols('USREC',src='FRED') 
chartSeries(P.Rec)
chartSeries(USREC)

#Compare
ReccessionForecast <- na.omit(cbind(P.Rec,USREC))
start <- min(index(ReccessionForecast))
ReccessionForecast <- ts(ReccessionForecast,frequency=12,start=c(year(start),month(start)))
plot(ReccessionForecast)
