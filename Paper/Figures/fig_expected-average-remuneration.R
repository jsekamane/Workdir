####################
#
#	Title:  Expected Average Remuneration under the EEG and the ROC. (2002 - 2021)
# Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). 
# Data sources: EUROSTAT (sts_inpp_a). DE: BMU (EEG 2000, section 7. EEG 2004, article 10. EEG 2009, section 20, 29 and 30. EEG 2012, section 20, 29 and 30). Lauber and Mez (2004). Statistisches Bundesamt (WZ08-47).  LT: Marciukatis (2011). Baltpool (2012). UK: [See previous calculations]. Office for National Statistics (MM23)
#	Notes: Assumed fixed annual inflation rate from 2012 in all three countries of 1.96%.
# Units: p/KWh and c/KWh (in 2003-prices)
# Dependencies: fig_roc-price-compontents.R (ROC.Value.csv)
# Reverse dependencies: fig_anticipated-price-energy.R (DE.PMT.csv, LT.PMT.csv, UK.PMT.csv).
#
####################

#library(ggplot2)
#library(reshape2)

# LOAD THE DATA
data.ROC.Value = read.csv("ROC.Value.csv")

data.rpi.UK = read.csv("ons_mm23_rpi.csv") # index (January 1987=100)
data.rpi.DE = read.csv("destatis_WZ08-47_rpi.csv") # index (2005=100)
data.ppi = read.csv("eurostat-sts_inpp_a_producerpriceindex.csv") # index (2005=100)

# Parameters
Discount.rate = 0.08 # 8%
Exchange.rate.GBP = 1.5 # GBP/EUR
Exchange.rate.LTL = 63.7/220 # LTL/EUR. Calculated as 63.7EUR/220LTL. Calculated using the infomation from Marciukatis (2011)
Pool.price.UK = 3 # p/KWh. 2.79 in Butler and Neuhoff calculations, but they use it from 2025.
# Before the Ignalina NPP was shut down, there was not electricity market in LT, since Ignalina contributed to the majority of electricity production. And as a consequence the "price" paid to nuclear has been very stable for 10 year, LTL 6.58 (2008), 6.11 (2009) ≈ 1.7. (NCC, 2009 and XXXX 20xx)
# We make the assumption that:
# - A fixed pool price for all years, correspond the the average of the last three years market data.
# - BALTPOOL: Last three year average (2010-2012): 157.36 LTL/MWh  ≈ 4.54 c/KWh.
# - Project developers belevied the the goverments commitment to close down the nuclear power plant no later than 31. december 2009 (European Commission 2008). And that they therefore anticipate the higher market pool price.
# - Similarly we assume that the market price also take the long term MC ??? into account, and any (evt.) newly constructed nuclear power will not increase the pool price / make it jump.
Pool.price.LT = 157.36 *Exchange.rate.LTL/10 # c/KWh.
Future.inflation.rate = 0.0196 # 1.96% annually. From Butler and Neuhoff calculations. Assumption: ECB target of just below 2%. And identical in all three countries.

# Time range for build year
years = c(2002:2021)
years.DE = c(2000:2021)

# Creating empty components that we will fill content into later
DE.FIT.initial.fee = NA
DE.FIT.basic.fee = NA
DE.FIT.degression.rate = NA
LT.FIT.initial.fee = NA
# Cash flow:
# x-axis: build year,     y-axis: years after build.
# x-axis: year 2002-2021, y-axis: the 20 year investment evaluation perio d.
DE.cash.flow = matrix(NA, ncol = length(years.DE), nrow = 20) # c/KWh. Start from 2000 - and afterwards we export and then remove extra years.
LT.cash.flow = matrix(NA, ncol = length(years), nrow = 20) # c/KWh
UK.cash.flow = matrix(NA, ncol = length(years), nrow = 20) # p/KWh
colnames(DE.cash.flow) = years.DE
colnames(LT.cash.flow) = years
colnames(UK.cash.flow) = years


#####
# Fixing base years of the indecies
#####

# We deflate each value in the cash flow, to the price index in the corresponding year. For years after 2011 we assume the same fixed inflation rate for all countries (1.96%)

# This function changes the base of of an index. Required: the new base year, list of years and corresponding list of index values (as old base year).
SET.BASE.YEAR = function(base.year, list.years, list.index){
  i = match(base.year, list.years) # finds the row number that corresponds to the base.year
  df = cbind(list.years, lapply(list.index, function(x){x/list.index[i]*100}) ) # recaulculating index values
  #colnames(df) = c("Years", paste("Index (",base.year,"=100)", sep = "") )
  colnames(df) = c("Years", "Index")
  df # return data frame with years and index.
}

UK.2003.rpi = SET.BASE.YEAR(2003, data.rpi.UK$X, data.rpi.UK$CHAW)
DE.2003.rpi = SET.BASE.YEAR(2003, data.rpi.DE$X, data.rpi.DE[,2])
LT.2003.ppi = SET.BASE.YEAR(2003, data.ppi$X, data.ppi$Lithuania)

# Extending the price indecies from 2012-2040, using the fixed inflation rates.

UK.2003.rpi.extended = matrix(NA, ncol = 2, nrow = (nrow(UK.2003.rpi)+(2040-2011)) )
colnames(UK.2003.rpi.extended) = c("Years", "Index")
for(i in 1:nrow(UK.2003.rpi.extended)) {
  if(i <= nrow(UK.2003.rpi)) {
    UK.2003.rpi.extended[,1][[i]] = UK.2003.rpi[,1][[i]]
    UK.2003.rpi.extended[,2][[i]] = UK.2003.rpi[,2][[i]]
  } else {
    UK.2003.rpi.extended[,1][[i]] = UK.2003.rpi.extended[,1][[i-1]] + 1
    UK.2003.rpi.extended[,2][[i]] = UK.2003.rpi.extended[,2][[i-1]] * (1+Future.inflation.rate)
  }
}

DE.2003.rpi.extended = matrix(NA, ncol = 2, nrow = (nrow(DE.2003.rpi)+(2040-2011)) )
colnames(DE.2003.rpi.extended) = c("Years", "Index")
for(i in 1:nrow(DE.2003.rpi.extended)) {
  if(i <= nrow(DE.2003.rpi)) {
    DE.2003.rpi.extended[,1][[i]] = DE.2003.rpi[,1][[i]]
    DE.2003.rpi.extended[,2][[i]] = DE.2003.rpi[,2][[i]]
  } else {
    DE.2003.rpi.extended[,1][[i]] = DE.2003.rpi.extended[,1][[i-1]] + 1
    DE.2003.rpi.extended[,2][[i]] = DE.2003.rpi.extended[,2][[i-1]] * (1+Future.inflation.rate)
  }
}

LT.2003.ppi.extended = matrix(NA, ncol = 2, nrow = (nrow(LT.2003.ppi)+(2040-2011)) )
colnames(LT.2003.ppi.extended) = c("Years", "Index")
for(i in 1:nrow(LT.2003.ppi.extended)) {
  if(i <= nrow(LT.2003.ppi)) {
    LT.2003.ppi.extended[,1][[i]] = LT.2003.ppi[,1][[i]]
    LT.2003.ppi.extended[,2][[i]] = LT.2003.ppi[,2][[i]]
  } else {
    LT.2003.ppi.extended[,1][[i]] = LT.2003.ppi.extended[,1][[i-1]] + 1
    LT.2003.ppi.extended[,2][[i]] = LT.2003.ppi.extended[,2][[i-1]] * (1+Future.inflation.rate)
  }
}


#####
# Germany cash flow
#####

# For each build year the EEG gives an intial fee (for the first 5 years) and a basic fee (for the following 15 years).
# These rates are degressed from build year to build year, corresponding to the degression rate specified in the EEG.
for(i in 1:20) { # for each year after build (row)
  # Start from 2000 - and afterwards we export and then remove extra years.
  for(j in 1:length(years.DE)) { # for each build year (coloumn)

    # EEG 2000 (2000-2004)
    if(j <= 3) {
      # Rates for year 2000
      DE.FIT.initial.fee = 9.1 # c/KWh
      DE.FIT.basic.fee = 6.17 # c/KWh
      DE.FIT.degression.rate = 0.015 # 1.5%
      # Degression from 2002.
      if(j == 3) {
        DE.FIT.initial.fee = DE.FIT.initial.fee * (1-DE.FIT.degression.rate)
        DE.FIT.basic.fee = DE.FIT.basic.fee * (1-DE.FIT.degression.rate)
      }
    # EEG 2004 (2005-2008)
    } else if(j == 6) {
      # Rates for year 2004
      DE.FIT.initial.fee = 8.7 # c/KWh
      DE.FIT.basic.fee = 5.5 # c/KWh
      DE.FIT.degression.rate = 0.02 # 2%
      # Start from 2005: 
      DE.FIT.initial.fee = DE.FIT.initial.fee * (1-DE.FIT.degression.rate)
      DE.FIT.basic.fee = DE.FIT.basic.fee * (1-DE.FIT.degression.rate)
      
    # EEG 2009 (2009-20011)
    } else if(j == 10) { 
      # Rates for year 2009
      DE.FIT.initial.fee = 9.2 # c/KWh
      DE.FIT.basic.fee = 5.02 # c/KWh
      DE.FIT.degression.rate = 0.01 # 1%
      
    # EEG 2012 (2012-)
    } else if(j == 13) { 
      # Rates for year 2012
      DE.FIT.initial.fee = 8.93 # c/KWh
      DE.FIT.basic.fee = 4.87 # c/KWh
      DE.FIT.degression.rate = 0.015 # 1.5%
      
    # Degress fees according to degression rates
    } else {
      DE.FIT.initial.fee = DE.FIT.initial.fee * (1-DE.FIT.degression.rate)
      DE.FIT.basic.fee = DE.FIT.basic.fee * (1-DE.FIT.degression.rate)
    }
    
    # First 5 years: initial fee. Thereafter: basic fee.
    if(i <= 5) {
      DE.cash.flow[i,j] = DE.FIT.initial.fee
    } else {
      DE.cash.flow[i,j] = DE.FIT.basic.fee
    }
  }
}

# Round numbers:
# Degression rates should be calculated on the unrounded numbers, but developers will pay using rounded numbers. 
for(i in 1:20) { # for each year after build (row)
  for(j in 1:length(years.DE)) { # for each build year (coloumn)
    
    # Round to 1 digit in EEG 2000, and 2 digit in all following EEGs.
    if(j > 2 && j < 6 ) {
      DE.cash.flow[i,j] = round(DE.cash.flow[i,j], digits = 1)
    } else {
      DE.cash.flow[i,j] = round(DE.cash.flow[i,j], digits = 2)
    }
    
  }
}

# Price adjust:
for(i in 1:20) { # for each year after build (row)
  for(j in 1:length(years.DE)) { # for each build year (coloumn)
    DE.cash.flow[i,j] = DE.cash.flow[i,j] / DE.2003.rpi.extended[,2][((i-1)+j+9)] * 100
  }
}

#####
# Lithuania cash flow
#####

# For each build year the LT FIT system gives fee (for the first 12 years) and thereafter developers recive the pool price.
for(i in 1:20) { # for each year after build (row)
  for(j in 1:length(years)) { # for each build year (coloumn)
    
    # 2002-2008
    if(j == 1) { LT.FIT.initial.fee = 220 *Exchange.rate.LTL/10 # c/KWh
      
    # 2009-2012
    } else if(j == 8) { LT.FIT.initial.fee = 300 *Exchange.rate.LTL/10 # c/KWh
      
    # 2012-
    # What assumptions should be make ?? Simple average of the three sub-band rates or ???
    } else if(j == 11) { # LT.FIT.initial.fee =
      # simple average of data from http://www.regula.lt in LTL, so also applying exchange rate to get it in euro-cent
      LT.FIT.initial.fee = mean(c(370, 360, 280)) *Exchange.rate.LTL/10
    }
    
    # First 12 years: initial fee. Thereafter: pool price.
    if(i <= 12) {
      LT.cash.flow[i,j] = LT.FIT.initial.fee
    } else {
      LT.cash.flow[i,j] = Pool.price.LT
    }
    
  }
}

# Price adjust:
for(i in 1:20) { # for each year after build (row)
  for(j in 1:length(years)) { # for each build year (coloumn)
    LT.cash.flow[i,j]  = LT.cash.flow[i,j] / LT.2003.ppi.extended[,2][((i-1)+j+7)] * 100
  }
}

#####
# UK cash flow
#####

# Using the total ROC value, that we calculated in fig_roc-price-compontents.R, we fill in each year with its respective price.
# For years after 2021 where we have not calculated ROC value:
# We will assume fixed price of 3 similar to (but not mentioned in) Butler and Neuhoff (2008). The number 3 has been found by method of trial-and-error.
for(i in 1:20) { # for each year after build (row)
  for(j in 1:length(years)) { # for each build year (coloumn)
    if( ((i-1)+j) <= length(data.ROC.Value$Total) ) { # if we have calculated the ROC value for the specific year
      UK.cash.flow[i,j] = data.ROC.Value$Total[[((i-1)+j)]] # Total ROC value p/KWH
    } else {
      UK.cash.flow[i,j] = Pool.price.UK # 3 p/KWh
    }
  }
}

# Price adjust:
#a = c("START:")
for(i in 1:20) { # for each year after build (row)
  for(j in 1:length(years)) { # for each build year (coloumn)
    #a = append(a, c(UK.2003.rpi.extended[,1][((i-1)+j+15)]))
    UK.cash.flow[i,j] = UK.cash.flow[i,j] / UK.2003.rpi.extended[,2][((i-1)+j+15)] * 100
  }
  #a = append(a, "###")
}
#a # prints the years from the price index that correspond to the years in the cash flow. Just to check whether we are using the correct offset.


#####
# Calculating Net Present Value and correspoding Payment
#####

NPV = function(i, N) {
  SUM = 0
  for(t in 1:length(N)) {
    SUM = SUM + N[[t]]/((1+i)^t)
  }
  SUM # returns the SUM
}

PMT = function(i, n, PV) {
  PV / ( (1 - (1/(1 + i)^n)) / i ) # Returns payment
}  

# calculating NPV for each year
DE.NPV = sapply(colnames(DE.cash.flow), function(x){NPV(Discount.rate, DE.cash.flow[, x])} ) 
LT.NPV = sapply(colnames(LT.cash.flow), function(x){NPV(Discount.rate, LT.cash.flow[, x])} )
UK.NPV = sapply(colnames(UK.cash.flow), function(x){NPV(Discount.rate, UK.cash.flow[, x])} )

# calculating Payment for each year
DE.PMT = lapply(DE.NPV, function(x){PMT(Discount.rate, 20, x)} ) 
LT.PMT = lapply(LT.NPV, function(x){PMT(Discount.rate, 20, x)} )
UK.PMT = lapply(UK.NPV, function(x){PMT(Discount.rate, 20, x)} )

# save for export and remove years 2000 and 2001
DE.PMT.short = DE.PMT[3:22]

#####
# Plotting
#####

# German rates in pounds instead of euro
DE.PMT.POUNDS = lapply(DE.PMT.short, function(x){ x/Exchange.rate.GBP } )

pdf(file="fig_expected-average-remuneration-priceadjusted.pdf", height=2.5, width=5)

par(mar=c(2,2.5,0,2.6)+.3, yaxs='i') # margin, and y-axis start at y=0
plot(x=years, y=UK.PMT, axes=FALSE, col="gray", type="h", lwd=10, lend="square", xlab="", ylab="", cex.lab=0.7, ylim=range(0,10))
abline(h=c(0,2,4,6,8,10), col="#D9D9D9", lwd=0.3)
mtext("p/KWh",side=2, line=2, cex=0.7)
axis(1, labels=FALSE, col="#989898")
axis(1, at=years, cex.axis=0.5, col="#989898")
axis(2, cex.axis=0.5, col="#989898")
points(x=years, y=DE.PMT.POUNDS, pch=4)
par(new=TRUE) # plot the following using the secondary axis: 
plot(x=years, y=DE.PMT.short, axes=FALSE, type="l", xaxt="n", yaxt="n", xlab="", ylab="", ylim=range(0,10*Exchange.rate.GBP))
box(col="#989898")
lines(x=years, y=LT.PMT, col="#FF00FF", lwd=2)
axis(4, cex.axis=0.5, col="#989898") # add secondary axis
mtext("c/KWh",side=4, line=2, cex=0.7)
legend("topright", legend=c("UK (p/kWh)","Germany (p/kWh)", "Germany (c/kWh)", "Lithuania (c/kWh)"), col=c("grey","black", "black","#FF00FF"), lty = c(NA, NA,"solid","solid"), lwd = c(NA, NA, 1, 2), pch = c(15, 4, NA, NA), cex=0.7, bty="n")

dev.off()

#####
# Export
#####

# Save to CSV
write.csv(cbind(DE.PMT), "DE.PMT.csv", row.names=TRUE)
write.csv(cbind(LT.PMT), "LT.PMT.csv", row.names=TRUE)
write.csv(cbind(UK.PMT), "UK.PMT.csv", row.names=TRUE)