####################
#
#	Title:  Anticipated price of wind energy (1990 - 2010)
#	Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). EUROSTAT (nrg_113a, nrg_1072). [Previous calculations]
#	Notes: 
# Units: p/KWh and c/KWh (in 2003 price)
# Dependencies: fig_expected-average-remuneration.R (DE.PMT.csv, LT.PMT.csv, UK.PMT.csv).
#
####################

#library(ggplot2)
#library(reshape2)

# LOAD THE DATA
data.DE.PMT = read.csv("DE.PMT.csv") # in c/KWh
data.LT.PMT = read.csv("LT.PMT.csv") # in c/KWh
data.UK.PMT = read.csv("UK.PMT.csv") # in p/KWh

# Use lagged values here:
data.capacity = read.csv("eurostat-nrg_113a_capacity-wind-MW.csv")
data.production = read.csv("eurostat-nrg_1072_production-wind-GWh.csv") # this is in GWh so late we have to multiply with 1000 to get it in MWh.

data.streg.prices = read.csv("lauber-mez_streg-prices.csv") # in Pfennig/KWh
data.nffo.prices2 = read.csv("mitchell_nffo-prices.csv") # in p/KWh
#data.nffo.prices = read.csv("nfpa_average-NFFO-prices.csv") # What specificly is wrong with these numbers ?
#colnames(data.nffo.prices) <- c("NFFO","Years", "Price")

data.rpi.UK = read.csv("ons_mm23_rpi.csv") # index (January 1987=100)
data.rpi.DE = read.csv("destatis_WZ08-47_rpi.csv") # index (2005=100)
data.ppi = read.csv("eurostat-sts_inpp_a_producerpriceindex.csv") # index (2005=100)
  
# Parameters
Discount.rate = 0.08 # 8%
Exchange.rate.GBP = 1.5 # GBP/EUR
Exchange.rate.pfennig =  0.51 # EUR-cent per Pfennig
DE.FIT.initial.fee = 9.1 # c/KWh. From EEG 2000.
DE.FIT.basic.fee = 6.17 # c/KWh. From EEG 2000.
Pool.price.UK = 1.5 # p/KWh. Why not same pool price for NFFO and ROC ???
Future.inflation.rate = 0.0196 # 1.96% annually. From Butler and Neuhoff calculations. Assumption: ECB target of just below 2%. And identical in all three countries.

# Time range
years = c(1990:2010)
# years.StrEG = years[1:10]

# Creating empty components that we will fill content into later
NFFO.cash.flow = matrix(NA, ncol = nrow(data.nffo.prices2), nrow = 20) # p/KWh
StrEG.cash.flow = matrix(NA, ncol = length(years[1:10]), nrow = 20) # c/KWh
colnames(NFFO.cash.flow) = data.nffo.prices2$Year
colnames(StrEG.cash.flow) = years[1:10]


#####
# Fixing base years of the indecies
#####

# We deflate each value in the cash flow, to the price index in the corresponding year. For years after 2011 we assume the same fixed inflation rate for all countries (1.96%)

# This function changes the base of of an index. Required: the new base year, list of years and corresponding list of index values (as old base year).
SET.BASE.YEAR = function(base.year, list.years, list.index){
  i = match(base.year, list.years) # finds the row number that corresponds to the base.year
  df = cbind(list.years, lapply(list.index, function(x){x/list.index[i]*100}) ) # recaulculating index values
  colnames(df) = c("Years", paste("Index (",base.year,"=100)", sep = "") )
  df # return data frame with years and index.
}

UK.2003.rpi = SET.BASE.YEAR(2003, data.rpi.UK$X, data.rpi.UK$CHAW)
DE.2003.rpi = SET.BASE.YEAR(2003, data.rpi.DE$X, data.rpi.DE[,2])

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



#####
# StrEG cash flow
#####

# Using the StrEG prices (90% of consumer electricity prices), we fill in each year with its respective price.
# For years after 1999 where the StrEG system has been terminated and therefore exsist no StrEG price:
# We will assume 4 years with initial fee (the min. in EEG 2000), and the remaining years with basic fee. This is similar to (but not mentioned in) Butler and Neuhoff (2008). This has been found by method of trial-and-error.
# EEG 2000: Operation time of prior installation is halved (min. 4 years). After that follow same reduction of rate rules as new installations.
for(i in 1:20) { # for each year after build (row)
  for(j in 2:length(years[1:10])) { # for each build year (coloumn). 
    # Note: we have no infomation on the StrEG prices in 1990, but have for 2000 - Butler and Neuhoff (2008) have for 1990 (for 2000 it is unknown wheter they use EEG or StrEG prices) - so we start our for-loop at 2 (1991) ???
    # Maybe Butler and Neuhoff (2008), made a "mistake" here, by using 1991-StrEG prices in 1990, and so on ???
    if( ((i-1)+(j-1)) <= length(data.streg.prices$StrEG) ) { # if we have a StrEG price for the given year (1991-2000).
      # StrEG prices are in pfennig - calculating it as EURO-cent.
      StrEG.cash.flow[i,j] = data.streg.prices$StrEG[[((i-1)+(j-1))]]*Exchange.rate.pfennig # c/KWh
    } else {
      if(i <= (length(data.streg.prices$StrEG)+4)-(j-1)+1) { # if in the first 4 years after StrEG (2001-2005), then pay initial fee from EEG 2000.
        StrEG.cash.flow[i,j] = DE.FIT.initial.fee # c/KWh
      } else { # if not in the first 4 years after StrEG (2001-2005), then pay basic fee from EEG 2000.
        StrEG.cash.flow[i,j] = DE.FIT.basic.fee # c/KWh
      }
    }
    
  }
}

# Price adjust
for(i in 1:20) { # for each year after build (row)
  for(j in 2:length(years[1:10])) { # for each build year (coloumn). 
    StrEG.cash.flow[i,j] = StrEG.cash.flow[i,j] / DE.2003.rpi.extended[,2][((i-1)+(j-1))] * 100
  }
}


#####
# NFFO cash flow
#####

# Calculating weigthed average contract price (weights are number of contracts in subband and non-subband)
# Setting the number of non-subbadn contracts to 1 instead of NA such that the weighted average works.
data.nffo.prices2$Projects[1:2] = 1
# Replace NA values with 0:
data.nffo.prices2[is.na(data.nffo.prices2)] = 0
# Calculate average price: (Price * Project + Price.Subband * Project.Subband) / (Project + Project.Subband)
NFFO.weighted.prices = cbind(data.nffo.prices2[1:2], apply(data.nffo.prices2, 1, function(x){ (x[3]*x[5]+x[4]*x[6])/(x[5]+x[6]) }))
colnames(NFFO.weighted.prices) = c("NFFO", "Year", "Price")
      
for(i in 1:20) { # for each year after build (row)
  for(j in 1:nrow(data.nffo.prices2)) { # for each build year (coloumn). 
    
    if(j == 1 && i <= 8) { # If NFFO1 and before 1998 then NFFO1 price
      NFFO.cash.flow[i,j] = NFFO.weighted.prices$Price[[j]]
    } else if(j == 2 && i <= 7) { # If NFFO2 and before 1998 then NFFO2 price
      NFFO.cash.flow[i,j] = NFFO.weighted.prices$Price[[j]]
    } else if(j > 2 && i <= 15) { # If NFFO3-5 then NFFO3-5 prices for first 15 years.
      NFFO.cash.flow[i,j] = NFFO.weighted.prices$Price[[j]]
    } else { # otherwise pay pool price
      NFFO.cash.flow[i,j] = Pool.price.UK 
    }
    
  }
}

# Price adjust
#a = c("START:")
for(i in 1:20) { # for each year after build (row)
  for(j in 1:nrow(data.nffo.prices2)) { # for each build year (coloumn). 
    y = match(colnames(NFFO.cash.flow)[[j]], UK.2003.rpi.extended[,1]) # finding the offset for each NFFO year
    #a = append(a, UK.2003.rpi.extended[,1][(i-1)+y])
    NFFO.cash.flow[i,j] = NFFO.cash.flow[i,j] / UK.2003.rpi.extended[,2][(i-1)+y] * 100
  }
  #a = append(a, "###")
}
#a

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
StrEG.NPV = sapply(colnames(StrEG.cash.flow), function(x){NPV(Discount.rate, StrEG.cash.flow[, x])} ) 
NFFO.NPV = sapply(colnames(NFFO.cash.flow), function(x){NPV(Discount.rate, NFFO.cash.flow[, x])} )

# calculating Payment for each year
StrEG.PMT = lapply(StrEG.NPV, function(x){PMT(Discount.rate, 20, x)} ) 
NFFO.PMT = lapply(NFFO.NPV, function(x){PMT(Discount.rate, 20, x)} )


#####
# Merging NFFO wtih ROC and StrEG with EEG
#####

NFFO.PMT.temp = cbind(data.nffo.prices2$Year, NFFO.PMT)
colnames(data.UK.PMT) = c("Years","UK.PMT")
colnames(NFFO.PMT.temp) = c("Years","UK.PMT") # needs identical colomnames inorder to merge
rownames(NFFO.PMT.temp) = NULL # resets row names
UK.PMT = rbind(NFFO.PMT.temp ,data.UK.PMT)

StrEG.PMT.temp = cbind(years[1:10], StrEG.PMT) 
colnames(data.DE.PMT) = c("Years","DE.PMT")
colnames(StrEG.PMT.temp) = c("Years","DE.PMT") # needs identical colomnames inorder to merge
rownames(StrEG.PMT.temp) = NULL # resets row names
DE.PMT = rbind(StrEG.PMT.temp, data.DE.PMT)


#####
# Scale to UK wind resources
#####

# From Butler and Neuhoff (2008): "If, for example, [wind-]output is 20% higher, then the price paid per MWh can be reduced by 20% while maintaining the same revenue stream for the project and creating little additional maintenance costs."
DE.wind.scalefactor = cbind(data.production$X[2:21], (data.production$Germany[2:21]/data.capacity$Germany[1:20]*1000) / (data.production$United.Kingdom[2:21]/data.capacity$United.Kingdom[1:20]*1000) )
colnames(DE.wind.scalefactor) = c("Years", "Share")
LT.wind.scalefactor = cbind(data.production$X[2:21], (data.production$Lithuania[2:21]/data.capacity$Lithuania[1:20]*1000) / (data.production$United.Kingdom[2:21]/data.capacity$United.Kingdom[1:20]*1000) )
colnames(LT.wind.scalefactor) = c("Years", "Share")

# Manual restricting time period to 1991-2010 for both lists.
DE.PMT.windadjusted = cbind(DE.PMT[2:21,1], as.numeric(DE.PMT[2:21,2])*as.numeric(DE.wind.scalefactor[1:20,2])) # c/KWh, RPI price adjusted (2003=100). And adjusted relative to the higher wind speeds in UK.
# Manual restricting time period to 2004-2010 for both lists.
LT.PMT.windadjusted = cbind(data.LT.PMT[3:9,1], as.numeric(data.LT.PMT[3:9,2])*as.numeric(LT.wind.scalefactor[14:20,2])) # c/KWh, RPI price adjusted (2003=100). And adjusted relative to the higher wind speeds in UK.


#####
# Plotting
#####

pdf(file="fig_anticipated-price-energy_lag.pdf", height=3.5, width=5)

par(mar=c(4,4,1,4)+.3, yaxs='i') # margin, and y-axis start at y=0
plot(x=UK.PMT[1:14,1], y=UK.PMT[1:14,2], axes=FALSE, col="gray", type="h", lwd=10, lend="square", xlab="Years", ylab="p/KWh", ylim=range(0,10))
axis(1, labels=FALSE)
axis(1, at=years, cex.axis=0.7)
axis(2)
par(new=TRUE) # plot the following using the secondary axis: 
plot(x=c(1990, DE.PMT[2:21,1]), y=c(NA, DE.PMT[2:21,2]), type="l", xaxt="n", yaxt="n", xlab="", ylab="", ylim=range(0,10*Exchange.rate.GBP))
lines(x=DE.PMT.windadjusted[,1], y=DE.PMT.windadjusted[,2], lty="dashed", lwd=1)
lines(x=data.LT.PMT[1:9,1], y=data.LT.PMT[1:9,2], col="purple")
lines(x=LT.PMT.windadjusted[,1], y=LT.PMT.windadjusted[,2], col="purple", lty="dashed", lwd=1)
axis(4) # add secondary axis
mtext("c/KWh",side=4,line=3)
legend("topright", legend=c("UK (p/KWh)","DE (c/KWh)", "DE (c/KWh) (with UK wind)", "LT (c/KWh)", "LT (c/KWh) (with UK wind)"), col=c("grey","black","black","purple","purple"), lty = c(NA,"solid","dashed","solid","dashed"), pch = c(15, NA, NA, NA, NA), lwd=c(20, 1, 1, 1, 1), cex=0.5)

dev.off()
