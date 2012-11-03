####################
#
#	Title:  Anticipated price of wind energy in Germany and the UK. (1990 - 2006)
#	Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). 
#	Notes: 
# Units: p/KWh and c/KWh
# Dependencies: fig_anticipated-price-energy.R (DE.PMT.csv, LT.PMT.csv, UK.PMT.csv).
#
####################

#library(ggplot2)
#library(reshape2)

# LOAD THE DATA
data.DE.PMT = read.csv("DE.PMT.csv") # in c/KWh
data.LT.PMT = read.csv("LT.PMT.csv") # in c/KWh
data.UK.PMT = read.csv("UK.PMT.csv") # in p/KWh

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

# This function changes the base of of an index. Required: the new base year, list of years and corresponding list of index values (as old base year).
SET.BASE.YEAR = function(base.year, list.years, list.index){
  i = match(base.year, list.years) # finds the row number that corresponds to the base.year
  df = cbind(list.years, lapply(list.index, function(x){x/list.index[i]*100}) ) # recaulculating index values
  colnames(df) = c("Years", paste("Index (",base.year,"=100)", sep = "") )
  df # return data frame with years and index.
}

UK.2010.rpi = SET.BASE.YEAR(2010, data.rpi.UK$X, data.rpi.UK$CHAW)
DE.2010.rpi = SET.BASE.YEAR(2010, data.rpi.DE$X, data.rpi.DE[,2])
LT.2010.ppi = SET.BASE.YEAR(2010, data.ppi$X, data.ppi$Lithuania)

# temporary:
UK.2003.rpi = SET.BASE.YEAR(2003, data.rpi.UK$X, data.rpi.UK$CHAW)
DE.2003.rpi = SET.BASE.YEAR(2003, data.rpi.DE$X, data.rpi.DE[,2])
LT.2003.ppi = SET.BASE.YEAR(2003, data.ppi$X, data.ppi$Lithuania)


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

#####
# NFFO cash flow
#####

# Calculating weigthed average contract price (weights are number of contracts in subband and non-subband)
# Setting the number of non-subbadn contracts to 1 instead of NA such that the weighted average works.
data.nffo.prices2$Projects[1:2] = 1
# Replace NA values with 0:
data.nffo.prices2[is.na(data.nffo.prices2)] = 0
NFFO.weighted.prices = cbind(data.nffo.prices2[1:2], apply(data.nffo.prices2, 1, function(x){ (x[3]*x[5]+x[4]*x[6])/(x[5]+x[6]) }))
colnames(NFFO.weighted.prices) = c("NFFO", "Year", "Price")
      
for(i in 1:20) { # for each year after build (row)
  for(j in 1:nrow(data.nffo.prices2)) { # for each build year (coloumn). 

    # NFFO.weighted.prices
    #NFFO.cash.flow[i,j] =
  }
}


# NFFO 1-5 (1990-2001)
# Net Present Value Calculation ???
#for(i in 1:5) {
#  j = (nffo.prices$Year[[i]]-1990) # Converts years coloumn to index that corresponds with out array
  # Adjust the prices to 2003 prices (???)
  # The length of contracts during which turbines received a subsidised electricity price under the NFFO increased during the 1990s. Take this into consideration!
  # Revenues in subsequent years were set at the pool price of 1.5 p/kWh.
#  UK[j] = nffo.prices$Price[[i]] # p/KWh
#}
# ROC (2002-2006)
#for(i in 12:17) {
  #UK[j] = # p/KWh
#}


#####
# Germany
#####

# Need data source ???
# Germany = 


#####
# Germany (with prologation, with UK wind resources)
#####



#####
# Germany (no prologation, with UK wind resources)
#####

# Germnay, but scaled according to fig_generation-capacity.pdf



# Join in table
#ROC.Value = cbind(years, Buy.Out.Value, Recycled.Green.Premium, Levy.Exemption.Certificate, Energy.Value, CO2.Price, deparse.level = 1)
#ROC.Value = cbind(Buy.Out.Value, Recycled.Green.Premium, Levy.Exemption.Certificate, Energy.Value, CO2.Price, deparse.level = 1)

# Transform to long format
#m.ROC.Value = melt(as.data.frame(ROC.Value), measure.vars = 2:6)
#m.ROC.Value = melt(ROC.Value)
#m.ROC.Value = cbind(m.ROC.Value, m.ROC.Value$Var1 + rep(2001, 100)) # work around to add years propperly
#colnames(m.ROC.Value) = c("id", "variable", "value", "years")

# Plot stacked barplot
#ggplot(m.ROC.Value, aes(x=factor(years), y=value, fill=factor(variable)) ) + 
#  geom_bar(position="stack") + 
#  ylab("p/KWh") + 
#  xlab("Years") +
#  labs(fill="", title="Components of the Price Paid to Wind Energy under ROC (2002 - 2021)") +
#  coord_cartesian(ylim = c(0, 8)) +
#  theme_bw() +
#  theme(legend.position="top")
#  #theme(axis.text.x = element_text(size = 8))
#ggsave("figure_roc-price-components.pdf")
