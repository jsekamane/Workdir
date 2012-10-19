####################
#
#	Title: Components of the Price Paid to Wind Energy under ROC (2002 - 2021)
#	Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). Data sources: EUROSTAT (sts_inppgr_a, ert_bil_eur_a, env_air_gge)
#	Notes: 
# Units: p/KWh = £0.01/KWh = £ 1/100 /KWh
#
####################

library(ggplot2)
library(reshape2)

# Load data
ppi = read.csv("eurostat-sts_inppgr_a_producerpriceindex.csv")
exchangerate = read.csv("eurostat-ert_bil_eur_a_exchangerate.csv")
co2 = read.csv("eurostat-env_air_gge_co2.csv") # in thousands of tonnes

# Time range
years = c(2002:2021)

# Creating empty components that we will fill content into later
Buy.Out.Value = rep(NA, 20)
Recycled.Green.Premium = rep(NA, 20)
Levy.Exemption.Certificate = rep(NA, 20)
Energy.Value = rep(NA, 20)
CO2.Price = rep(NA, 20)


#####
# Buy Out Value
#####

# From 2002-2010: At the limit of £30/MWh in 2002/2003, increasing anually with inflation (???%).
Buy.Out.Value[1] = 30/1000 *100 # p/KWh
for(i in 2:9) {
  #Buy.Out.Value[[i]] = Buy.Out.Value[[i-1]] * 1.02 # ??? % annual increase
  Buy.Out.Value[[i]] = Buy.Out.Value[[i-1]] * (1 + ppi$United.Kingdom[5+i]/100) # change with producer price index ???
}
# From 2010-2021: Falls with 1/5 p/KWh annually (falls with technology)
for(i in 10:20) {
  Buy.Out.Value[[i]] = Buy.Out.Value[[i-1]] - 1/5 # 1/5 p/KWh annual decrease ???
}


#####
# Recycled Green Premium
#####

# I'am missing a data source ???


#####
# Levy Exemption Certificate
#####

# Assume: generators are able to negotiate 50% of the LEC value. 
# Remains constant to its current level of 0.44 p/kWh (so 50% is 0.22 p/kWh).
Levy.Exemption.Certificate[1:20] = 0.22 # p/KWh


#####
# Energy Value
#####

# Data source NFPA auctions ???
# Energy Value = Price of energy including ROC and LEC - (Price of ROC + Price of LEC) ???
for(i in 1:9) {
  # I'am missing a data source ???
}
# Falls with technology ??? - Fall to 1.5p/KWh in 2020
Energy.Value[9] = 3 # Last observation - Just something until we have data ???
Energy.Value[19:20] = 1.5 #p/KWH in 2020 and 2021
for(i in 10:18) {
  Energy.Value[[i]] = Energy.Value[[i-1]] - (Energy.Value[9] - Energy.Value[19])/(19-9)
}


#####
# CO2 Price
#####

# European Emission Trading Scheme was launched in 2005
CO2.Price[1:3] = 0
# From 2005-2007: tCO2  x  € ??? /tCO2  x  Exch.Rate  = CO2-price p/KWh ???
for(i in 4:5) {
  # I'am missing a data source ???
  # CO2.Price[[i]] = * * exchangerate$Pound.sterling[[i]]
}
# From 2008-2012: tCO2  x  € ??? /tCO2  x  Exch.Rate  = CO2-price p/KWh ???
for(i in 6:11) {
  # I'am missing a data source ???
  # CO2.Price[[i]] = * * exchangerate$Pound.sterling[[i]]
}

# From 2013-2021: tCO2  x  €22.77/tCO2  x  Exch.Rate  = CO2-price p/KWh  ???
for(i in 12:20) {
  # I'am missing a data source ???
  #CO2.Price[[i]] = 22.77 * exchangerate$Pound.sterling[[i]]
}

# Join in table
ROC.Value = cbind(years, Buy.Out.Value, Recycled.Green.Premium, Levy.Exemption.Certificate, Energy.Value, CO2.Price, deparse.level = 1)
colnames(ROC.Value) = c("years",1:5)

m.ROC.Value = reshape(ROC.Value, direction="long", 
                      varying=list(names(ROC.Value)[2:6]), 
                      v.names="Value", 
                      idvar="NA", 
                      timevar="Variable", 
                      times=1:5) #times=1950:1954
# http://stackoverflow.com/questions/2185252/reshaping-data-frame-from-wide-to-long-format
# http://stackoverflow.com/questions/4811316/quick-help-creating-a-stacked-bar-chart-ggplot2
# http://stackoverflow.com/questions/7583432/plot-stacked-bar-plot-in-r

#barplot(ROC.Value, xlab="Years", ylab="p/KWh") # , legend = rownames(ROC.Value) col=c("darkblue","red")

#pdf(file="figure_roc-price-components.pdf", height=3.5, width=5)
#plot(capacity$Germany, type="l", ylim=range(0,28000), axes=F, ann=T, xlab="Years", ylab="p/KWh", cex.lab=0.8, lwd=2)
#axis(1, lab=F) #disable labels on x-axis
#axis(1, at=1:21, lab=years, cex.axis=0.7)
#axis(2, las=1, cex.axis=0.8)
#box()
#lines(x=c(15:21), y=capacity$Lithuania[15:21], type="l", lty=2, lwd=2) # only plot line from 2004
#lines(capacity$United.Kingdom, type="l", lty=3, lwd=2)

#title(main="Components of the Price Paid to Wind Energy under ROC (2002 - 2021)")
#mtext("2002-2021");
#legend("topleft", names(capacity[-1]), cex=0.8, lty=1:3, lwd=2, bty="n");
#dev.off()
