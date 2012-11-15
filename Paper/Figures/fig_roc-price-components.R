####################
#
#	Title: Components of the Price Paid to Wind Energy under ROC (2002 - 2020)
#	Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). 
#	Data sources: EUROSTAT (sts_inppgr_a, ert_bil_eur_a). Ofgem (2004, p. 23-24. 2008, Table 1-3. 2012, Table 1-3). IFS (2012). NFPA (2012). e-ROC (2012). Defra / DECC (2012, table 4). BlueNext (2012).
#	Notes: Recycled Green Premium (redistributed buy-out funds) are reported six months displaced: 2002/2003, 2003/2004, etc. and we have therefore been averaged. Levy Exemption Certificate (Climate Change Levy) is similarly reported, we have used first year, e.g. 2002 in 2002/2003 rate.
#	Units: p/KWh = £0.01/KWh = £ 1/100 /KWh
# Reverse dependencies: fig_expected-average-remuneration.R (ROC.Value.csv)
#
####################

library(ggplot2)
library(reshape2)

# LOAD THE DATA
# Buy.Out.Value
data.ppi = read.csv("eurostat-sts_inppgr_a_producerpriceindex.csv")
# Recycled.Green.Premium
data.buyout.fund = read.csv("ofgem_buyout-fund.csv") # in p/KWh # in halfyear terms
# Levy.Exemption.Certificate
data.climate.change.levy = read.csv("ifs_climate-change-levy.csv") # in p/KWh # in halfyear terms
# Energy.Value
data.wind.auction = read.csv("nfpa_wind-auction.csv") # in p/KWh
data.roc.auction = read.csv("eroc_roc-auction.csv") # in £/MWh
# CO2.Price
data.exchangerate = read.csv("eurostat-ert_bil_eur_a_exchangerate.csv")
#data.co2.emission = read.csv("eurostat-env_air_gge_co2.csv") # in thousands of tonnes
data.co2.conversion.factor = read.csv("DECC_CO2-conversion-factor.csv") # in kgCO2e / kWh
data.co2.avg.spot.price = read.csv("bluenext_co2price-avg-year.csv") # in €/tCO2
# data.co2.avg.spot.price2 = read.csv("ice_co2price-avg-year.csv") # in ??? assuming €/tCO2. Alternative CO2 Spot prices, but no data for 2005.

# Sorting data
data.co2.avg.spot.price = data.co2.avg.spot.price[ order(data.co2.avg.spot.price[,2]), ]


# Time range - dividing between 31. Dec and 1. Jan.
years = c(2002:2021) # 2002, 2003, ... , 2021

# Creating empty components that we will fill content into later
Buy.Out.Value = rep(NA, 20)
Recycled.Green.Premium = rep(NA, 20)
Levy.Exemption.Certificate = rep(NA, 20)
Energy.Value = rep(NA, 20)
CO2.Price = rep(NA, 20)


#####
# Buy Out Value
#####

# From 2002-2010: At the limit of £30/MWh in 2002/2003, increasing anually with inflation (retail price index).
Buy.Out.Value[1] = 30/1000 *100 # p/KWh
for(i in 2:9) {
  #Buy.Out.Value[[i]] = Buy.Out.Value[[i-1]] * 1.02 # ??? % annual increase
  Buy.Out.Value[[i]] = Buy.Out.Value[[i-1]] * (1 + data.ppi$United.Kingdom[5+i]/100) # change with retail price index
}
# From 2010-2021: Falls with 1/5 p/KWh annually (falls with technology)
for(i in 10:20) {
  Buy.Out.Value[[i]] = Buy.Out.Value[[i-1]] - 1/5 # 1/5 p/KWh annual decrease ???
}


#####
# Recycled Green Premium
#####

# Halfyear values: 2002 will be set to 2002/2003 value. The remaining will be averages across this period and previous period.
Recycled.Green.Premium[1] = data.buyout.fund$Redestributed[1]
for(i in 2:9) {
  Recycled.Green.Premium[[i]] = mean(c(data.buyout.fund$Redestributed[[i-1]],data.buyout.fund$Redestributed[[i]]))
}
# we assume ... that the renewables target is met by ... 2020 ... and the recycled premium falls to zero.
Recycled.Green.Premium[19:20] = 0
for(i in 10:18) {
  Recycled.Green.Premium[[i]] = Recycled.Green.Premium[[i-1]] - (Recycled.Green.Premium[9]-Recycled.Green.Premium[19])/10
}


#####
# Levy Exemption Certificate
#####

# Assume: generators are able to negotiate 50% of the LEC value. 
# Remains constant to its current level of 0.44 p/kWh (so 50% is 0.22 p/kWh).
#Levy.Exemption.Certificate[1:20] = 0.22 # p/KWh
# Alternatively don't make assumtion, but use real data/rate:
# Halfyear values: Years will be set to first year in range, e.g. 2002 will be set to 2002-03, 2003 will be set to 2003-04.
Levy.Exemption.Certificate[1] = data.climate.change.levy$Electricity[2]
for(i in 2:9) {
  Levy.Exemption.Certificate[[i]] = data.climate.change.levy$Electricity[[i+1]]
}
# Assumption from 2010 and onwards: Fairly stable so far, therefore we wil assume that it will be 50% of the 2010-11 level.
Levy.Exemption.Certificate[10:20] = data.climate.change.levy$Electricity[10]*0.50


#####
# Energy Value
#####

# Energy Value = Auction price of wind energy including ROC and LEC - (Auction price of ROC + Price of LEC)
# 2002-2012.
for(i in 1:11) {
  # wind auction started in 2001. roc auction is in £/MWh.
  Energy.Value[[i]] = data.wind.auction$Average.price[[i+1]] - (data.roc.auction$Average.price[[i]]*100/1000 + Levy.Exemption.Certificate[[i]])
}
# Assume: falls with technology. fall to 1.5p/KWh in 2020
Energy.Value[20] = 1.5 # p/KWH in 2020
for(i in 12:19) {
  Energy.Value[[i]] = Energy.Value[[i-1]] - (Energy.Value[11] - Energy.Value[20])/9
}


#####
# CO2 Price
#####

# European Emission Trading Scheme was launched in 2005
CO2.Price[1:3] = 0
# Calculation:
# CO2-price (in p/KWh) =  CO2-price (in €/tCO2)  /  Exch.Rate (from € to £)  x  100 (from £ to pennies)  x  CO2 conversion rate (from t/CO2 to KWh)
# 2005-2010:
for(i in 4:9) {
  # Note: data.exchangerate starts in 1990. data.co2.conversion.factor starts in 1990. data.co2.conversion.factor in kgCO2e / kWh
  CO2.Price[[i]] = data.co2.avg.spot.price$Price3[[i-3]] / data.exchangerate$Pound.sterling[[i+12]] * 100 * (data.co2.conversion.factor$Emission.Factor[[i+12]]/1000)
  # Using fixed exchange rate of 1.5
  #CO2.Price[[i]] = data.co2.avg.spot.price$Price3[[i-3]] * 1.5 * 100 * (data.co2.conversion.factor$Emission.Factor[[i+12]]/1000)
}
# From 2010-2020:
# Butler and Neuhoff assume a fixed CO2 price of 22.77 euros/tCO2 form 2007 and on.
# But since then the CO2 price has fallen from a level around the 20-22 eurto/tCO2 in 2005 and 2008, to 7.5 euro/tCO2 this seems to be a very unlikely assumption.
# We will instead base our assumption on the Committee on Climate Change (2008, p. 162) report:
# Their "Central case offset credit price projection" sets price at 16 euro/tCO2 in 2020, and sligthly lower in 2015.
# We assumme a fixed cost of 16 euro/tCO2, an exchange rate of 1.5 (lower bound), using a conversion factor of 1/2 that approximates the 2010 factor.
# This corresponds to 1.2 p/kWh (which is in identical to Butler and Neuhoff).
CO2.Price[10:20] = 16 * 1.5 * 100 * (0.5/1000)


#####
# Make graph
#####

# Join in table
ROC.Value = cbind(years, Buy.Out.Value, Recycled.Green.Premium, Levy.Exemption.Certificate, Energy.Value, CO2.Price, deparse.level = 1)
#ROC.Value = cbind(Buy.Out.Value, Recycled.Green.Premium, Levy.Exemption.Certificate, Energy.Value, CO2.Price, deparse.level = 1)

# Transform to long format
m.ROC.Value = melt(as.data.frame(ROC.Value), measure.vars = 2:6)
#m.ROC.Value = melt(ROC.Value)
#m.ROC.Value = cbind(m.ROC.Value, m.ROC.Value$Var1 + rep(2001, 100)) # work around to add years propperly
#colnames(m.ROC.Value) = c("id", "variable", "value", "years")

# Plot stacked barplot
colours = c("#7B3294", "#C2A5CF", "#F7F7F7", "#A6DBA0", "#008837")
ggplot(m.ROC.Value, aes(x=factor(years), y=value, fill=factor(variable)) ) + 
  geom_bar(position="stack") + 
  ylab("p/kWh") + 
  xlab("") +
  labs(fill="") +
  coord_cartesian(ylim = c(0, 14)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14)) +
  theme_bw() +
  #scale_fill_grey() +
  scale_fill_manual(values=colours) +
  theme(
    legend.position="top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line=element_line(),
    panel.border=element_blank()
  )
  #theme(axis.text.x = element_text(size = 8))
ggsave("figure_roc-price-components.pdf")


#####
# Export data
#####

# ROC value sum
Total = rowSums(ROC.Value[,2:6])
# Save to CSV
write.csv(cbind(years, Total), "ROC.Value.csv", row.names=TRUE)