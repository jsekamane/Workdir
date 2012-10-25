####################
#
#	Title:  Anticipated price of wind energy in Germany and the UK. (1990 - 2006)
#	Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). 
#	Notes: 
# Units: p/KWh = £0.01/KWh = £ 1/100 /KWh
#
####################

#library(ggplot2)
#library(reshape2)

# Load data
nffo.prices = read.csv("nfpa_average-NFFO-prices.csv")
colnames(nffo.prices) <- c("NFFO","Years", "Price")
#exchangerate = read.csv("eurostat-ert_bil_eur_a_exchangerate.csv")
#co2 = read.csv("eurostat-env_air_gge_co2.csv") # in thousands of tonnes

# Time range
years = c(1990:2006)

# Creating empty components that we will fill content into later
UK = rep(NA, length(years)) # UK Expected Payment
Germany = rep(NA, length(years)) # Germany (no prologation)
Germany.UK.adjust.prologation = rep(NA, length(years)) # Germany (with prologation, with UK wind resources)
Germany.UK.adjust = rep(NA, length(years)) # Germany (no prologation, with UK wind resources)


#####
# UK
#####

# NFFO 1-5 (1990-2001)
# Net Present Value Calculation ???
for(i in 1:5) {
  j = (nffo.prices$Year[[i]]-1990) # Converts years coloumn to index that corresponds with out array
  # Adjust the prices to 2003 prices (???)
  # The length of contracts during which turbines received a subsidised electricity price under the NFFO increased during the 1990s. Take this into consideration!
  # Revenues in subsequent years were set at the pool price of 1.5 p/kWh.
  UK[j] = nffo.prices$Price[[i]] # p/KWh
}
# ROC (2002-2006)
for(i in 12:17) {
  #UK[j] = # p/KWh
}


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
