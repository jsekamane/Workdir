####################
#
#	Title:  Expected Average Remuneration under the EEG and the ROC. (2002 - 2021)
#	Source: Calculations and assumptions: Buttler and Neuhoff (2008, 2004). DE: Lauber and Mez (2004)
#	Notes: 
# Units: p/KWh = £0.01/KWh = £ 1/100 /KWh
#
####################

#library(ggplot2)
#library(reshape2)

# Load data
#nffo.prices = read.csv("nfpa_average-NFFO-prices.csv")
#colnames(nffo.prices) <- c("NFFO","Years", "Price")
#exchangerate = read.csv("eurostat-ert_bil_eur_a_exchangerate.csv")
#co2 = read.csv("eurostat-env_air_gge_co2.csv") # in thousands of tonnes

# Time range
years = c(2002:2021)

# Creating empty components that we will fill content into later
ROC.pounds = rep(NA, length(years)) # ROC (p/KWh)
EEG.euro = rep(NA, length(years)) # EGG (c/KWh)
EEG.pounds = rep(NA, length(years)) # EGG (p/KWh)
LT.euro = rep(NA, length(years)) # Lithuania (p/KWh)


#####
# ROC
#####



#####
# EGG (c/KWh)
#####
Feedintariff.DE = rep(NA, length(years))
# 2002-2004: The Renewable Energy Sources Act of 2000
Feedintariff.DE[1] = 9.0 
Feedintariff.DE[2] = 8.8
Feedintariff.DE[3] = 8.7
for(i in 4:20) {
  # 2004: 8.70 c/KWh. Decrease with 2% anually
  Feedintariff.DE[[i]] = Feedintariff.DE[[i-1]] * (1 - 0.02)
}




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
