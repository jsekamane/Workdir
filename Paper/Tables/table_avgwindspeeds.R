####################
#
#  Title: Comparable average wind speeds at height of 30 m (m/s)
#	 Source: UK: Butler and Neuhoff (2004, p. 13). DE: IEA (2000, p. 96). LT: Marciukatis (2011), Erlickytė-Marčiukaitienė, Marčiukaitis and Tumosa (2007, p. 27)
#	 Notes: LT and UK wind speeds have been calculated at 30 m using the wind gradient equation with a Hellman exponent of 1/7. * Wind speeds from hydrometeorological stations measured at 10 m above ground. ** Wind speeds calculated using the DTI NOABL database
#
####################


# DATA
# speed at existing wind farms ... an average wind speed of 7.04 m/s at 25m. UK wind speed calculated using the DTI NOABL database.
UK.25m = 7.04 # m/s
# “reference turbine” is ... operating at an average wind speed of 5.5 m/s at 30 m height with ... a roughness length of 0.1 m
DE.30m = 5.5 # m/s
# At three test sites: Vilkyciai, Kretinga, Tauragè:
LT.V.43m = 5.94
LT.K.27.5m = 6
LT.T.28m = 6.4
# Nustatyta, kad 1995–2003 m. vidutinis vėjo greitis 50 m aukštyje Giruliuose siekė 6,4 m/s.
LT.G.50m = 6.4
# 1996–1997 m. mokslinė inovacinė bendrovė „Eksponentė“ kartu su Norvegijos elektros įmone „Nord Trondelag Elektrisi- tetsverk“ bei Norvegijos energetikos technologijų institutu Būtingėje atliko vėjo greičio matavimus 30 m aukštyje. Išanalizavus matavimo duomenis, nustatyta, kad vidutinis metinis vėjo greitis 50 m aukštyje siekia 7,4 m/s.
LT.B.50m = 7.4
# Pirmieji vėjo greičio ir krypties matavimo duomenų tyrimai buvo atliekami remiantis hidrometeorologijos stočių matavimo duomenimis 10 m aukštyje virš žemės paviršiaus 
# Pa- gal šį žemėlapį didžiausias vėjo energijos potencialas (> 5,5 m/s) yra Lietuvos pajūryje, o mažiausias (< 2,5 m/s) – Rytų ir Pietry- čių Lietuvoje.
LT.costal.10m = 5.5
LT.southeast.10m = 2.5

# CALCULATING THE HELLMAN EXPONENT
# used in Butler and Neuhoff (2004): 
# wind speed of 7.04 m/s at 25m, which corresponds to 7.22m/s at 30m

# http://en.wikipedia.org/wiki/Wind_gradient#Wind_turbines
# Equation: V(h) = V_30 * ( h / 30 ) ^ a
# Solving: a = ln(V/V_30) / ln(h/30)
# a = log(7.04/7.22) / log(25/30) # 0.1384739
a = 1/7
# Commonly used Hellman exponent is 1/7 in neutral flow. 1/7 = 0.1428571
# Inbetween:
# Unstable air above flat open coast:  0.11
# Neutral air above flat open coast:	0.16

# WIND SPEED CALCULATOR
V = function(V_ref, h_ref, h) {
  V_ref * (h/h_ref)^a
}

# WIND SPEEDS AT 30M
UK.30m = V(UK.25m, 25, 30) # m/s at height of 30m.

LT.V.30m = V(LT.V.43m, 43, 30)
LT.K.30m = V(LT.K.27.5m, 27.5, 30)
LT.T.30m = V(LT.T.28m, 28, 30)
LT.testsites.avg.30m = mean(c(LT.V.30m, LT.K.30m, LT.T.30m))

LT.G.30m = V(LT.G.50m, 50, 30)

LT.B.30m = V(LT.B.50m, 50, 30)

LT.costal.30m = V(LT.costal.10m, 10, 30)
LT.southeast.30m = V(LT.southeast.10m, 10, 30)

# PRESENTATION:
row.header = c("DE", "LT", "LT", "LT", "LT", "UK") 
row.subheader = c("Reference site", "Average of test sites (Vilkyciai, Kretinga, Tauragè)", "Giruliuose", "Būtingėje", "Coastal region *", "Existing wind farms **")
row.values = round(c(DE.30m, LT.testsites.avg.30m, LT.G.30m, LT.B.30m, LT.costal.30m, UK.30m), digits=2)

table.windspeeds.30m = rbind(row.subheader, row.values, deparse.level = 0)
colnames(table.windspeeds.30m) = row.header

write.csv(table.windspeeds.30m, "table_avgwindspeeds.csv", row.names = FALSE)
