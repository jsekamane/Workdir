####################
#
#	Title: Generation from installed capacity lagged one period (MWh/MW_-1)
#	Source: Eurostat (nrg_113a, nrg_1072)
#	Notes: Onshore and offshore wind turbines. Sum of main activity producers and autoproducers
#
####################


capacity = read.csv("eurostat-nrg_113a_capacity-wind-MW.csv")
production = read.csv("eurostat-nrg_1072_production-wind-GWh.csv") # this is in GWh so late we have to multiply with 1000 to get it in MWh.

# How would it look if the numbers went rounded
# capacity$Lithuania[15] = 0.85
# capacity$Lithuania[16] = 0.99
# production$Lithuania[15] = 1.2
# production$Lithuania[16] = 1.8

pdf(file="figure_generation-capacity_lag.pdf", height=3.5, width=5)
plot(production$Germany[2:21]*1000/capacity$Germany[1:20], type="l", ylim=range(0,6000), axes=F, ann=T, xlab="Years", ylab="MWh / MW_-1", cex.lab=0.8, lwd=2)
axis(1, lab=F) #disable labels on x-axis
axis(1, at=1:20, lab=capacity$X[2:21], cex.axis=0.7)
axis(2, las=1, cex.axis=0.8)
box()
lines(production$Lithuania[2:21]*1000/capacity$Lithuania[1:20], type="l", lty=2, lwd=2)
lines(production$United.Kingdom[2:21]*1000/capacity$United.Kingdom[1:20], type="l", lty=3, lwd=2)
title(main="Generation from lagged installed capacity (MWh/MW_-1)")
mtext("1991-2010");
legend("top", names(capacity[-1]), cex=0.8, lty=1:3, lwd=2, bty="n");
dev.off()
