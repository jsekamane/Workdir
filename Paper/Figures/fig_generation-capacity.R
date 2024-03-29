####################
#
#	Title: Generation from installed capacity (MWh/MW)
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

pdf(file="figure_generation-capacity.pdf", height=2.5, width=5)

par(mar=c(2,3,0,0)+.3, yaxs='i', xaxs='i')
plot(production$Germany*1000/capacity$Germany, type="l", ylim=range(0,3000), axes=F, ann=T, ylab="", xlab="", cex.lab=0.7, lwd=2)
abline(h=c(0,500,1000,1500,2000,2500,3000), col="#D9D9D9", lwd=0.3)
mtext("Full load hors (MWh/MW)", 2, cex=0.7, line=2.5)
axis(1, lab=F, col="#989898") #disable labels on x-axis
axis(1, at=1:21, lab=capacity$X, cex.axis=0.5, col="#989898")
axis(2, las=1, cex.axis=0.5, col="#989898")
#box()
lines(production$Lithuania*1000/capacity$Lithuania, type="l", lty=2, lwd=2)
lines(production$United.Kingdom*1000/capacity$United.Kingdom, type="l", lty=3, lwd=2)
#title(main="Generation from installed capacity (MWh/MW)")
#mtext("1990-2010");
legend("topleft", names(capacity[-1]), cex=0.7, lty=1:3, lwd=2, bty="n")

dev.off()
