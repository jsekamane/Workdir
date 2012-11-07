####################
#
#	Title: Wind power share of total generation (%)
#	Source: Eurostat (nrg_113a, nrg_105a)
#	Notes: Onshore and offshore wind turbines. Sum of main activity producers and autoproducers
#
####################

finalconsumption = read.csv("eurostat-nrg_105a_consumption-final-GWh.csv")
totalproduction = read.csv("eurostat-nrg_105a_production-total-GWh.csv")
production = read.csv("eurostat-nrg_1072_production-wind-GWh.csv")

pdf(file="figure_generation-share2.pdf", height=3.5, width=5)

plot(production$Germany/finalconsumption$Germany*100, type="l", ylim=range(0,10), axes=F, ann=T, xlab="Years", ylab="%", cex.lab=0.8, lwd=2)
axis(1, lab=F) #disable labels on x-axis
axis(1, at=1:21, lab=production$X, cex.axis=0.7)
axis(2, las=1, cex.axis=0.8)
box()
lines(x=c(15:21),y=production$Lithuania[15:21]/finalconsumption$Lithuania[15:21]*100, type="l", lty=2, lwd=2)
lines(production$United.Kingdom/finalconsumption$United.Kingdom*100, type="l", lty=3, lwd=2)

# Adds %-label to last observation
text(21, production$Germany[21]/finalconsumption$Germany[21]*100, paste(format(production$Germany[21]/finalconsumption$Germany[21]*100,digits=2,nsmall=1),"%"), cex=0.6, pos=3)
text(21, production$Lithuania[21]/finalconsumption$Lithuania[21]*100, paste(format(production$Lithuania[21]/finalconsumption$Lithuania[21]*100,digits=2,nsmall=1),"%"), cex=0.6, pos=1)
text(21, production$United.Kingdom[21]/finalconsumption$United.Kingdom[21]*100, paste(format(production$United.Kingdom[21]/finalconsumption$United.Kingdom[21]*100,digits=2,nsmall=1),"%"), cex=0.6, pos=3)

title(main="Wind power share of final consumption (%)")
mtext("1990-2010");
legend("topleft", names(production[-1]), cex=0.8, lty=1:3, lwd=2, bty="n");

dev.off()
