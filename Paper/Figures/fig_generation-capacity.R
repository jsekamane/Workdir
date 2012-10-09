capacity = read.csv("eurostat-nrg_113a_capacity-wind-MW.csv")
production = read.csv("eurostat-nrg_1072_production-wind-GWh.csv")
pdf(file="figure_generation-capacity.pdf", height=3.5, width=5)
#par(mar=c(4.2, 3.8, 0.2, 0.2)) #Trim margins
plot(production$Germany*1000/capacity$Germany, type="l", ylim=range(0,3000), axes=F, ann=T, xlab="Years", ylab="MWh / MW", cex.lab=0.8, lwd=2)
axis(1, lab=F) #disable labels on x-axis
axis(1, at=1:21, lab=capacity$X, cex.axis=0.7)
axis(2, las=1, cex.axis=0.8)
box()
lines(production$Lithuania*1000/capacity$Lithuania, type="l", lty=2, lwd=2)
lines(production$United.Kingdom*1000/capacity$United.Kingdom, type="l", lty=3, lwd=2)
title(main="Generation from installed capacity (MWh/MW)")
mtext("1990-2010");
legend("topleft", names(capacity[-1]), cex=0.8, lty=1:3, lwd=2, bty="n");
dev.off()
#par(mar=c(5, 4, 4, 2) + 0.1) #Reset standart margins