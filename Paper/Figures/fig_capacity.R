####################
#
#	Title: Installed capacity (MW)
#	Source: Eurostat (nrg_113a)
#	Notes: Onshore and offshore wind turbines. Sum of main activity producers and autoproducers
#
####################

subsetx = c(14:21)

capacity = read.csv("eurostat-nrg_113a_capacity-wind-MW.csv")
pdf(file="figure_capacity.pdf", height=3.5, width=5)
plot(capacity$Germany, type="l", ylim=range(0,28000), axes=F, ann=T, xlab="Years", ylab="MW", cex.lab=0.8, lwd=2)
axis(1, lab=F) #disable labels on x-axis
axis(1, at=1:21, lab=capacity$X, cex.axis=0.7)
axis(2, las=1, cex.axis=0.8)
box()
lines(x=c(15:21), y=capacity$Lithuania[15:21], type="l", lty=2, lwd=2) # only plot line from 2004
lines(capacity$United.Kingdom, type="l", lty=3, lwd=2)

#Adding labels and points to year 2010
#points(x=21,y=capacity$Germany[21], pch=16)
text(21, capacity$Germany[21], paste(capacity$Germany[21],"MW"), cex=0.6, pos=1)
#points(x=21,y=capacity$Lithuania[21], pch=17)
text(21, capacity$Lithuania[21], paste(capacity$Lithuania[21],"MW"), cex=0.6, pos=3)
#points(x=21,y=capacity$United.Kingdom[21], pch=15)
text(21, capacity$United.Kingdom[21], paste(capacity$United.Kingdom[21],"MW"), cex=0.6, pos=3)

title(main="Installed capacity (MW)")
mtext("1990-2010");
legend("topleft", names(capacity[-1]), cex=0.8, lty=1:3, lwd=2, bty="n");
dev.off()
