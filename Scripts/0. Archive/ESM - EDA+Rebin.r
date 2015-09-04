rm(list=ls())
library(data.table)

file.in = '/Work/Research/Macrosystems/FIA Phase Space/Data/ESMrawRKv03_trees.rds'
# file.in = '/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/ESMraw.csv'

# -------------------
q = as.data.table(readRDS(file.in))
  setnames(q, tolower(names(q)))
  q[, plt_cn := as.character(plt_cn)]

# q = as.data.table(read.csv(file.in,header=T,as.is=T))
#   setnames(q, tolower(names(q)))
#   q[, plt_cn := as.character(plt_cn)]



#create plot
plot(c((q$PREV_DIAmean),(q$DIAmean)),c((q$PREV_TPAsum),(q$TPAsum)), type ="n", col="grey94",
main=c("Empirical Model of 5-year forest density change, DBH>=12.7 cm", "FIA survey 1998-2012, Eastern USA")
,ylim=c(50,950), xlim=c(14, 43), mgp=c(2,1,0)
,xlab=NA, ylab=NA, axes=FALSE) #1250
axis(side=1, tck=-0.01, labels=NA, lwd=0.75)
axis(side=2, tck=-0.01, labels=NA, lwd=0.75)
axis(side=1, lwd=0, line= -0.7)
axis(side=2, lwd=0, line= -0.7)
mtext(side=1, "Mean tree diameter (cm)", line=1.2)
mtext(side=2, expression(paste("Stem density (trees ha"^"-1",")")), line=1.2)
box()
#plot all plots as individual background vectors colored by inital stocking
# arrows((q$prevdiamean),(q$prevtpasum),(q$diamean),(q$tpasum), col= (q$prevstocking5mid/4)+18, length=0.065, angle=22,lwd=0.9)
arrows((q$prevdiamean),(q$prevtpasum),(q$diamean),(q$tpasum), col=grey(0.5), length=0.065, angle=22,lwd=0.9)

arrows((q$prevdia),(q$prevtpasum),(q$dia),(q$tpasum), col=grey(0.5), length=0.065, angle=22,lwd=0.9)

arrows((q$prevdia5alive),(q$prevtpasum),(q$dia5alive),(q$tpasum), col=grey(0.5), length=0.065, angle=22,lwd=0.9)

summary(q$prevdiamean)
summary(q$diamean)

summary(q$prevtpasum)
summary(q$tpasum)
