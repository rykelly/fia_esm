rm(list=ls())
library(data.table)

file.in.me = '/Work/Research/Macrosystems/FIA Phase Space/Data/ESMrawRKv05_all.rds'
file.in.TA = '/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/ESMraw.rds'

TA.only = T

thin.plot = 10000
x.lim = c(0,50) #c(14, 43)
y.lim = c(0,2000) #c(50,950)

dia.bin   = 1.5
dia.lim   = c(0,60)

use.tpa   = F
tpa.bin   = 25
tpa.lim   = c(0,1500)

stock.bin = 10

bin.min.n = 50

# -------------------
q = as.data.table(readRDS(file.in.me))
  setnames(q, tolower(names(q)))
  q[, plt_cn := as.character(plt_cn)]

# TEST
#   q[, prevdiamean := mean(prevdia, na.rm=T), by=plt_cn]
#   q[, diamean     := mean(dia, na.rm=T), by=plt_cn]
#   q = q[prevstocking5mid>0]

# # PREV_PLT_CN>0
#   q = q[prev_plt_cn>0]


  if(TA.only) q = q[in.ta==1]


x = as.data.table(readRDS(file.in.TA))
  setnames(x, tolower(names(x)))
  x[, plt_cn := as.character(plt_cn)]
  x[, prevdiamean := mean(prevdia, na.rm=T), by=plt_cn]
  x[, diamean     := mean(dia, na.rm=T), by=plt_cn]


# par(mfrow=c(2,2))
#   hist(x$prevdiamean, seq(0,100,2))
#   hist(q$prevdiamean, seq(0,100,2))
#   hist(x$prevtpasum, seq(0,1000,20))
#   hist(q$prevtpasum, seq(0,1000,20))



par(mfrow=c(1,2))
lw2 = 1


i=1
for(i in 1:2) {
  if(i==1) {
    z=x
    main="ESM - T. Andrews"
  } else {
    z=q
    main="ESM - RK"
  }

# --- Convert units
  z[, diamean.m     := diamean     * 2.54      ]
  z[, prevdiamean.m := prevdiamean * 2.54      ]
  z[, tpasum.m      := tpasum      / 0.404686  ]
  z[, prevtpasum.m  := prevtpasum  / 0.404686  ]


# --- Binning
  z = z[ prevdiamean.m>min(dia.lim) & prevdiamean.m<max(dia.lim) &
         prevtpasum.m >min(tpa.lim) & prevtpasum.m <max(tpa.lim) ]

  z[,                   prevdiabin   := ceiling(prevdiamean/dia.bin)*dia.bin       ]
  
  if(use.tpa) {
    z[,                   prevstockbin := ceiling(prevtpasum/tpa.bin)*tpa.bin  ]
  } else {
    z[,                   prevstockbin := ceiling(prevstocking5mid/stock.bin)*stock.bin  ]  
  }
  z[, prevdiastockbin := prevdiabin * 10000 + prevstockbin]


# --- Bin means
  binmean = function(x) {
    if(length(x)<bin.min.n) {
      return(NULL)
    } else {
      return(mean(x, na.rm=T))
    }
  }
  meanB = z[, .(binmean(prevdiamean.m),binmean(diamean.m),
                binmean(prevtpasum.m),binmean(tpasum.m)), by=prevdiastockbin]
    setnames(meanB, c("bin","prevdiameanB","diameanB","prevtpasumB","tpasumB"))
    meanB
  
# --- PLOT
  plot(c((z$prevdiamean.m),(z$diamean.m)),c((z$prevtpasum.m),(z$tpasum.m)), type ="n", col="grey94",
    main=main,ylim=y.lim, xlim=x.lim, mgp=c(2,1,0), xlab=NA, ylab=NA, axes=F) 
  axis(side=1, tck=-0.01, labels=NA, lwd=0.75)
  axis(side=2, tck=-0.01, labels=NA, lwd=0.75)
  axis(side=1, lwd=0, line= -0.7)
  axis(side=2, lwd=0, line= -0.7)
  mtext(side=1, "mean tree diameter (cm)", line=1.2)
  mtext(side=2, expression(paste("stem density (trees ha"^"-1",")")), line=1.2)
  box()

  if(thin.plot>0) {
    ind = seq(1,nrow(z), length=thin.plot)
    arrows(z$prevdiamean.m[ind], z$prevtpasum.m[ind],
         z$diamean.m[ind], z$tpasum.m[ind], 
         col=grey(0.8), length=0.065, angle=22,lwd=0.9)
  } else {
    arrows(z$prevdiamean.m,z$prevtpasum.m,
           z$diamean.m,z$tpasum.m, 
           col=grey(0.8), length=0.065, angle=22,lwd=0.9)
  }

  arrows(meanB$prevdiameanB,meanB$prevtpasumB,meanB$diameanB,meanB$tpasumB, col=grey(1), 
     length=0.085, angle=25,lwd=lw2+0.75)
  arrows(meanB$prevdiameanB,meanB$prevtpasumB,meanB$diameanB,meanB$tpasumB, col=1,
     length=0.08, angle=28,lwd=lw2) 

}

  #plot all plots as individual background vectors colored by inital stocking
  # arrows((q$prevdiamean),(q$prevtpasum),(q$diamean),(q$tpasum), col= (q$prevstocking5mid/4)+18, length=0.065, angle=22,lwd=0.9)

  # arrows((q$prevdia),(q$prevtpasum),(q$dia),(q$tpasum), col=grey(0.5), length=0.065, angle=22,lwd=0.9)
  # 
  # arrows((q$prevdia5alive),(q$prevtpasum),(q$dia5alive),(q$tpasum), col=grey(0.5), length=0.065, angle=22,lwd=0.9)
  # 
  # summary(q$prevdiamean)
  # summary(q$diamean)
  # 
  # summary(q$prevtpasum)
  # summary(q$tpasum)
