# rm(list=ls())
library(data.table)

file.in.me = "/Work/Research/Macrosystems/FIA Phase Space/Data/1.Filtered/ESMrawRKv09.rds"
file.in.TA = '/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/ESMraw.rds'

TA.only = F

thin.plot = 10000
x.lim = c(10,50) #c(14, 43)
y.lim = c(0,2000) #c(50,950)

dia.bin   = 2.5
dia.lim   = c(0,50)

use.tpa   = T
tpa.bin   = 100
tpa.lim   = c(0,1500)

stock.bin = 10

bin.min.n = 100

out.dir        = '/Work/Research/Macrosystems/FIA Phase Space/'
file.name.base = 'ESM_RK_v04_'
save.fig   = T
fig.format = 'pdf'

save.dat   = T

# -------------------
file.name.base = paste0(file.name.base,
  ifelse(TA.only, 'TA.', 'ALL.'),
  "d",dia.bin,".", 
  ifelse(use.tpa, paste0("t",tpa.bin), paste0("s",stock.bin)), ".",
  "n", bin.min.n)


if(!exists("TA.only.store")) TA.only.store=TA.only
if(!exists("file.in.me.store")) file.in.me.store=file.in.me
if(!exists("file.in.TA.store")) file.in.TA.store=file.in.TA
if(!exists("x") | TA.only.store!=TA.only | file.in.me.store!=file.in.me | file.in.TA.store!=file.in.TA) {
  cat("Reloading data...")
  TA.only.store = TA.only; file.in.me.store = file.in.me; file.in.TA.store = file.in.TA
  
  q = as.data.table(readRDS(file.in.me))
    setnames(q, tolower(names(q)))
    q[, plt_cn := as.character(plt_cn)]

    if(TA.only) q = q[in.ta==1]


  x = as.data.table(readRDS(file.in.TA))
    setnames(x, tolower(names(x)))
    x[, plt_cn := as.character(plt_cn)]
    x[, prevdiamean := mean(prevdia, na.rm=T), by=plt_cn]
    x[, diamean     := mean(dia, na.rm=T), by=plt_cn]
  cat("done!\n")
}


if(save.fig) {
  fig.file = paste0(out.dir,'Figs/',file.name.base,'.',fig.format)
  if(fig.format=='pdf')
    pdf(fig.file, width=10,height=6)
  if(fig.format=='png')
    png(fig.file, width=10, height=6, res=150, units="in")
}
par(mfrow=c(1,2))
lwd1 = 0.5
lw2 = 2
i=1
for(i in 1:2) {
  if(i==1) {
    z=x
    main="ESM - T. Andrews"
    cat("Plotting TA data...\n")
  } else {
    z=q
    main="ESM - RK"
    cat("Plotting RK data...\n")
  }

# --- Convert units
  z[, diamean.m     := diamean     * 2.54      ]
  z[, prevdiamean.m := prevdiamean * 2.54      ]
  z[, tpasum.m      := tpasum      / 0.404686  ]
  z[, prevtpasum.m  := prevtpasum  / 0.404686  ]


# --- Binning
  z = z[ prevdiamean.m>min(dia.lim) & prevdiamean.m<max(dia.lim) &
         prevtpasum.m >min(tpa.lim) & prevtpasum.m <max(tpa.lim) ]

  z[,                   prevdiabin   := ceiling(prevdiamean.m/dia.bin)*dia.bin       ]
  
  if(use.tpa) {
    z[,                   prevstockbin := ceiling(prevtpasum.m/tpa.bin)*tpa.bin  ]
  } else {
    z[,                   prevstockbin := ceiling(prevstocking5mid/stock.bin)*stock.bin  ]  
  }
  z[, prevdiastockbin := paste(prevdiabin,prevstockbin,sep='_')]


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
  mtext(side=1, "mean tree diameter (cm)", line=2)
  mtext(side=2, expression(paste("stem density (trees ha"^"-1",")")), line=2)
  title(main=paste0("diam.bin=",dia.bin,", ", 
      ifelse(use.tpa, paste0("tpa.bin=",tpa.bin), paste0("stocking.bin=",stock.bin)),
      ", bin.min.n=", bin.min.n),
      cex.main=0.8, col.main=grey(0.3), line=0.7)
  box()

  options(warn=-1) # There will be zero-length error warnings, but they don't matter here. 
  if(thin.plot>0) {
    ind = seq(1,nrow(z), length=thin.plot)
    arrows(z$prevdiamean.m[ind], z$prevtpasum.m[ind],
           z$diamean.m[ind], z$tpasum.m[ind], 
           col=grey(0.8), length=0.045, angle=22,lwd=lwd1)
  } else {
    arrows(z$prevdiamean.m,z$prevtpasum.m,
           z$diamean.m,z$tpasum.m, 
           col=grey(0.8), length=0.045, angle=22,lwd=lwd1)
  }
  options(warn=0) # Turn warnings back on because do want to know if any of the mean arrows are missing

  arrows(meanB$prevdiameanB,meanB$prevtpasumB,meanB$diameanB,meanB$tpasumB, col=1,
     length=0.065, angle=22,lwd=lw2) 
}

if(save.fig) dev.off()

if(save.dat) {
  cat("Saving data...\n")
  dat.file = paste0(out.dir,'Data/2.Binned/',file.name.base,'.rds')
  saveRDS(z, dat.file)
}






#     points(z$prevdiamean.m,z$prevtpasum.m, col=2, pch=3, lwd=2, cex=0.5)
