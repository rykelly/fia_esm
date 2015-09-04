rm(list=ls())
library(data.table)

file.in = '/Work/Research/Macrosystems/FIA Phase Space/Data/2.Binned/ESM_RK_v04_ALL.d2.5.t100.n100.rds'

diam.rebin.width = 1

# Output file params
  out.dir   = '/Work/Research/Macrosystems/FIA Phase Space/Data/3.edinputs/' #'./output/DF_HW_1999_v04_25p/'
  prefix    = 'ESMv01'
  loc.text  = "lat35.5lon-79.5"

# ED2IN settings
  file.ed2in.template = '/Work/Research/Macrosystems/FIA Phase Space/Data/3.edinputs/ESMv01/ED2IN.ESMv01_TEMPLATE'
  
  ffilout.prefix = '/projectnb/dietzelab/rykelly/edoutputs/analy/'
  sfilout.prefix = '/projectnb/dietzelab/rykelly/edoutputs/histo/'
  sfilin.prefix  = '/usr2/postdoc/rykelly/edinputs/init/ESMv01/'

# ----------------------------

dat = readRDS(file.in)
  dat = dat[,.(dia_begin, start1tpa, pft, prevdiabin, prevstockbin, prevdiastockbin)]
  setnames(dat, c('diam','dens','pft','bin.diam','bin.dens','bin'))
  dat0=dat # **********************************************

# Re-bin on diameter
#   dat[, diam.rebin := ceiling(diam/diam.rebin.width)*diam.rebin.width]
  # Replaced ceiling way with floor way to avoid forming a bin '5' with only trees of exactly dbh 5 in it
  dat[, diam.rebin := (floor(diam/diam.rebin.width)+1)*diam.rebin.width]


# Convert density from 1/ha to 1/m2
  dat[, dens := dens/10000]

# Remove NA
  dat = dat[!is.na(diam) & !is.na(pft)]


# Assign PFT codes
   #---------------------------------------------------------------------------------------#
   # 1 - C4 grass                         |  9 - early temperate deciduous                 #
   # 2 - early tropical                   | 10 - mid temperate deciduous                   #
   # 3 - mid tropical                     | 11 - late temperate deciduous                  #
   # 4 - late tropical                    | 12:15 - agricultural PFTs                      #
   # 5 - temperate C3 grass               | 16 - Subtropical C3 grass                      #
   # 6 - northern pines                   |      (C4 grass with C3 photo).                 #
   # 7 - southern pines                   | 17 - "Araucaria" (non-optimised                #
   # 8 - late conifers                    |      Southern Pines).                          #
   #---------------------------------------------------------------------------------------#

  pftnums = data.table(
    pft     = c('CD','EH','Evergreen','Hydric','LC','LH','MC','NMH','NP','SMH','SP'),
    pft.num = c(  8 ,  9 ,        11 ,     10 ,  8 , 11 ,  8 ,  10 ,  6 ,  10 ,  7 ))

  dat = merge(dat, pftnums, all.x=T, by='pft')

  # Remove NA again
    dat = dat[!is.na(pft.num)]


# Reorder (for convenience only)
  setorder(dat, 'pft.num','diam.rebin')


# Summarize bins (maybe just for convenience?)
  bins = dat[, .(as.numeric(bin.diam[1]), as.numeric(bin.dens[1]), .N), by=bin]
    setnames(bins, c('dd','diam','dens','n'))
    n.bin = bins[,.N]  


# Tally density within every bin, by pft & diameter
  g = dat[, list(diam=mean(diam), dens=sum(dens), nrec=.N), by=.(bin, pft.num, diam.rebin)]
    setkey(g, 'bin')
    
    # Check
    g
    g[min(bin)]
    dat[ bin=="15_100" & pft=="CD" & diam.rebin==6]


# ----------- Create files
# --- Dirs and files
  out.dir.complete = file.path(out.dir, prefix)
  dir.create(out.dir.complete, recursive=T, showWarnings=F)
  
  file.names.pss   = paste0(out.dir.complete,"/psscss/",prefix,"_",bins$dd,".",loc.text,".pss")
  file.names.css   = paste0(out.dir.complete,"/psscss/",prefix,"_",bins$dd,".",loc.text,".css")
  file.names.ed2in = paste0(out.dir.complete,"/ed2in/ED2IN.",prefix,"_",bins$dd) 

# --- PSS
  # For now, create a single, identical PSS for each bin

  # Vars
  # These are essentially arbitrary since we really span many "patches" here, but maybe worth thinking about more sensible choices later.
  n.patch = 1
  area    = 1
  yr = 1999
  dst = 1   # Veg type/history (I think) (assumed 2ndary for now)
  age = 100
  soils = list(fsc=1.0,stsc=5.0,stsl=5.0,ssc=0.010,psc=0.0,msn=1.0,fsn=1.0)


  pss = data.frame(
    site  = rep(1,n.patch),           # Site (dummy for now)
    year  = rep(yr, n.patch),         # Year
    patch = 1:n.patch,          
    dst   = rep(dst, n.patch),        # Veg type/history (I think) (assumed 2ndary for now)
    age   = rep(age, n.patch),        # Age
    area  = area/sum(area,na.rm=T),   # Fractional area
    water = rep(0.1, n.patch),        # water (apparently not actually read)
    lapply(soils, rep, n.patch)       # Soil data: "fsc","stsc","stsl","ssc","psc","msn","fsn"
    )
  

  # Write output
  for(i in 1:n.bin) {
    write.table(pss,file.names.pss[i],quote=F,na="-9999",row.names=F,col.names=T)
  }


# --- CSS
  # For now, one cohort file for each diam/dens bin
  bins[order(n)][1:20]

  i=54
  for(i in 1:n.bin) {
    x = g[ bin==bins[i,dd] ]
    nc.i = x[,.N] # number of cohorts in this bin


      css = data.frame(
        year   = rep(yr, nc.i),           # Year
        patch  = rep(1,  nc.i),           # Patch index
        cohort = 1:nc.i,                  # Cohort-within-patch index
        dbh    = x$diam,                  # DBH
    #     hite   = trees$dbh/10,            # Height (why set = to dbh/10)? Maybe computed allometrically
        hite   = rep(0,nc.i),           # Computed allometrically, I assume
        pft    = x$pft.num,               # PFT
        n      = x$dens,              # Density
        bdead  = rep(0,nc.i),           # Structural C. Computed allometrically?
        balive = rep(0,nc.i),           # Ditto
        Avgrg  = rep(-9999,nc.i)        # Not used
        )
    
      # Remove NA rows
      css = css[apply(css,1,function(x) !any(is.na(x))),]

  # Write output
    write.table(css,file.names.css[i],quote=F,na="-9999",row.names=F,col.names=T)
  }



# --- ED2IN
  expnme.list  = paste0('\'', prefix, '_', bins$dd, '\'')
  ffilout.list = paste0('\'', ffilout.prefix, '/', prefix, '_', bins$dd, '\'')
  sfilout.list = paste0('\'', sfilout.prefix, '/', prefix, '_', bins$dd, '\'')
  sfilin.list  = paste0('\'', sfilin.prefix, '/', prefix, '_', bins$dd, '\'')

  ed2in = readLines(file.ed2in.template)

  i=1
  for(i in 1:n.bin) {
    ed2in.out = ed2in
    ed2in.out = gsub('@expnme@',  expnme.list[i], ed2in.out)
    ed2in.out = gsub('@ffilout@', ffilout.list[i], ed2in.out)
    ed2in.out = gsub('@sfilout@', sfilout.list[i], ed2in.out)
    ed2in.out = gsub('@sfilin@',  sfilin.list[i], ed2in.out)
    writeLines(ed2in.out, con=file.names.ed2in[i])
  }