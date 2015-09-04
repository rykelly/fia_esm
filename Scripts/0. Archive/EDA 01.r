rm(list=ls())
library(data.table)
library(bit64)

file.in1 = '/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/ESMraw.csv'
file.in2 = '/Work/Research/Macrosystems/FIA Phase Space/Data/ESMrawRKv03_trees.rds'
# file.in3 = '/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEditv01.csv'

# -------------------
dat1 = as.data.table(read.csv(file.in1, header=T, as.is=T))
  setnames(dat1, tolower(names(dat1)))
  dat1[, plt_cn := as.character(plt_cn)]
dat2 = as.data.table(readRDS(file.in2))
  setnames(dat2, tolower(names(dat2)))
  dat2[, plt_cn := as.character(plt_cn)]
  setkey(dat2, plt_cn)

q = dat2[which(plt_cn %in% dat1$plt_cn)]


dim(q)
dim(dat1)
dim(dat2)

dif1 = setdiff(dat1$plt_cn, dat2$plt_cn) # in 1, not 2
dif2 = setdiff(dat2$plt_cn, dat1$plt_cn) # in 2, not 1

length(dif1)
length(dif2)

pcnc = intersect(dat1$plt_cn, dat2$plt_cn) # in both
length(pcnc)

# number of records in each dataset that are in shared plots
sum(dat1$plt_cn %in% pcnc)
sum(dat2$plt_cn %in% pcnc)

# number of records in each that are in unique plots
  sum(dat1$plt_cn %in% dif1)
  sum(dat2$plt_cn %in% dif2)

# check all accounted for:
  sum(dat1$plt_cn %in% pcnc) + sum(dat1$plt_cn %in% dif1) == nrow(dat1)
  sum(dat2$plt_cn %in% pcnc) + sum(dat2$plt_cn %in% dif2) == nrow(dat2)


#  --- Find the non-overlapping plots and see why
  library(RPostgreSQL)
  source('/Work/Research/FIA/Scripts/Distribute/PSQL_utils.R')

  dbsettings = list(
    user     = "ryan",            # PSQL username
    password = "",                # PSQL password
    dbname   = "fia5_20150205",  # PSQL database name
    host     = "localhost",       # PSQL server address (don't change unless server is remote)
    driver   = 'PostgreSQL',      # DB driver (shouldn't need to change)
    write    = FALSE              # Whether to open connection with write access. 
  )

  fia.con = db.open(dbsettings)
  query = paste0('SELECT * FROM plot WHERE CAST(cn AS bigint) IN (', paste(pcnc,collapse=','), ')')
    p = as.data.table(db.query(query, con=fia.con))
  
  query = paste0('SELECT * FROM plot WHERE CAST(cn AS bigint) IN (', paste(dif1,collapse=','), ')')
    pd1 = as.data.table(db.query(query, con=fia.con))


  query = paste0('SELECT * FROM plot WHERE CAST(cn AS bigint) IN (', paste(dif2,collapse=','), ')')
    pd2 = as.data.table(db.query(query, con=fia.con))


  # 12 of T. Andrews' nonoverlapping plots (accounting for 342 records) simply don't exist in my fia5 database
  q1 = which(!(dif1 %in% p1$cn))
    length(q1)
    paste(dif1[q1])
    dat1[ PLT_CN %in% dif1[q1] ]
    dim(dat1[ PLT_CN %in% dif1[q1] ])

  # All of my nonoverlapping plots are in the database (no surprise).
  q2 = which(!(dif2 %in% p2$cn))
    length(q2)
    paste(dif2[q2])
  
  p1[, sort(unique(statecd))]
  p2[, sort(unique(statecd))]




  summary(p1)