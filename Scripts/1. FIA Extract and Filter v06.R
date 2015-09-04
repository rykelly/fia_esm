rm(list=ls())
library(data.table)
library(RPostgreSQL)
library(bit64)
# library(PEcAn.DB)
source('/Work/Research/FIA/Scripts/Distribute/PSQL_utils.R')

sumNA  = function(x) sum(x,na.rm=T)
meanNA = function(x) mean(x,na.rm=T)
maxNA  = function(x) max(x,na.rm=T)
tic = function() assign("timer", Sys.time(), envir=.GlobalEnv)
toc = function() print(Sys.time()-timer)
bigtime = Sys.time()

dbsettings = list(
  user     = "ryan",            # PSQL username
  password = "",                # PSQL password
  dbname   = "fia5_20150205",  # PSQL database name
  host     = "localhost",       # PSQL server address (don't change unless server is remote)
  driver   = 'PostgreSQL',      # DB driver (shouldn't need to change)
  write    = FALSE              # Whether to open connection with write access. 
)

lon.bounds = c(-95,999)
lat.bounds = c(-999,999)

file.pft = "/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/gcbPFT.csv"
# file.pft = "~/rykelly/PhaseSpace/gcbPFT.csv"

file.TA = '/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/ESMraw.rds'

file.out = "/Work/Research/Macrosystems/FIA Phase Space/Data/1.Filtered/ESMrawRKv09.rds"

# -----------------------------
  # Open connection to database
  fia.con = db.open(dbsettings)

  # ---------- PLOT & COND DATA
    # --- Query PLOT
      cat("Query PLOT...\n")
      query = paste('SELECT 
                cn, statecd, prev_plt_cn, remper
              FROM plot WHERE remper>3 AND remper<9.5 AND designcd=1 AND statecd<=56 AND ',
               'lon>', min(lon.bounds),' AND lon<', max(lon.bounds), ' AND ',
               'lat>', min(lat.bounds),' AND lat<', max(lat.bounds))

      tic() # ~10 sec
      PLOT = as.data.table(db.query(query, con=fia.con))
        setnames(PLOT, toupper(names(PLOT)))
        setnames(PLOT,"CN","PLT_CN")
        toc()

      # Remove states that haven't been resurveyed
        PLOT[, REMPERTOT := sumNA(REMPER), by=STATECD]
        PLOT = PLOT[ REMPERTOT>10, ]

      # Remove this one miscellaneous plot, per TA
        PLOT = PLOT[ PLT_CN!= 134680578010854 ]

      # Store statecd and state names
        states = sort(unique(PLOT$STATECD))
        n.state = length(states)
        surv = db.query('SELECT statecd,statenm FROM survey', con=fia.con)
        state.names = surv$statenm[match(states,surv$statecd)]


    # --- Query COND
      cat("Query COND...\n")
      query = paste('SELECT 
              plt_cn, condid, stdorgcd
            FROM cond WHERE 
              stdorgcd=0 AND ',
              'statecd IN (', paste(states,collapse=','), ')')

      tic() # ~ 15 sec
      COND = as.data.table(db.query(query, con=fia.con))
        setnames(COND, toupper(names(COND)))
        toc()
  
      # Remove all plots with more than 1 condition
        COND[, CONmax := maxNA(CONDID), by=PLT_CN]
        # *** RK: This is slightly wrong. In a few cases plots have CONDID>1, but still only have a single condition. This would work better:
          #     COND[, CONmax2 := .N, by=PLT_CN]
        COND = COND[ CONmax==1,]


    # --- Merge PLOT and COND
      cat("Merge PLOT and COND ...\n")
      tic()
      PC = merge(COND, PLOT, by="PLT_CN")
      toc()



  # ---------- RESURVEY DATA
    # --- Query
      cat("Query TREE_GRM_ESTN...\n")
      query = paste('SELECT 
                plt_cn, invyr, tpagrow_unadj, dia_begin, dia_end, component, tre_cn, remper, statecd
              FROM tree_grm_estn WHERE ',
#                 'dia_begin>5 AND ',
                'statecd IN (', paste(states,collapse=','),') AND ',
                'estn_type=\'AL\' AND land_basis=\'TIMBERLAND\'')

      tic()
      GRM = as.data.table(db.query(query, con=fia.con))
        setnames(GRM, toupper(names(GRM)))
        toc()

    # --- Filtering
      cat("Filtering TREE_GRM_ESTN...\n")

      # By plot/cond criteria
        GRM = GRM[ PLT_CN %in% PC$PLT_CN ]
    
      # Assign GRM$START + GRM$CUT and restrict to cut==0, start>0
        GRM[, START      := INVYR - REMPER                                  ]
          GRM[, REMPER := NULL]
        GRM[, CUT1TPA    := (COMPONENT=="CUT1") * TPAGROW_UNADJ             ]
        GRM[, CUT2TPA    := (COMPONENT=="CUT2") * TPAGROW_UNADJ             ]
        GRM[, CUT        := sumNA(CUT2TPA + CUT1TPA), by=PLT_CN             ]
        GRM = GRM[ START>0 & CUT==0, ]

      # Assign Reversion/Diversion, and exclude plots with either
        GRM[, DIVERSION1TPA  := (COMPONENT=="DIVERSION1") * TPAGROW_UNADJ   ]
        GRM[, DIVERSION2TPA  := (COMPONENT=="DIVERSION2") * TPAGROW_UNADJ   ]
        GRM[, REVERSION1TPA  := (COMPONENT=="REVERSION1") * TPAGROW_UNADJ   ]
        GRM[, REVERSION2TPA  := (COMPONENT=="REVERSION2") * TPAGROW_UNADJ   ]
        GRM[, REDIV          := sumNA(REVERSION2TPA+REVERSION1TPA+DIVERSION2TPA+DIVERSION1TPA), by=PLT_CN]
        GRM = GRM[ REDIV==0, ] 

      # Assign SURVIVORTPA, and remove records from any state with <1000 measured trees
        GRM[, SURVIVORTPA    := (COMPONENT=="SURVIVOR") * TPAGROW_UNADJ     ]
        GRM[, TPATOT         := sumNA(SURVIVORTPA), by=STATECD              ]
        GRM = GRM[ TPATOT>1000, ]


    # --- Assign additional variables
      cat("Calculating TPA and Diameter...\n")
      # Compute TPA
        GRM[, INGROWTHTPA    := (COMPONENT=="INGROWTH") * TPAGROW_UNADJ     ]
        GRM[, MORTALITY1TPA  := (COMPONENT=="MORTALITY1") * TPAGROW_UNADJ   ]
        GRM[, MORTALITY2TPA  := (COMPONENT=="MORTALITY2") * TPAGROW_UNADJ   ]
        GRM[, MORTALITYTPA   := MORTALITY1TPA + MORTALITY2TPA               ]

        # Initial number of trees is current survivors plus those that died during the resurvey period.
        GRM[, start1tpa      := SURVIVORTPA + MORTALITYTPA                  ]
        GRM[, PREVTPAsum     := sumNA(start1tpa), by=PLT_CN                 ]  # "startsumTPA"

        # Final number of trees is current survivors plus new trees that cross the 5" threshold
        GRM[, end1tpa        := SURVIVORTPA + INGROWTHTPA                   ]
        GRM[, TPAsum         := sumNA(end1tpa), by=PLT_CN                   ]  # "endsumTPA"

      # Compute plot mean diameters
        GRM[, PREVDIAmean    := meanNA(DIA_BEGIN), by=PLT_CN                ]  # "DIAbeginmean"
        GRM[, DIAmean        := meanNA(DIA_END),   by=PLT_CN                ]  # "DIAendmean"


    # --- Subset for output
      GRM.out = GRM[, .(PLT_CN, TRE_CN, PREVTPAsum, TPAsum, PREVDIAmean, DIAmean)]


  # ---------- TREE
  cat("Query TREE...\n")
    # --- Query
      query = paste('SELECT 
              cn, prev_tre_cn, plt_cn, invyr, condid, dia, tpa_unadj, spcd, stocking, statuscd, 
              prevdia, prev_status_cd, p2a_grm_flg, reconcilecd
            FROM tree WHERE 
              (prevdia>5 OR dia>5) AND (statuscd=1 OR prev_status_cd=1) AND p2a_grm_flg!=\'N\'  AND
              statecd IN (', paste(states,collapse=','), ')')

      tic() # ~ 10 min
      TREE = as.data.table(db.query(query, con=fia.con))
        setnames(TREE, toupper(names(TREE)))
      toc()

    # --- Filter TREE
      cat("Filter TREE ...\n")
      # By plot/cond criteria
        TREE = TREE[ PLT_CN %in% PC$PLT_CN ]
           
      # CONDID ("Remove edge effects" --TA)
        TREE[, CONmax := maxNA(CONDID), by=PLT_CN]

      # STATUSCD
        # *** RK: Next line looks wrong. It's a sum, not max, despite the name. I did rewrite the line but this is equivalent to what Travis had so keeping for now.
        TREE[, STATUSCDmax := sumNA(3*as.integer(STATUSCD==3)), by=PLT_CN]

      # RECONCILECD
        TREE[is.na(RECONCILECD), RECONCILECD :=0] # Set NA values to 0 (unused)

      # Filter
      TREE = TREE[ CONmax==1 & INVYR<2014 & STATUSCDmax!=3 & STATUSCD!=0 & RECONCILECD<=4 ]
 

    # --- Merge in PFTS
      cat("Merge in PFTs...\n")
      tic() # ~ 1.5 min
      MCDPFT = as.data.table(read.csv(file.pft, header = TRUE))
      TREE = merge(TREE, MCDPFT, all.x=T, by = "SPCD")
      toc()


    # --- Connect PREV_CN for each tree prior to subset
      cat("Connect consecutive observations...\n")
      tic() # ~20 sec
        TREE.prev = TREE[,.(CN, STOCKING, SPCD, TPA_UNADJ, PFT)]
        setnames(TREE.prev, paste0("PREV_TRE_",names(TREE.prev))) 
      toc()

      # Convert PREV_TRE_CN columns to integer64 (have experienced crashes otherwise. memory leak?)
        TREE.prev[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]
        TREE[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]
  
      tic()
      TREE = merge(TREE, TREE.prev, all.x=T, by="PREV_TRE_CN")
        setnames(TREE,"CN","TRE_CN")
      toc()
  
  
    # --- Define DIA and STOCKING columns for trees >5"
      cat("Calculate DIA and STOCKING...\n")
      # DIAmean of DIA>5
      TREE[DIA>=5 & STATUSCD==1,                                   DIA5alive := DIA      ]
        TREE[, DIA5meanalive     := meanNA(DIA5alive), by=PLT_CN                         ]
      TREE[PREVDIA>=5 & PREV_STATUS_CD==1,                     PREVDIA5alive := PREVDIA  ]
        TREE[, PREVDIA5meanalive := meanNA(PREVDIA5alive), by=PLT_CN                     ]

      #Stocking of plots for trees with DIA>5
      TREE[DIA5alive>0, STOCKING5 := STOCKING]
      TREE[, STOCKING5mid := sumNA(STOCKING5), by=PLT_CN]
      TREE[PREVDIA5alive>0, PREVSTOCKING5 := PREV_TRE_STOCKING]
      TREE[, PREVSTOCKING5mid := sumNA(PREVSTOCKING5), by=PLT_CN]


# ---------- MERGE
  cat("Final merge...\n")
  ALL = merge(GRM, TREE, all.x=T, by='TRE_CN')
    ALL[, c("PLT_CN.x","INVYR.x") := list(NULL,NULL)]
    setnames(ALL, c("PLT_CN.y","INVYR.y"), c("PLT_CN","INVYR"))
  ALL = merge(ALL, PC, by='PLT_CN')
    ALL[, c("STATECD.x","CONmax.x") := list(NULL,NULL)]
    setnames(ALL, c("STATECD.y","CONmax.y"), c("STATECD","CONmax"))
    setnames(ALL, "START", "PREVYR")

#   ALL = merge(GRM, PC, by='PLT_CN')
#     setnames(ALL, "START", "PREVYR")

  # --- Flag plots that aren't in T. Andrew's file
    dat.TA = as.data.table(readRDS(file.TA))
      pcn.TA = as.character(dat.TA$PLT_CN)
      ALL[, in.TA := 0]
        ALL[ PLT_CN %in% pcn.TA, in.TA := 1 ]



# --- Save outputs
  cat("Save...\n")
  tic()
    saveRDS(ALL, file = file.out)
  toc()


db.close(fia.con)

print(Sys.time()-bigtime)

# ---------- Additional filtering? Seems unnecessary. 
# # PREVSTOCKING5mid>0
# *** This doesn't seem right. PREVSTOCKING5mid==0 for numerous records that have nonzero DIA_BEGIN and start1tpa. This is usually (always?) because they have PREVSTOCKING5=NA. However, since these records clearly had trees at the previous survey, makes no sense to omit them. There are a lot in this category (~1e6). 
#   ind = ALL[,which(PREVSTOCKING5mid<=0)]
#   length(ind)
#   ALL[ind, .(DIA_BEGIN, start1tpa, PREVSTOCKING5, PREVSTOCKING5mid)]

# # PREV_PLT_CN>0
# *** As above, this seems to rule out records in error. Many records have previous dia/tpa measurements but PREV_PLT_CN=="".
#   ind = ALL[,which(PREV_PLT_CN<=0)]
#   length(ind)
#   ALL[ind, .(DIA_BEGIN, start1tpa, PREV_PLT_CN)]




# ---------- DIAGNOSTICS
# q = merge(GRM,TREE, by='TRE_CN', select=c("DIA_BEGIN","DIA_END","PREVDIAmean","DIAmean","DIA","PREVDIA","DIA5alive","DIA5meanalive","PREVDIA5alive","PREVDIA5meanalive"))
# 
# n=100000
# plot(q$DIA_BEGIN[1:n], q$PREVDIA[1:n])
# plot(q$DIA_END[1:n], q$DIA[1:n])
# plot(q$DIA_BEGIN[1:n], q$PREVDIA5alive[1:n])
# plot(q$DIA_END[1:n], q$DIA5alive[1:n])
# plot(q$PREVDIAmean[1:n], q$PREVDIA5meanalive[1:n])
# plot(q$DIAmean[1:n], q$DIA5meanalive[1:n])
# 
# n=100000
# 
# hist(q$DIA_BEGIN[1:n]-q$PREVDIA[1:n], 1000)
# hist(q$DIA_END[1:n]-q$DIA[1:n], 1000)
# hist(q$DIA_BEGIN[1:n]-q$PREVDIA5alive[1:n], 1000)
# hist(q$DIA_END[1:n]-q$DIA5alive[1:n], 1000)
# hist(q$PREVDIAmean[1:n]-q$PREVDIA5meanalive[1:n], 1000)
# hist(q$DIAmean[1:n]-q$DIA5meanalive[1:n], 1000)
# 
# mean(q$DIA_BEGIN[1:n]-q$PREVDIA[1:n], na.rm=T)
# mean(q$DIA_END[1:n]-q$DIA[1:n], na.rm=T)
# mean(q$DIA_BEGIN[1:n]-q$PREVDIA5alive[1:n], na.rm=T)
# mean(q$DIA_END[1:n]-q$DIA5alive[1:n], na.rm=T)
# mean(q$PREVDIAmean[1:n]-q$PREVDIA5meanalive[1:n], na.rm=T)
# mean(q$DIAmean[1:n]-q$DIA5meanalive[1:n], na.rm=T)
# 
# 
# 
# # Note: DIA_END from GRM is the same as DIA from TREE. However, DIA_BEGIN is not necessarily the same as PREVDIA