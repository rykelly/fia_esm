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

file.TA = '/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/ESMraw.csv'

# -----------------------------
# ----- RESURVEY GROWTH DATA PLOTS
  cat("Resurvey growth data plots...\n")
  # Find state codes
    # Open connection to database
    fia.con = db.open(dbsettings)

    # Query state codes within lat/lon bounds, and restricted to states only (statecd≥56 = territories)
      query = paste0('SELECT statecd FROM plot WHERE statecd<=56 AND ',
                     'lon>', min(lon.bounds),' AND lon<', max(lon.bounds), ' AND ',
                     'lat>', min(lat.bounds),' AND lat<', max(lat.bounds))
      plots = db.query(query, con=fia.con)
      states = sort(unique(plots$statecd))
      n.state = length(states)

    # Get state names too
      surv = db.query('SELECT statecd,statenm FROM survey', con=fia.con)
      state.names = surv$statenm[match(states,surv$statecd)]

  # Query
#     query = paste('SELECT 
#               plt_cn, invyr, remper, tpagrow_unadj,tparemv_unadj, tpamort_unadj, 
#               ann_net_growth, removals, mortality, dia_begin, dia_end, component, 
#               dia_begin_recalc, statecd, tre_cn
#             FROM tree_grm_estn WHERE statecd IN (', paste(states,collapse=','), 
#             ') AND estn_type=\'AL\' AND land_basis=\'TIMBERLAND\'')
    query = paste('SELECT 
              plt_cn, invyr, remper, tpagrow_unadj, dia_begin, dia_end, component, statecd, tre_cn
            FROM tree_grm_estn WHERE statecd IN (', paste(states,collapse=','), 
            ') AND estn_type=\'AL\' AND land_basis=\'TIMBERLAND\'')

    tic()
    RM = as.data.table(db.query(query, con=fia.con))
      setnames(RM, toupper(names(RM)))
      toc()


  # --- Assign variables
  cat("Modify RM...\n")
    # # Assign RM$START + RM$CUT and restrict to cut==0, start>0
      RM[, START      := INVYR - REMPER                                  ]
      RM[, CUT1TPA    := (COMPONENT=="CUT1") * TPAGROW_UNADJ             ]
      RM[, CUT2TPA    := (COMPONENT=="CUT2") * TPAGROW_UNADJ             ]
      RM[, CUT        := sumNA(CUT2TPA + CUT1TPA), by=PLT_CN             ]
      RM = RM[ START>0 & CUT==0, ]

    # Assign Reversion/Diversion, and exclude plots with either
      RM[, DIVERSION1TPA  := (COMPONENT=="DIVERSION1") * TPAGROW_UNADJ   ]
      RM[, DIVERSION2TPA  := (COMPONENT=="DIVERSION2") * TPAGROW_UNADJ   ]
      RM[, REVERSION1TPA  := (COMPONENT=="REVERSION1") * TPAGROW_UNADJ   ]
      RM[, REVERSION2TPA  := (COMPONENT=="REVERSION2") * TPAGROW_UNADJ   ]
      RM[, REDIV          := sumNA(REVERSION2TPA+REVERSION1TPA+DIVERSION2TPA+DIVERSION1TPA), by=PLT_CN]
      RM = RM[ REDIV==0, ] 

    # Assign SURVIVORTPA, and remove records from any state with <1000 measured trees
      RM[, SURVIVORTPA    := (COMPONENT=="SURVIVOR") * TPAGROW_UNADJ     ]
      RM[, TPATOT         := sumNA(SURVIVORTPA), by=STATECD              ]
      RM = RM[ TPATOT>1000, ]

    # Assign additional variables
      RM[, INGROWTHTPA    := (COMPONENT=="INGROWTH") * TPAGROW_UNADJ     ]
      RM[, MORTALITY1TPA  := (COMPONENT=="MORTALITY1") * TPAGROW_UNADJ   ]
      RM[, MORTALITY2TPA  := (COMPONENT=="MORTALITY2") * TPAGROW_UNADJ   ]
      RM[, MORTALITYTPA   := MORTALITY1TPA + MORTALITY2TPA               ]
#       RM[, PREVDIAmean    := DIA_BEGIN                                   ]
#       RM[, DIAmean        := DIA_END                                     ]
      RM[, PREVDIAmean    := meanNA(DIA_BEGIN), by=PLT_CN                ]  # "DIAbeginmean"
      RM[, DIAmean        := meanNA(DIA_END),   by=PLT_CN                ]  # "DIAendmean"


      # Initial number of trees is current survivors plus those that died during the resurvey period.
      RM[, start1tpa      := SURVIVORTPA + MORTALITYTPA                  ]
      RM[, PREVTPAsum     := sumNA(start1tpa), by=PLT_CN                 ]  # "startsumTPA"

      # Final number of trees is current survivors plus new trees that cross the 5" threshold
      RM[, end1tpa        := SURVIVORTPA + INGROWTHTPA                   ]
      RM[, TPAsum         := sumNA(end1tpa), by=PLT_CN                   ]  # "endsumTPA"


  # --- Subset for output?
#     S3RM = RM[, .(PLT_CN, TRE_CN, PREVTPAsum, TPAsum, PREVDIAmean, DIAmean)]


# ----- DATA ----------------------------------
  cat("Query PLOT, COND, and TREE...\n")
  # --- Query PLOT
#     query = paste('SELECT 
#               cn, lat, lon, elev, ecosubcd, statecd, prev_plt_cn, remper, designcd, 
#               kindcd, microplot_loc
#             FROM plot WHERE statecd<=56 AND ',
#              'lon>', min(lon.bounds),' AND lon<', max(lon.bounds), ' AND ',
#              'lat>', min(lat.bounds),' AND lat<', max(lat.bounds))
    query = paste('SELECT 
              cn, statecd, prev_plt_cn, remper, designcd
            FROM plot WHERE statecd<=56 AND ',
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


  # --- Query COND
#     query = paste('SELECT 
#             plt_cn,condid, forindcd, stdage, stdszcd, fldszcd, fldtypcd,siteclcd, slope, 
#             physclcd, gsstkcd,alstk,alstkcd, gsstk, dstrbcd1,dstrbyr1,dstrbcd2,dstrbyr2, 
#             trtcd1, trtyr1,trtcd2,trtyr2,trtcd3,trtyr3,presnfcd, dstrbcd3, dstrbyr3, 
#             balive, carbon_down_dead,stdorgcd, sisp, statecd
#           FROM cond WHERE statecd IN (', paste(states,collapse=','), ')')
    query = paste('SELECT 
            plt_cn, condid, stdage, stdorgcd
          FROM cond WHERE statecd IN (', paste(states,collapse=','), ')')

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
    tic()
    PC = merge(COND, PLOT, all.x=TRUE, by="PLT_CN")
    toc()


  # --- Query TREE
#       query = paste('SELECT 
#               cn, prev_tre_cn, plt_cn, invyr,condid, dia, tpa_unadj, ht, agentcd, damtyp1, 
#               decaycd, spcd, stocking, statuscd, standing_dead_cd, spgrpcd, cr, cclcd, 
#               reconcilecd, diacheck, diahtcd, prevdia, prev_status_cd, p2a_grm_flg, 
#               carbon_ag, carbon_bg, plot, tree, statecd
#             FROM tree WHERE statecd IN (', paste(states,collapse=','), ')')

      query = paste('SELECT 
              cn, prev_tre_cn, plt_cn, invyr, condid, dia, tpa_unadj, spcd, stocking, statuscd, 
              reconcilecd, prevdia, prev_status_cd, p2a_grm_flg
            FROM tree WHERE statecd IN (', paste(states,collapse=','), ')')

      tic() # ~ 10 min
      TREE = as.data.table(db.query(query, con=fia.con))
        setnames(TREE, toupper(names(TREE)))
      toc()

    db.close(fia.con)


  # --- Modify TREE
    cat("Modify TREE variables...\n")
    TREE[is.na(RECONCILECD), RECONCILECD := 0]
    TREE[, PREVDIAna := PREVDIA]
      TREE[is.na(PREVDIAna), PREVDIAna := 0]
    TREE[, PREVSTATna := PREV_STATUS_CD]
      TREE[is.na(PREVSTATna), PREVSTATna := 0]
    TREE[, CONmax := maxNA(CONDID), by=PLT_CN]
    TREE[, PLTnb := as.integer(STATUSCD==1)]
    TREE[, PLTn := sumNA(PLTnb), by=PLT_CN]  
    TREE[, PrevPLTnb := as.integer(PREV_STATUS_CD==1)]
    TREE[, PrevPLTn := sumNA(PrevPLTnb), by=PLT_CN]

    # *** RK: Next line looks wrong. It's a sum, not max, despite the name. I did rewrite the line but this is equivalent to what Travis had so keeping for now.
    TREE[, STATUSCDmax := sumNA(3*as.integer(STATUSCD==3)), by=PLT_CN]

 
    # Remove edge effects
    TREE1a = TREE[ CONmax==1 & INVYR<2014 ]
      # RK: Note TREE not used again

    MCDPFT = as.data.table(read.csv(file.pft, header = TRUE))

    tic() # ~ 1.5 min
    TREE1 = merge(TREE1a, MCDPFT, all.x=T, by = "SPCD")
      # RK: Note TREE1a not used again
      rm(TREE1a)
    toc()
  
    tic() # ~ 1.5 min
    TCPb  = merge(TREE1, PC, by="PLT_CN")
      # RK: Note TREE1 not used again
      rm(TREE1)
    toc()


  # --- Connect PREV_CN for each tree prior to subset
    tic() # ~20 sec
    TREE3 = TCPb[,.(CN, STOCKING, SPCD, TPA_UNADJ, PFT)]
      setnames(TREE3, paste0("PREV_TRE_",names(TREE3))) 
    toc()

    # Convert PREV_TRE_CN columns to integer64 (have experienced crashes otherwise. memory leak?)
      TREE3[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]
      TCPb[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]
  
    tic()
    TCP = merge(TCPb, TREE3, all.x=T, by="PREV_TRE_CN")
      setnames(TCP,"CN","TRE_CN")
    toc()
  

  # --- Additional mods to TCP
    # Remove plots that have not been resurveyed and those surveyed over unusually long or short periods or were harvested,  STATUSCD!=0:remove trees not resurveyed due to RECONCILECD = 5-9
    TCP = TCP[REMPER>3 & REMPER<9.5 & STATUSCDmax!=3 & STATUSCD!=0 & DESIGNCD==1, ]
  
    #fill in missing with current age minus remeasurement period
#     TCP[, PREV_TRE_STDAGE := as.numeric(PREV_TRE_STDAGE)]
#     TCP[is.na(PREV_TRE_STDAGE), PREV_TRE_STDAGE := STDAGE - REMPER]

    ##DIAmean of DIA>5
    TCP[DIA>=5 & STATUSCD==1 & P2A_GRM_FLG!="N", DIA5alive               := DIA      ]
      TCP[, DIA5meanalive     := meanNA(DIA5alive), by=PLT_CN                        ]
    TCP[PREVDIA>=5 & PREV_STATUS_CD==1 & P2A_GRM_FLG!="N", PREVDIA5alive := PREVDIA  ]
      TCP[, PREVDIA5meanalive := meanNA(PREVDIA5alive), by=PLT_CN                    ]

    #Stocking of plots for trees with DIA>5
    TCP[DIA5alive>0, STOCKING5 := STOCKING]
    TCP[, STOCKING5mid := sumNA(STOCKING5), by=PLT_CN]
    TCP[PREVDIA5alive>0, PREVSTOCKING5 := PREV_TRE_STOCKING]
    TCP[, PREVSTOCKING5mid := sumNA(PREVSTOCKING5), by=PLT_CN]


  # --- Assign columns for PFTs from ED2
#     cat("Assign PFT vars...\n")
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "EH", EHstock := STOCKING]
#     TCP[, EHstocksum := sumNA(EHstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "EH", 
#         PREVEHstock := PREV_TRE_STOCKING]
#     TCP[, PREVEHstocksum := sumNA(PREVEHstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "Evergreen", Evergreenstock := STOCKING]
#     TCP[, Evergreenstocksum := sumNA(Evergreenstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "Evergreen", 
#         PREVEvergreenstock := PREV_TRE_STOCKING]
#     TCP[, PREVEvergreenstocksum := sumNA(PREVEvergreenstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "Hydric", Hydricstock := STOCKING]
#     TCP[, Hydricstocksum := sumNA(Hydricstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "Hydric", 
#         PREVHydricstock := PREV_TRE_STOCKING]
#     TCP[, PREVHydricstocksum := sumNA(PREVHydricstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "LC", LCstock := STOCKING]
#     TCP[, LCstocksum := sumNA(LCstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "LC", 
#         PREVLCstock := PREV_TRE_STOCKING]
#     TCP[, PREVLCstocksum := sumNA(PREVLCstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "LH", LHstock := STOCKING]
#     TCP[, LHstocksum := sumNA(LHstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "LH", 
#         PREVLHstock := PREV_TRE_STOCKING]
#     TCP[, PREVLHstocksum := sumNA(PREVLHstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "MC", MCstock := STOCKING]
#     TCP[, MCstocksum := sumNA(MCstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "MC", 
#         PREVMCstock := PREV_TRE_STOCKING]
#     TCP[, PREVMCstocksum := sumNA(PREVMCstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "NMH", NMHstock := STOCKING]
#     TCP[, NMHstocksum := sumNA(NMHstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "NMH", 
#         PREVNMHstock := PREV_TRE_STOCKING]
#     TCP[, PREVNMHstocksum := sumNA(PREVNMHstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "NP", NPstock := STOCKING]
#     TCP[, NPstocksum := sumNA(NPstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "NP", 
#         PREVNPstock := PREV_TRE_STOCKING]
#     TCP[, PREVNPstocksum := sumNA(PREVNPstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "SMH", SMHstock := STOCKING]
#     TCP[, SMHstocksum := sumNA(SMHstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "SMH", 
#         PREVSMHstock := PREV_TRE_STOCKING]
#     TCP[, PREVSMHstocksum := sumNA(PREVSMHstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "SP", SPstock := STOCKING]
#     TCP[, SPstocksum := sumNA(SPstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "SP", 
#         PREVSPstock := PREV_TRE_STOCKING]
#     TCP[, PREVSPstocksum := sumNA(PREVSPstock), by=PLT_CN]
# 
#     TCP[STATUSCD==1 & DIA>=5 & P2A_GRM_FLG!="N" & PFT == "CD", CDstock := STOCKING]
#     TCP[, CDstocksum := sumNA(CDstock), by=PLT_CN]
#     TCP[PREV_STATUS_CD==1 & PREVDIA>=5 & P2A_GRM_FLG!="N" & PREV_TRE_PFT == "CD", 
#         PREVCDstock := PREV_TRE_STOCKING]
#     TCP[, PREVCDstocksum := sumNA(PREVCDstock), by=PLT_CN]
# 
# 
#     TCP[, LCstocksum := LCstocksum + CDstocksum]
#     TCP[, PREVLCstocksum := PREVLCstocksum + PREVCDstocksum]

  # --- Final Subset
#     S3 = TCP[ STDORGCD==0 & PREVSTOCKING5mid>0 & PLT_CN!= 134680578010854 & 
#               !duplicated(PLT_CN) & PREV_PLT_CN>0,
#             .(PLT_CN, PREVSTOCKING5mid, EHstocksum, PREVEHstocksum, Evergreenstocksum, PREVEvergreenstocksum, Hydricstocksum, PREVHydricstocksum,LCstocksum, PREVLCstocksum,LHstocksum, PREVLHstocksum, MCstocksum, PREVMCstocksum,NMHstocksum, PREVNMHstocksum,NPstocksum, PREVNPstocksum,SMHstocksum, PREVSMHstocksum,SPstocksum, PREVSPstocksum)
#             ]

    S3 = TCP[ STDORGCD==0 & PREVSTOCKING5mid>0 & PLT_CN!= 134680578010854 & 
              !duplicated(PLT_CN) & PREV_PLT_CN>0, ]


# --- Final merging and binning
  cat("Merge and bin...\n")
#   S4 = merge(S3RM, S3, by=c("PLT_CN"))
  S4 = merge(RM, S3, by=c("PLT_CN"))
  
  F2 = S4[PREVSTOCKING5mid>0, ]

  F2[, PREVDIA := PREVDIAmean  ]
  F2[, DIA     := DIAmean      ]


  #average means for binning
  F2[, PREVDIAmean  := meanNA(PREVDIA), by=PLT_CN]
  F2[, PREVDIAbin   := ifelse(PREVDIAmean<=14, 
                              ceiling(PREVDIAmean),
                              ceiling(PREVDIAmean/2)*2        )]
    F2[PREVDIAbin   > 20  , PREVDIAbin   := 20  ]
  F2[, PREVSTOCKbin := ifelse(PREVDIAmean<=14, 
                              ceiling(PREVSTOCKING5mid/10)*10,
                              ceiling(PREVSTOCKING5mid/20)*20 )]
    F2[PREVSTOCKbin > 110 , PREVSTOCKbin := 110 ]

  F2[, PREVDIASTOCKbin := PREVDIAbin * 10000 + PREVSTOCKbin]
  # F3 = F2[ PREVDIASTOCKbin>0, .(PREVDIAbin, PREVSTOCKbin, DIAmean, PREVDIAmean )]

#   F3 = F2[ PREVDIASTOCKbin>0,
#   c("PLT_CN","PREVTPAsum","TPAsum","PREVSTOCKING5mid","EHstocksum","PREVEHstocksum","Evergreenstocksum","PREVEvergreenstocksum","Hydricstocksum","PREVHydricstocksum","LCstocksum","PREVLCstocksum","LHstocksum","PREVLHstocksum","MCstocksum","PREVMCstocksum","NMHstocksum","PREVNMHstocksum","NPstocksum","PREVNPstocksum","SMHstocksum","PREVSMHstocksum","SPstocksum","PREVSPstocksum","PREVDIA","DIA","PREVDIASTOCKbin"), with=F]


# --- Restrict to plots in T. Andrew's file
  dat.TA = as.data.table(read.csv(file.TA, header=T, as.is=T))
    dat.TA[, PLT_CN := as.character(PLT_CN)]

  q = F2[which(PLT_CN %in% dat.TA$PLT_CN)]



# --- Save outputs
  cat("Save...\n")
  tic()
    saveRDS(q, file = "/Work/Research/Macrosystems/FIA Phase Space/Data/ESMrawRKv03_trees.rds")
  toc()
  tic()
    save.image('/Work/Research/Macrosystems/FIA Phase Space/Data/ESMrawRKv03_workspace.rdata')
  toc()