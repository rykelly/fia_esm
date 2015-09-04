rm(list=ls())
# library(ncdf)
# library(chron)
# library(sp)
library(data.table)
# library(plyr)
# library(reshape2)
# library(foreach)
# library(doParallel)
# library(doMC)
# registerDoMC(cores=4)  #set number of CPU cores to use

# options(scipen=999) #remove sci notation

##open script in R directly   > source("/Volumes/m-z/tda210/USFS/NullpaperPAReq.R")

#####################################################################################################################################################################
############ RESURVEY GROWTH DATA PLOTS ###

#load tree succession data
#PAsuccess<-as.data.table(read.csv("/Volumes/m-z/tda210/USFS/pasuccess.csv", header = TRUE, sep = ",", quote="\"", dec="."))

#State_TREE_GRM_ESTN.CSV files must be downloaded from the FIA website (http://apps.fs.fed.us/fiadb-downloads/datamart.html) to a local drive
# RMfiles  <-  list.files("/Users/robertbooth/Documents/TDA210/USFSFIA/GRM", pattern="*ESTN.CSV", full.names=TRUE) #get file names

file.pft = "/Work/Research/Macrosystems/FIA Phase Space/From T. Andrews/gcbPFT.csv"

library(RPostgreSQL)
# library(PEcAn.DB)
source('/Work/Research/FIA/Scripts/Distribute/PSQL_utils.R')
sumNA  = function(x) sum(x,na.rm=T)
meanNA = function(x) mean(x,na.rm=T)
maxNA  = function(x) max(x,na.rm=T)
tic = function() assign("timer", Sys.time(), envir=.GlobalEnv)
toc = function() print(Sys.time()-timer)

# dbsettings = list(
#   user     = "bety",            # PSQL username
#   password = "",                # PSQL password
# #   dbname   = "fia5data_csvimport10",  # PSQL database name
#   dbname   = "fia5",  # PSQL database name
#   host     = "psql-pecan.bu.edu",       # PSQL server address (don't change unless server is remote)
#   driver   = 'PostgreSQL',      # DB driver (shouldn't need to change)
#   write    = FALSE              # Whether to open connection with write access. 
# )

dbsettings = list(
  user     = "ryan",            # PSQL username
  password = "",                # PSQL password
#   dbname   = "fia5data_csvimport10",  # PSQL database name
  dbname   = "fia5_20150205",  # PSQL database name
  host     = "localhost",       # PSQL server address (don't change unless server is remote)
  driver   = 'PostgreSQL',      # DB driver (shouldn't need to change)
  write    = FALSE              # Whether to open connection with write access. 
)

# S3RM  <-  data.table(NULL) #set up space for rbind of state data
# t  <- length(RMfiles)
# print("script starting")  #for loop that opens state data, calculates variables, then adds it to data from previous states. 
# S3RM   <-  foreach(i=1:t, .combine=rbind, .inorder=FALSE) %dopar%
# {


# Find state codes
  # Open connection to database
  fia.con = db.open(dbsettings)

  # Query state codes within lat/lon bounds, and restricted to states only (statecd≥56 = territories)
    query = 'SELECT statecd FROM plot WHERE statecd<=56 AND lon>-95'
    plots = db.query(query, con=fia.con)
    states = sort(unique(plots$statecd))
    n.state = length(states)

  # Get state names too
    surv = db.query('SELECT statecd,statenm FROM survey', con=fia.con)
    state.names = surv$statenm[match(states,surv$statecd)]


# Query
  query = paste('SELECT 
            plt_cn, invyr, remper, tpagrow_unadj,tparemv_unadj, tpamort_unadj, 
            ann_net_growth, removals, mortality, dia_begin, dia_end, component, 
            dia_begin_recalc, statecd
          FROM tree_grm_estn WHERE statecd IN (', paste(states,collapse=','), 
          ') AND estn_type=\'AL\' AND land_basis=\'TIMBERLAND\'')

  tic()
  RM = as.data.table(db.query(query, con=fia.con))
    setnames(RM, toupper(names(RM)))
    toc()

    # 
#      		RM   <-  subset(as.data.table(read.csv(RMfiles[i], header = TRUE, sep = ",", quote="\"", dec=".")), ESTN_TYPE=="AL" 
#           	& LAND_BASIS=="TIMBERLAND", select=c(PLT_CN, INVYR, REMPER,TPAGROW_UNADJ,TPAREMV_UNADJ, 
# 			TPAMORT_UNADJ, ANN_NET_GROWTH, REMOVALS, MORTALITY, DIA_BEGIN, DIA_END, COMPONENT, DIA_BEGIN_RECALC)) 
# 		
			RM$SURVIVORTPA  <-   ifelse(RM$COMPONENT=="SURVIVOR", RM$TPAGROW_UNADJ,0) 
						
			
#   	if(sum(RM$SURVIVORTPA)>1000) #states must have at least ~150 measured trees, else move to next state
# 		{ 
		RM$start  <-  RM$INVYR-RM$REMPER
		RM$CUT1TPA  <-   ifelse(RM$COMPONENT=="CUT1", RM$TPAGROW_UNADJ,0) 
		RM$CUT2TPA  <-   ifelse(RM$COMPONENT=="CUT2", RM$TPAGROW_UNADJ,0) 
		RM$CUT  <-  RM$CUT2TPA+RM$CUT1TPA
		RM$DIVERSION1TPA  <-   ifelse(RM$COMPONENT=="DIVERSION1", RM$TPAGROW_UNADJ,0) 
		RM$DIVERSION2TPA  <-   ifelse(RM$COMPONENT=="DIVERSION2", RM$TPAGROW_UNADJ,0) 
		RM$INGROWTHTPA  <-   ifelse(RM$COMPONENT=="INGROWTH", RM$TPAGROW_UNADJ,0) 
		RM$MORTALITY1TPA  <-   ifelse(RM$COMPONENT=="MORTALITY1", RM$TPAGROW_UNADJ,0) 
		RM$MORTALITY2TPA  <-   ifelse(RM$COMPONENT=="MORTALITY2", RM$TPAGROW_UNADJ,0)
		RM$MORTALITYTPA  <-  RM$MORTALITY1TPA+RM$MORTALITY2TPA
		RM$REVERSION1TPA  <-   ifelse(RM$COMPONENT=="REVERSION1", RM$TPAGROW_UNADJ,0) 
		RM$REVERSION2TPA  <-   ifelse(RM$COMPONENT=="REVERSION2", RM$TPAGROW_UNADJ,0) 
		RM$REDIV  <-  RM$REVERSION2TPA+RM$REVERSION1TPA+RM$DIVERSION2TPA+RM$DIVERSION1TPA
		RM$REDIV  <-  ave(RM$REDIV, RM$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE))
		RM$cut  <-  ave(RM$CUT, RM$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE))

		RM  <-  subset(RM, REDIV==0) #exclude plots with reversion or diversion
		RM$PREVDIAmean  <- RM$DIA_BEGIN
		RM$DIAmean  <-  RM$DIA_END
    	RM$start1tpa  <-  RM$SURVIVORTPA+RM$MORTALITYTPA #initial number of trees is current survivors plus those that died during the resurvey period.
		RM$PREVTPAsum  <-  ave(RM$start1tpa, RM$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE)) 
		RM$end1tpa  <-  RM$SURVIVORTPA+RM$INGROWTHTPA #final number of trees is current survivors plus new trees that cross the 5 inch measurement threshold
		RM$TPAsum  <-  ave(RM$end1tpa, RM$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE))

	RM1  <-  subset(RM)    
		
RM2  <-  subset(RM1, cut==0 & start>0, select=c(PLT_CN, PREVTPAsum, TPAsum, PREVDIAmean, DIAmean))

# 		}
# }
# 
# print(Sys.time())

S3RM = RM2; rm(RM2,RM1)


##############DATA#############################DATA##############################DATA#################################
 #State_TREE.CSV; State_COND.CSV; State_PLOT.CSV files must be downloaded from the FIA website (http://apps.fs.fed.us/fiadb-downloads/datamart.html) to a local drive
# treefiles  <-  list.files("/Users/robertbooth/Documents/TDA210/USFSFIA", pattern="*TREE.CSV", full.names=TRUE)
# condfiles  <-  list.files("/Users/robertbooth/Documents/TDA210/USFSFIA", pattern="*COND.CSV", full.names=TRUE)
# plotfiles  <-  list.files("/Users/robertbooth/Documents/TDA210/USFSFIA", pattern="*PLOT.CSV", full.names=TRUE)
# 
# S3  <-  data.table(NULL)
# t  <- length(treefiles)
# 
# #load remeasured tree data 
# print(c("Starting:", treefiles[1]))
# 
# S3   <-   foreach(i=1:t, .combine=rbind, .inorder=FALSE) %dopar%
# {
# 	#print(c("Starting:", treefiles[i]))
# 	PLOT   <-   subset(as.data.table(read.csv(plotfiles[i],
#                  header = TRUE, sep = ",", quote="\"", dec=".")), LON>(-95), select=c(CN, LAT, LON,
#                  ELEV, ECOSUBCD, STATECD,PREV_PLT_CN, REMPER, DESIGNCD, KINDCD, MICROPLOT_LOC))
#             setnames(PLOT, "CN", "PLT_CN")
# 
# 	if(sum(PLOT$REMPER, na.rm=TRUE)>10) #proceed if state has been resurveyed
# 	{	
# 	TREE   <-   subset(as.data.table(read.csv(treefiles[i], header = TRUE, sep = ",", quote="\"", dec=".")), select=c(CN, PREV_TRE_CN, PLT_CN, INVYR,CONDID, DIA, TPA_UNADJ, HT, AGENTCD, DAMTYP1, DECAYCD, SPCD, STOCKING, STATUSCD, STANDING_DEAD_CD, SPGRPCD, CR, CCLCD, RECONCILECD, DIACHECK, DIAHTCD, PREVDIA, PREV_STATUS_CD, P2A_GRM_FLG, CARBON_AG, CARBON_BG, PLOT, TREE))
# 
# 	COND   <-   subset(as.data.table(read.csv(condfiles[i], header = TRUE, sep = ",", quote="\"", dec=".")), select=c(PLT_CN,CONDID, FORINDCD, STDAGE, STDSZCD, FLDSZCD, FLDTYPCD,SITECLCD, SLOPE, PHYSCLCD, GSSTKCD,ALSTK,ALSTKCD, GSSTK, DSTRBCD1,DSTRBYR1,DSTRBCD2,DSTRBYR2, TRTCD1, TRTYR1,TRTCD2,TRTYR2,TRTCD3,TRTYR3,PRESNFCD, DSTRBCD3, DSTRBYR3, BALIVE, CARBON_DOWN_DEAD,STDORGCD, SISP))
# 
# 	COND$CONmax  <-  ave(COND$CONDID, COND$PLT_CN,FUN=function(x) max(x, na.rm=TRUE)) 
# 	COND  <-  subset(COND, CONmax==1)  ## remove all plots with more than 1 condition 
# 	PC  <-  merge(COND,PLOT,all.x=TRUE, by=c("PLT_CN"))


    query = paste('SELECT 
              cn, lat, lon, elev, ecosubcd, statecd, prev_plt_cn, remper, designcd, 
              kindcd, microplot_loc
            FROM plot WHERE statecd<=56 AND lon>-95')

    tic() # ~10 sec
    PLOT = as.data.table(db.query(query, con=fia.con))
      setnames(PLOT, toupper(names(PLOT)))
      setnames(PLOT,"CN","PLT_CN")
      toc()

  # Remove states that haven't been resurveyed
    PLOT[, REMPERTOT := sumNA(REMPER), by=STATECD]
    PLOT = PLOT[ REMPERTOT>10, ]

  
      query = paste('SELECT 
              plt_cn,condid, forindcd, stdage, stdszcd, fldszcd, fldtypcd,siteclcd, slope, 
              physclcd, gsstkcd,alstk,alstkcd, gsstk, dstrbcd1,dstrbyr1,dstrbcd2,dstrbyr2, 
              trtcd1, trtyr1,trtcd2,trtyr2,trtcd3,trtyr3,presnfcd, dstrbcd3, dstrbyr3, 
              balive, carbon_down_dead,stdorgcd, sisp, statecd
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


  tic()
  PC = merge(COND, PLOT, all.x=TRUE, by="PLT_CN")
  toc()


  
      query = paste('SELECT 
              cn, prev_tre_cn, plt_cn, invyr,condid, dia, tpa_unadj, ht, agentcd, damtyp1, 
              decaycd, spcd, stocking, statuscd, standing_dead_cd, spgrpcd, cr, cclcd, 
              reconcilecd, diacheck, diahtcd, prevdia, prev_status_cd, p2a_grm_flg, 
              carbon_ag, carbon_bg, plot, tree, statecd
            FROM tree WHERE statecd IN (', paste(states,collapse=','), ')')
      tic() # ~ 10 min
      TREE = as.data.table(db.query(query, con=fia.con))
        setnames(TREE, toupper(names(TREE)))
      toc()

  db.close(fia.con)

	TREE$CONmax  <-  ave(TREE$CONDID, TREE$PLT_CN,FUN=function(x) max(x, na.rm=TRUE))
	TREE$RECONCILECD[is.na(TREE$RECONCILECD)]  <-  0
	TREE$PREVDIAna  <-  (TREE$PREVDIA)
	TREE$PREVDIAna[is.na(TREE$PREVDIAna)]  <-  0
	TREE$PREVSTATna  <-  TREE$PREV_STATUS_CD
	TREE$PREVSTATna[is.na(TREE$PREVSTATna)]  <-  0
	TREE$PLTnb  <-  ifelse(TREE$STATUSCD==1, 1,0)
	TREE$PLTn  <-  ave(TREE$PLTnb, TREE$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE))
	TREE$PrevPLTnb  <-  ifelse(TREE$PREV_STATUS_CD==1, 1,0)
	TREE$PrevPLTn  <-  ave(TREE$PrevPLTnb, TREE$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE))
	TREE$STATUSCDmax  <-  ifelse(TREE$STATUSCD==3, 3,0)
	TREE$STATUSCDmax  <-  ave(TREE$STATUSCDmax, TREE$PLT_CN, FUN=function(x) sum(x, na.rm=TRUE))
	
	TREE1a  <-  subset(TREE, CONmax==1 & INVYR<2014) #remove edge effects 

save.image("/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEdit_inprogress01.rdata")
  rm(TREE)

load("/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEdit_inprogress01.rdata")
library(data.table)
# RESTART HERE


# 	MCDPFT  <- as.data.table(read.csv("/Volumes/m-z/tda210/USFS/gcbPFT.csv", header = TRUE, sep = ",", quote="\"", dec="."))
  MCDPFT = as.data.table(read.csv(file.pft, header = TRUE))
	TREE1 <- merge(TREE1a, MCDPFT, all.x=T, by = "SPCD")
	  rm(TREE1a)
	
	TCPb  <-  merge(TREE1, PC, by=c("PLT_CN")) 
# 	TCPb  <-  subset(TCPb) #, DESIGNCD==1)
	  rm(TREE1)
	
	#connect PREV_CN for each tree prior to subset
	TREE3  <-  subset(TCPb, select=c(CN, DIA, HT, STOCKING, SPCD, TPA_UNADJ, AGENTCD, DSTRBCD1, STDAGE, MICROPLOT_LOC, SISP, CARBON_AG, CARBON_BG, PFT))
	PREVnames  <-  names(TREE3)
	PREVnamesnew  <-  paste("PREV_TRE_",PREVnames,sep="")
	setnames(TREE3, PREVnames,PREVnamesnew) 
# 	TREE3$PREV_TRE_CN  <-  as.numeric((TREE3$PREV_TRE_CN))
# 	TCPb$PREV_TRE_CN  <-  as.numeric((TCPb$PREV_TRE_CN))
  library(bit64)
  TREE3[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]
  TCPb[, PREV_TRE_CN := as.integer64(PREV_TRE_CN)]

# save.image("/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEdit_inprogress02.rdata")
# load("/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEdit_inprogress02.rdata")
# library(data.table)
  rm(list=setdiff(ls(), c('TREE3','TCPb')))


	TCP  <-  merge(TREE3,TCPb,  all.x=TRUE, all.y=TRUE, by="PREV_TRE_CN") #all.x=TRUE, all.y=TRUE
	save.image("/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEdit_inprogressTCP+TREE3.rdata")
	
	
	TCP  <-  subset(TCP, REMPER>3 & REMPER<9.5 & STATUSCDmax!=3 & STATUSCD!=0 & DESIGNCD==1) #remove plots that have not been resurveyed and those surveyed over unusually long or short periods or were harvested,  STATUSCD!=0:remove trees not resurveyed due to RECONCILECD = 5-9
	
	TCP$PREV_TRE_STDAGE  <-  ifelse(is.na(TCP$PREV_TRE_STDAGE)==TRUE, TCP$STDAGE-TCP$REMPER, TCP$PREV_TRE_STDAGE) #fill in missing with current age minus remeasurement period
	
	##DIAmean of DIA>5
	TCP$DIA5alive  <-  ifelse(TCP$DIA>=5 & TCP$STATUSCD==1 & TCP$P2A_GRM_FLG!="N", TCP$DIA, NA)
	TCP$DIA5meanalive  <-  ave(TCP$DIA5alive,TCP$PLT_CN,FUN=function(x) mean(x,na.rm=TRUE))
	TCP$PREVDIA5alive  <-  ifelse(TCP$PREVDIA>=5 & TCP$PREV_STATUS_CD==1 & TCP$P2A_GRM_FLG!="N", TCP$PREVDIA, NA)
	TCP$PREVDIA5meanalive  <-  ave(TCP$PREVDIA5alive,TCP$PLT_CN,FUN=function(x) mean(x,na.rm=TRUE))	

#Stocking of plots for trees with DIA>5
	TCP$STOCKING5  <-  ifelse(TCP$DIA5alive>0, TCP$STOCKING, NA)
	TCP$STOCKING5mid  <-  ave(TCP$STOCKING5,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE)) 	
	TCP$PREVSTOCKING5  <-  ifelse(TCP$PREVDIA5alive>0, TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVSTOCKING5mid  <-  ave(TCP$PREVSTOCKING5,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE)) 


	#new PFT's from ED2
		TCP$EHstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "EH", TCP$STOCKING, NA)
	TCP$EHstocksum  <-  ave(TCP$EHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVEHstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "EH", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVEHstocksum  <-  ave(TCP$PREVEHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$Evergreenstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "Evergreen", TCP$STOCKING, NA)
	TCP$Evergreenstocksum  <-  ave(TCP$Evergreenstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVEvergreenstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "Evergreen", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVEvergreenstocksum  <-  ave(TCP$PREVEvergreenstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$Hydricstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "Hydric", TCP$STOCKING, NA)
	TCP$Hydricstocksum  <-  ave(TCP$Hydricstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVHydricstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "Hydric", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVHydricstocksum  <-  ave(TCP$PREVHydricstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$LCstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "LC", TCP$STOCKING, NA)
	TCP$LCstocksum  <-  ave(TCP$LCstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVLCstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "LC", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVLCstocksum  <-  ave(TCP$PREVLCstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$LHstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "LH", TCP$STOCKING, NA)
	TCP$LHstocksum  <-  ave(TCP$LHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVLHstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "LH", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVLHstocksum  <-  ave(TCP$PREVLHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$MCstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "MC", TCP$STOCKING, NA)
	TCP$MCstocksum  <-  ave(TCP$MCstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVMCstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "MC", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVMCstocksum  <-  ave(TCP$PREVMCstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$NMHstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "NMH", TCP$STOCKING, NA)
	TCP$NMHstocksum  <-  ave(TCP$NMHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVNMHstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "NMH", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVNMHstocksum  <-  ave(TCP$PREVNMHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$NPstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "NP", TCP$STOCKING, NA)
	TCP$NPstocksum  <-  ave(TCP$NPstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVNPstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "NP", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVNPstocksum  <-  ave(TCP$PREVNPstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$SMHstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "SMH", TCP$STOCKING, NA)
	TCP$SMHstocksum  <-  ave(TCP$SMHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVSMHstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "SMH", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVSMHstocksum  <-  ave(TCP$PREVSMHstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$SPstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "SP", TCP$STOCKING, NA)
	TCP$SPstocksum  <-  ave(TCP$SPstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVSPstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "SP", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVSPstocksum  <-  ave(TCP$PREVSPstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
		TCP$CDstock  <-  ifelse(TCP$STATUSCD==1 & TCP$DIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PFT == "CD", TCP$STOCKING, NA)
	TCP$CDstocksum  <-  ave(TCP$CDstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))
	TCP$PREVCDstock  <-  ifelse(TCP$PREV_STATUS_CD==1 & TCP$PREVDIA>=5 & TCP$P2A_GRM_FLG!="N" & TCP$PREV_TRE_PFT == "CD", TCP$PREV_TRE_STOCKING, NA)
	TCP$PREVCDstocksum  <-  ave(TCP$PREVCDstock,TCP$PLT_CN,FUN=function(x) sum(x,na.rm=TRUE))

	TCP$LCstocksum <- TCP$LCstocksum+ TCP$CDstocksum	
	TCP$PREVLCstocksum <- TCP$PREVLCstocksum+ TCP$PREVCDstocksum
	
	S1  <-  NULL
S1  <-  subset(TCP, STDORGCD==0 & PREVSTOCKING5mid>0   &  PLT_CN!= 134680578010854 & !duplicated(PLT_CN) & PREV_PLT_CN>0, select=c(PLT_CN, PREVSTOCKING5mid, EHstocksum, PREVEHstocksum, Evergreenstocksum, PREVEvergreenstocksum, Hydricstocksum, PREVHydricstocksum,LCstocksum, PREVLCstocksum,LHstocksum, PREVLHstocksum, MCstocksum, PREVMCstocksum,NMHstocksum, PREVNMHstocksum,NPstocksum, PREVNPstocksum,SMHstocksum, PREVSMHstocksum,SPstocksum, PREVSPstocksum))

# 	}		
# 
# }
print("S1 done")

S3 = S1

S4  <-  merge(S3RM, S3, by=c("PLT_CN"))

F2<-subset(S4, PREVSTOCKING5mid>0)

F2$PREVDIA<- F2$PREVDIAmean
F2$DIA<- F2$DIAmean

#average means for binning
F2$PREVDIAmean  <-  ave(F2$PREVDIA, F2$PLT_CN, FUN=function(x) mean(x,na.rm=TRUE))

F2$PREVDIAbin<-ifelse(F2$PREVDIAmean<=14, ceiling(F2$PREVDIAmean), ceiling(F2$PREVDIAmean/2)*2)
F2$PREVDIAbin[F2$PREVDIAbin> 20]<-20  #collapse outliers

F2$PREVSTOCKbin<-ifelse(F2$PREVDIAmean<=14, ceiling(F2$PREVSTOCKING5mid/10)*10,ceiling(F2$PREVSTOCKING5mid/20)*20) 
F2$PREVSTOCKbin[F2$PREVSTOCKbin>110]<-110  #collapse outliers

#bins
F2$PREVDIASTOCKbin<- F2$PREVDIAbin*10000+F2$PREVSTOCKbin
# F2<-subset(F2, PREVDIASTOCKbin > 0, select=-c(PREVDIAbin, PREVSTOCKbin, DIAmean, PREVDIAmean ))
# write.csv(F2, file = "/Users/robertbooth/Documents/TDA210/USFSFIA/ESMraw.csv") #write to local drive
F3 = F2[ PREVDIASTOCKbin>0,
c("PLT_CN","PREVTPAsum","TPAsum","PREVSTOCKING5mid","EHstocksum","PREVEHstocksum","Evergreenstocksum","PREVEvergreenstocksum","Hydricstocksum","PREVHydricstocksum","LCstocksum","PREVLCstocksum","LHstocksum","PREVLHstocksum","MCstocksum","PREVMCstocksum","NMHstocksum","PREVNMHstocksum","NPstocksum","PREVNPstocksum","SMHstocksum","PREVSMHstocksum","SPstocksum","PREVSPstocksum","PREVDIA","DIA","PREVDIASTOCKbin"), with=F]


write.csv(F3, file = "/Work/Research/Macrosystems/FIA Phase Space/Data/ESMraw_minEditv01.csv") #write to local drive
# write.csv(F3, file = "~/rykelly/PhaseSpace/ESMraw_minEditv01.csv") #write to local drive


print("Done and Done")

