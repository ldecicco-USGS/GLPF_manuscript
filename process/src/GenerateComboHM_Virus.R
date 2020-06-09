# GLPf manuscript, Combine the human marker data along with study identifier 
# and hydrologic condition for use in figure 2.

comboHM_HV <- function(filename){
  #Load data
  # ## GLRI:
  # load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
  # glri <- dfAll
  load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
  glri <- dfOptSumAll
  # Consider all values < 225 as censored to make censored values consistent across studies
  # Set all values <= 225 to 225
  
  glri[which(glri$BACHUM.cn.100mls <= 225),"BACHUM.cn.100mls"] <- 225 
  glri[which(glri$Lachno.2.cn.100ml <= 225),"Lachno.2.cn.100ml"] <- 225 
  glri <- filter(glri,!is.na(FT.))
  
  # load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
  #
  # ## MMSD:
  mmsd <- readRDS("raw/MMSD/dfOptP3P4Combined.rds")
  names(mmsd)[which(names(mmsd) == "humanVirus")] <- "HumanVirus"
  
  # 
  # Find common optical variables
  glri_vars <- names(glri)
  mmsd_vars <- names(mmsd)

  glri_viruses <- names(glri)[16:23]
  glri_bacteria <- names(glri)[25:27]
  names(glri)[which(names(glri) == "Entero.CFUs.100ml")] <- "ent"
  names(glri)[which(names(glri) == "E..coli.CFUs.100ml")] <- "eColi"
  glri_FIB <- names(glri)[c(46,47,49)]
  
  mmsd_viruses <- names(mmsd)[5:7]
  mmsd_FIB <- names(mmsd)[c(9,10)]
  
  #glri_vars[which(glri_vars %in% mmsd_vars)]
  #glri_vars[which(!(glri_vars %in% mmsd_vars))]
  
  # Extract human markers and make one dataframe with all three scales
  
  #GLRI data
  #names(glri)[1:20]
  glriHV <- glri[,c("GRnumber","Site","GMTStartTime","FinalHydrologicConditionDecision",
                    "BACHUM.cn.100mls", "Lachno.2.cn.100ml","T","F","A","A254","HumanVirus",
                    glri_viruses,glri_bacteria,glri_FIB)]
  glriHV$scale <- "watershed"
  
  #MMSD data
  #Munging to combine Phase III and Phase IV are in git/MMSDOptical. 
  #Data file dfOptP3P4Combined.rds is the result of that munging and what we will
  #use as the "raw" data for the manuscript
  
  mmsdHV <- mmsd[,c("GRnumber","abbrev","psdate","hydro_condition","bacHum","lachno2",
                    "T","F","A","A254","HumanVirus",
                    mmsd_viruses,mmsd_FIB)]
  
  
  
  mmsdHV$scale <- "subwatershed"
  
  # Consider all values < 225 as censored to make censored values consistent across studies
  # Set all values <= 225 to 225
  mmsdHV[which(mmsdHV$bacHum <= 225),"bacHum"] <- 225 
  mmsdHV[which(mmsdHV$lachno2 <= 225),"lachno2"] <- 225 
  
  names(glriHV)[1:11] <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","T","F","A","A254","HumanVirus")
  names(mmsdHV)[1:11] <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","T","F","A","A254","HumanVirus")
  
  dfHV <- full_join(glriHV,mmsdHV)
  
  dfHV <- filter(dfHV,!is.na(bacHum))
  baseflow_Rows <- grep("base",dfHV$hydro_condition,ignore.case = TRUE)
  dfHV$hydro_condition[baseflow_Rows] <- "Low Flow"
  
  names(sitesSmall) <- c("Kinnickinnic","Red","Middle Clinton")
  sitesWatershed <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
  names(sitesWatershed) <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
  sitesSubWatershed <- c("HW","UW","MC","MW","MF","LD","CG","BK")
  names(sitesSubWatershed) <- c("Honey","Underwood","Menomonee DS","Menomonee Mid","Menomonee US","Little Menomonee","Milwaukee US","Bark")
  
  
  sites <- c(sitesSmall,sitesSubWatershed,sitesWatershed)
  table(dfHV$site)
  dfHV$site <- factor(dfHV$site,levels =sites)
  length(unique(dfHV$site))
  
  #Reduce down to sites with more than 20 observations
  sites.GT.20_obs <- names(which(table(dfHV$site)>20))
  
  keepRows <- dfHV$site %in% sites.GT.20_obs
  dfHV <- dfHV[keepRows,]
  dfHV$site <- droplevels( dfHV$site)
  saveRDS(dfHV,file=file.path("process","out",filename))
  
}

# These viruses are in GLRI, but not in mmsd.
# sum(glri$G1.Norovirus>0,na.rm=TRUE)  #3, sum = 54.83
# sum(glri$G2.Norovirus>0,na.rm=TRUE)  #7, sum = 1153.17
# sum(glri$Adenovirus.B>0,na.rm=TRUE)  #0
# sum(glri$HAV>0,na.rm=TRUE)           #0
# sum(glri$HumanRotavirus>0,na.rm=TRUE)#0
# 
# sum(glri$G1.Norovirus,na.rm=TRUE)
# sum(glri$G2.Norovirus,na.rm=TRUE)

