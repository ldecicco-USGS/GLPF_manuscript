# GLPf manuscript, Combine the human marker data along with study identifier 
# and hydrologic condition for use in figure 2.

comboHMPlusBasicOptical <- function(filename){
  #Load data
  # ## GLPF:
  glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
  #
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
  # 
  # 
  # Extract human markers and make one dataframe with all three scales
  
  # GLPF data
  glpfHM <- glpf[,c("CAGRnumber","State","pdate","hydroCondition","bacHum", "lachno","T","F","A","A254")]
  glpfHM$scale <- "small"
  
  #GLRI data
  #names(glri)[1:20]
  glriHM <- glri[,c("GRnumber","Site","GMTStartTime","FinalHydrologicConditionDecision","BACHUM.cn.100mls", "Lachno.2.cn.100ml","T","F","A","A254")]
  glriHM$scale <- "watershed"
  
  #MMSD data
  #Munging to combine Phase III and Phase IV are in git/MMSDOptical. 
  #Data file dfOptP3P4Combined.rds is the result of that munging and what we will
  #use as the "raw" data for the manuscript
  
  mmsdHM <- mmsd[,c("GRnumber","abbrev","psdate","hydro_condition","bacHum","lachno2","T","F","A","A254")]
  mmsdHM$scale <- "subwatershed"
  
  # Consider all values < 225 as censored to make censored values consistent across studies
  # Set all values <= 225 to 225
  mmsdHM[which(mmsdHM$bacHum <= 225),"bacHum"] <- 225 
  mmsdHM[which(mmsdHM$lachno2 <= 225),"lachno2"] <- 225 
  
  names(glpfHM) <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","T","F","A","A254","scale")
  names(glriHM) <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","T","F","A","A254","scale")
  names(mmsdHM) <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","T","F","A","A254","scale")
  
  dfHM <- rbind(glpfHM,glriHM)
  dfHM <- rbind(dfHM,mmsdHM)               
  dfHM <- filter(dfHM,!is.na(bacHum))
  baseflow_Rows <- grep("base",dfHM$hydro_condition,ignore.case = TRUE)
  dfHM$hydro_condition[baseflow_Rows] <- "Low Flow"
  
  sitesSmall <- c("WI","NY","MI")
  names(sitesSmall) <- c("Kinnickinnic","Red","Middle Clinton")
  sitesWatershed <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
  names(sitesWatershed) <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
  sitesSubWatershed <- c("HW","UW","MC","MW","MF","LD","CG","BK")
  names(sitesSubWatershed) <- c("Honey","Underwood","Menomonee DS","Menomonee Mid","Menomonee US","Little Menomonee","Milwaukee US","Bark")
  
  
  sites <- c(sitesSmall,sitesSubWatershed,sitesWatershed)
  table(dfHM$site)
  dfHM$site <- factor(dfHM$site,levels =sites)
  length(unique(dfHM$site))
  
  #Reduce down to sites with more than 20 observations
  sites.GT.20_obs <- names(which(table(dfHM$site)>20))
  
  keepRows <- dfHM$site %in% sites.GT.20_obs
  dfHM <- dfHM[keepRows,]
  dfHM$site <- droplevels( dfHM$site)
  saveRDS(dfHM,file=file.path("process","out",filename))
  
}