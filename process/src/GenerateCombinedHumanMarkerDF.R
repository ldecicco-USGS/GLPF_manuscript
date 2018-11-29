# GLPf manuscript, Combine the human marker data along with study identifier 
# and hydrologic condition for use in figure 2.

combineHumanMarkerFiles <- function(filename){
  #Load data
  # ## GLPF:
  glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
  #
  # ## GLRI:
  load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
  glri <- dfAll
  
  # Consider all values < 225 as censored to make censored values consistent across studies
  # Set all values <= 225 to 225
  
  glri[which(glri$BACHUM.cn.100mls <= 225),"BACHUM.cn.100mls"] <- 225 
  
  
  # load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
  #
  # ## MMSD:
  mmsd <- readRDS("raw/MMSD/dfOptP3P4Combined.rds")
  # 
  # 
  # Extract human markers and make one dataframe with all three scales
  
  # GLPF data
  glpfHM <- glpf[,c("CAGRnumber","State","pdate","hydroCondition","bacHum", "lachno")]
  glpfHM$scale <- "small"
  
  #GLRI data
  #names(glri)[1:20]
  glriHM <- glri[,c("GRnumber","Site","GMTStartTime","FinalHydrologicConditionDecision","BACHUM.cn.100mls", "Lachno.2.cn.100ml")]
  glriHM$scale <- "watershed"
  
  #MMSD data
  #Munging to combine Phase III and Phase IV are in git/MMSDOptical. 
  #Data file dfOptP3P4Combined.rds is the result of that munging and what we will
  #use as the "raw" data for the manuscript
  
  mmsdHM <- mmsd[,c("GRnumber","abbrev","psdate","hydro_condition","bacHum","lachno2")]
  mmsdHM$scale <- "subwatershed"
  
  # Consider all values < 225 as censored to make censored values consistent across studies
  # Set all values <= 225 to 225
  mmsd[which(mmsd$bacHum <= 225),"bacHum"] <- 225 
  
  names(glpfHM) <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","scale")
  names(glriHM) <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","scale")
  names(mmsdHM) <- c("GRnumber","site","psdate","hydro_condition","bacHum","lachno2","scale")
  
  dfHM <- rbind(glpfHM,glriHM)
  dfHM <- rbind(dfHM,mmsdHM)               
  dfHM <- filter(dfHM,!is.na(bacHum))
  
  saveRDS(dfHM,file=file.path("process","out",filename))
  
}