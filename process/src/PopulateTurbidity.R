
library(dplyr)
library(USGSHydroTools)
library(ggplot2)

PopulateTurbidity <- function() {
  
  staid <- c("04067500", "04085427", "04087170", "04165500", "04166500", "04176500", "04193500", "04195500")
  names(staid) <- c("EE","OC", "JI", "CL", "RO", "RM", "MA", "PO")
  
  staidTurb <- c("04067500", "04085427", "04087170", "04165500", "04166500", "04176500", "04193490", "04195500")
  names(staidTurb) <- c("EE","OC", "JI", "CL", "RO", "RM", "MA", "PO")
  
  #Load summary dataframe with bacteria and optical data
  load(file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
  df <- dfOptSumAll
  
  #Load turbidity data
  dfTurb <- readRDS(file.path("raw","GLRI","GLRI_Turbidity.rds"))
  
  #Determine adjustment to times in dataframe for converting to UTC
  TimeZoneAdjustment <- c(6,6,6,5,5,5,5,5)
  names(TimeZoneAdjustment) <- names(staid)
  df$TimeZoneAdjustment <- TimeZoneAdjustment[df$abbrev]*3600 #seconds to adjust time in dataframe by 
  
  #Convert dates to POSIXct and adjust to UTC time
  df$psdate <- as.POSIXct(df$Start.date.time..mm.dd.yy.hh.mm.,format = "%m/%d/%Y %H:%M", tz="UTC")
  df$psdate <- df$psdate + df$TimeZoneAdjustment
  
  df$pedate <- as.POSIXct(df$End.date.time..mm.dd.yy.hh.mm.,format = "%m/%d/%Y %H:%M", tz="UTC")
  df$pedate <- df$pedate + df$TimeZoneAdjustment
  
  #Reorder columns to have begin and end pdates next to each other 
  dateColumns <- which(names(df) %in% c("psdate","pedate"))
  dfNamesOrder <- names(df)[c(1:dateColumns[1],dateColumns[2],(dateColumns[1]+1):(dim(df)[2]-1))]
  df <- df[,dfNamesOrder]
  
  
  # Add turbidity to summary dataframe
  
  for (i in 1:length(staid)) {
    dfTurbTemp <- subset(dfTurb, site_no == staidTurb[i])
    dfTemp <- subset(df,abbrev == names(staid)[i])
    #  plot(dfTurbTemp$Turbidity~dfTurbTemp$dateTime,pch=20,cex=0.8)
    #  ggplot(dfTurbTemp,aes(x = dfTurbTemp$dateTime,y = dfTemp$Turbidity)) + geom_line()
    
    dfTemp2 <- TSstormstats(df = dfTurbTemp, date = "dateTime", varname = "Turbidity", dates = dfTemp,
                            starttime = "psdate", endtime = "pedate", stats.return = c("mean","max","min","median"),
                            out.varname = "Turbidity")
    if(i == 1) {dfWithTurb <- dfTemp2
    } else{ dfWithTurb <- rbind(dfWithTurb,dfTemp2)
    }
  }
  
  saveRDS(dfWithTurb,file=file.path("process","out","GLRISummaryWithTurbidity.rds"))
  
}