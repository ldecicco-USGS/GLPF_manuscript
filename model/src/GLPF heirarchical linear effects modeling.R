# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R

#Intro to Linear mixed models: https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/

library(lme4)
library(smwrBase)
library(car)


#set data directories
# raw.path <- "raw_data"
# cached.path <- "cached_data"
# summary.path <- "SummaryVariables"
# summary.save <- "1_SummaryVariables"
# cached.save <- "0_munge"

df_GLPF <- readRDS(file.path("process","out","glpf_summary.rds"))
df <- df_GLPF

df <- df[!is.na(df$lachno),]
df$lachno <- ifelse(df$lachno==1,112,df$lachno)

df <- df[!is.na(df$bacHum),]
df$bacHum <- ifelse(df$bacHum==1,112,df$bacHum)

df$logLachno <- log10(df$lachno)
df$logBachum <- log10(df$bacHum)
df$logEC <- log10(df$eColi)
df$logEnt <- log10(df$ent)
df$sinDate <- fourier(df$pdate)[,1]
df$cosDate <- fourier(df$pdate)[,2]

response <- "logEC"
response <- "logLachno"
#which(substr(names(df),1,1)=="A")
# AbsVars <- names(df)[c(61:138,232:240)]
# FlVars <- names(df)[c(17:60,139:231)]
# IVs <- c(AbsVars,FlVars)

# df$season <- as.factor(floor((as.POSIXlt(df$psdate)$mon+1)/3))
# df$season <- ifelse(as.POSIXlt(df$psdate)$mon == 11,1,df$season)

# test <- df[,c("season","psdate")]
# df$season <- ifelse(as.POSIXlt(df$psdate)$mon == 11,1,df$season)

sites <- c("WI","MI","NY")

# form <- paste(response," ~ ", 
# paste(IVs,collapse = " + "),"+"," (",paste(IVs[1:3], collapse = " + "),"|","season",")")
# Determine significance of coefficients: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

dfModel <- df
selectedRows <- which(df$State %in% sites)

m <- lmer(logEC ~ T*sinDate + T*cosDate + F*cosDate + F*sinDate + ( T + F  | eventNum),data=df[selectedRows,])

# Remove missing turbidity rows
missing_Turb <- which(is.na(df$Turb) | is.infinite(df$Turb))
dfModel <- df[c(-missing_Turb),]
dfModel <- dfModel[which(!is.na(dfModel[,response])),]
selectedRows <- which(dfModel$State %in% sites)

dfModel <- na.omit(df[,c(response,"T","F","Aresids","sinDate","cosDate","eventNum","State")])
m <- lmer(logLachno ~ T*cosDate + T*sinDate  + F*cosDate + F*sinDate + 
            Aresids*cosDate + Aresids*sinDate  +
            Turb + 
            (T +   F + Aresids + Turb | eventNum) + 
            (T +   F + Aresids + Turb | State),data=dfModel[selectedRows,])

m <- lmer(logLachno ~ T*cosDate + T*sinDate  + F*cosDate + F*sinDate + 
            Aresids*cosDate + Aresids*sinDate  +
            (T +   F + Aresids  | eventNum) + 
            (T +   F + Aresids  | State),data=dfModel[selectedRows,])

m <- lmer(logLachno ~ T*cosDate + T*sinDate  + F*cosDate + F*sinDate + (T +   F  | eventNum),data=dfModel[selectedRows,])


m <- lmer(logLachno ~ Aresids*cosDate + Aresids*sinDate  + F*cosDate + F*sinDate + Turb + (Aresids +   F  + Turb | eventNum),data=dfModel[selectedRows,])

m <- lmer(logLachno ~  F*cosDate + F*sinDate + Turb + (F  + Turb | State),data=dfModel[selectedRows,])

m <- lmer(logLachno ~  Turb + (Turb | State),data=dfModel[selectedRows,])

m <- lmer(logLachno ~ T*sinDate + T*cosDate + F*cosDate + F*sinDate + Turb + ( T + F + Turb | State),data=dfModel[selectedRows,])

m <- lm(logLachno ~ Aresids*cosDate + Aresids*sinDate  + F*cosDate + F*sinDate + Turb ,data=dfModel[selectedRows,])

# m <- lmer(logLachno ~ T + F + Turb + CSO + ( T + F + Turb | State) + ( T + F + Turb | season),data=dfModel[selectedRows,])


#plot(fitted(m) ~ df[selectedRows,"logLachno"])

#Plot Observed vs Predicted from cv-determined model

colorOptions <- c("red","green","blue")#,"black","springgreen4","blue","grey","darkorchid1")
names(colorOptions) <- unique(dfModel$State)

pdf("regression_by_eventNum_wo_turb.pdf")
for(event in sort(unique(dfModel$eventNum))) {
  plotColors <- colorOptions[dfModel[selectedRows,"State"]]
  levels(as.factor(dfModel[selectedRows,"State"]))
  plotColors <- ifelse(dfModel$eventNum==event,"purple","grey")
  plotCex <- ifelse(dfModel$eventNum==event,2,1)
  
  axis.log <- ""
  axis.limits <- c(1,7)
  
  par(mfcol=c(1,1))
  plot(dfModel[,response],fitted(m),
       xlab="Observed",ylab="Predicted",col=plotColors,pch=20,
       log=axis.log,ylim=axis.limits,xlim=axis.limits,cex=plotCex,
       main = event)
  abline(0,1)
  #mtext(paste(Active.Coef.names[2:length(Active.Coef.names)],collapse=", "),side=3,line=1,cex=0.8)
  mtext(paste("Linear mixed effects moedel for GLRI watersheds",response),side=3,line=2,font=2,cex=1)
  legend(x="topleft",legend=names(colorOptions),col=colorOptions,pch=20,text.col=colorOptions,cex=0.7)
}
dev.off()
shell.exec("regression_by_eventNum.pdf")


summary(m)
Anova(m)

plotdf <- dfModel[selectedRows,]
plotCex <- ifelse(plotdf$State %in% (c("CL","RO")),1.5,1)
plotCol <- ifelse(plotdf$State %in% (c("CL","RO")),"orange","black")

points(plotdf[,response],fitted(m),
       xlab="Observed",ylab="Predicted",col=plotCol,pch=20,
       ylim=axis.limits,xlim=axis.limits,cex=plotCex)



mstep <- step(m)
summary(m)
summary(mstep)


