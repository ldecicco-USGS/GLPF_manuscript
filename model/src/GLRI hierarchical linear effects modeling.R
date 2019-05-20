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

df_GLRI <- readRDS(file.path("process","out","GLRISummaryWithTurbidity.rds"))
df <- df_GLRI

df <- df[!is.na(df$Lachno.2.cn.100ml),]
df$Lachno.2.cn.100ml <- ifelse(df$Lachno.2.cn.100ml==1,112,df$Lachno.2.cn.100ml)

df <- df[!is.na(df$BACHUM.cn.100mls),]
df$BACHUM.cn.100mls <- ifelse(df$BACHUM.cn.100mls==1,112,df$BACHUM.cn.100mls)

df$logLachno <- log10(df$Lachno.2.cn.100ml)
df$logBachum <- log10(df$BACHUM.cn.100mls)
df$sinDate <- fourier(df$GMTStartTime)[,1]
df$cosDate <- fourier(df$GMTStartTime)[,2]

response <- "logLachno"
#which(substr(names(df),1,1)=="A")
# AbsVars <- names(df)[c(61:138,232:240)]
# FlVars <- names(df)[c(17:60,139:231)]
# IVs <- c(AbsVars,FlVars)

# df$season <- as.factor(floor((as.POSIXlt(df$psdate)$mon+1)/3))
# df$season <- ifelse(as.POSIXlt(df$psdate)$mon == 11,1,df$season)

# test <- df[,c("season","psdate")]
# df$season <- ifelse(as.POSIXlt(df$psdate)$mon == 11,1,df$season)

sites <- c("JI", "EE", "OC", "PO", "MA", "CL", "RO", "RM")
sites <- c("JI")
sites <- c("EE", "OC")
sites <- c("PO", "MA", "CL", "RO", "RM","JI")
sites <- c("CL", "RO")
sites <- c("CL", "RO","JI")
sites <- c("PO", "MA", "RM")



# form <- paste(response," ~ ", 
# paste(IVs,collapse = " + "),"+"," (",paste(IVs[1:3], collapse = " + "),"|","season",")")
# Determine significance of coefficients: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

dfModel <- df
selectedRows <- which(df$abbrev %in% sites)

m <- lmer(logLachno ~ T*sinDate + T*cosDate + F*cosDate + F*sinDate + ( T + F  | abbrev),data=df[selectedRows,])

# Remove missing turbidity rows
missing_Turb <- which(is.na(df$Turbidity_mean) | is.infinite(df$Turbidity_mean))
dfModel <- df[c(-missing_Turb),]
selectedRows <- which(dfModel$abbrev %in% sites)

m <- lmer(logLachno ~ Aresids*cosDate + Aresids*sinDate  + F*cosDate + F*sinDate + Turbidity_mean + (Aresids +   F  + Turbidity_mean | abbrev),data=dfModel[selectedRows,])

m <- lmer(logLachno ~  F*cosDate + F*sinDate + Turbidity_mean + (F  + Turbidity_mean | abbrev),data=dfModel[selectedRows,])

m <- lmer(logLachno ~  Turbidity_mean + (Turbidity_mean | abbrev),data=dfModel[selectedRows,])

m <- lmer(logLachno ~ T*sinDate + T*cosDate + F*cosDate + F*sinDate + Turbidity_mean + ( T + F + Turbidity_mean | abbrev),data=dfModel[selectedRows,])

m <- lm(logLachno ~ Aresids*cosDate + Aresids*sinDate  + F*cosDate + F*sinDate + Turbidity_mean ,data=dfModel[selectedRows,])

# m <- lmer(logLachno ~ T + F + Turbidity_mean + CSO + ( T + F + Turbidity_mean | abbrev) + ( T + F + Turbidity_mean | season),data=dfModel[selectedRows,])


#plot(fitted(m) ~ df[selectedRows,"logLachno"])

#Plot Observed vs Predicted from cv-determined model

colorOptions <- c("orange","yellow2","skyblue","black","springgreen4","blue","grey","darkorchid1")
names(colorOptions) <- unique(dfModel$abbrev)

plotColors <- colorOptions[dfModel[selectedRows,"abbrev"]]
levels(as.factor(dfModel[selectedRows,"abbrev"]))

axis.log <- ""
axis.limits <- c(1,7)

par(mfcol=c(1,1))
plot(dfModel[selectedRows,response],fitted(m),
     xlab="Observed",ylab="Predicted",col=plotColors,pch=20,log=axis.log,ylim=axis.limits,xlim=axis.limits)
abline(0,1)
#mtext(paste(Active.Coef.names[2:length(Active.Coef.names)],collapse=", "),side=3,line=1,cex=0.8)
mtext(paste("Linear mixed effects moedel for MMSD subwatersheds",response),side=3,line=2,font=2,cex=1)
legend(x="topleft",legend=names(colorOptions),col=colorOptions,pch=20,text.col=colorOptions,cex=0.7)

summary(m)
Anova(m)

plotdf <- dfModel[selectedRows,]
plotCex <- ifelse(plotdf$abbrev %in% (c("CL","RO")),1.5,1)
plotCol <- ifelse(plotdf$abbrev %in% (c("CL","RO")),"orange","black")

points(plotdf[,response],fitted(m),
     xlab="Observed",ylab="Predicted",col=plotCol,pch=20,
     ylim=axis.limits,xlim=axis.limits,cex=plotCex)



mstep <- step(m)
summary(m)
summary(mstep)
