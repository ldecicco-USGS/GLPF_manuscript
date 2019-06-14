# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R

#Intro to Linear mixed models: https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/

# Load data
# General modeling setup:
# Define response variables
# Define predictors
# Define interactions
# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
# Choose sites or states to be included

# Transform variables where needed
# Filter data to sites and make model df
# Construct formula
# Run LME model for all response variables
# Graph results


library(lme4)
library(smwrBase)
library(car)

# Load data
df_MMSD <- readRDS(file.path("process","out","mmsd_summary.rds"))
df <- df_MMSD

# General modeling setup:

# Define response variables
response <- c("lachno2","bacHum","eColi","ent")


i <- 1

# Define predictors and interaction terms
predictors<- c("Aresid267", "T", "F")
interactors <- c("sinDate","cosDate")
#predictors_withou_int <- "Turb"


# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")

# Choose sites or states to be included
sites <- c("MC","MW","UW")

# transform response variable
df$log_response <- log10(df[,response[i]])

# Filter data to sites and make model df
model_df <- df[which(df[,groupings] %in% sites),]

# Construct formula
form <- paste("")
for(j in 1:length(interactors)){ 
  form_temp <- paste(paste(predictors,"*",interactors[j],sep=""),collapse = " + ")
  if(j>1) {form <- paste(form_temp, " + ", form) 
  }else form <- form_temp
}



form <- paste("log_respnse ~ ")


paste(form, paste(predictors,"*",interactors),collapse = " + ")



paste(predictors,interactors[j],sep="*")

# Run LME model for all response variables
# Graph results




####################################################################################

df <- df[!is.na(df$lachno2),]
#df$lachno2 <- ifelse(df$lachno2<225,225,df$lachno2)

df <- df[!is.na(df$bacHum),]
#df$bacHum <- ifelse(df$bacHum<225,225,df$bacHum)

df$logLachno <- log10(df$lachno2)
df$logBachum <- log10(df$bacHum)
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

response <- "logLachno"
#which(substr(names(df),1,1)=="A")
# AbsVars <- names(df)[c(61:138,232:240)]
# FlVars <- names(df)[c(17:60,139:231)]
# IVs <- c(AbsVars,FlVars)

# df$season <- as.factor(floor((as.POSIXlt(df$psdate)$mon+1)/3))
# df$season <- ifelse(as.POSIXlt(df$psdate)$mon == 11,1,df$season)

# test <- df[,c("season","psdate")]
# df$season <- ifelse(as.POSIXlt(df$psdate)$mon == 11,1,df$season)

sites <- c("MC","MW","UW")




# form <- paste(response," ~ ", 
# paste(IVs,collapse = " + "),"+"," (",paste(IVs[1:3], collapse = " + "),"|","season",")")
# Determine significance of coefficients: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

dfModel <- df
selectedRows <- which(df$abbrev %in% sites)

#m <- lmer(logLachno ~ T*sinDate + T*cosDate + F*cosDate + F*sinDate + ( T + F  || abbrev),data=df[selectedRows,])

# Remove missing turbidity rows
# missing_Turb <- which(is.na(df$Turbidity_mean) | is.infinite(df$Turbidity_mean))
# dfModel <- df[c(-missing_Turb),]
selectedRows <- which(dfModel$abbrev %in% sites)

m <- lmer(logLachno ~ Aresid267*cosDate + Aresid267*sinDate  + F*cosDate + F*sinDate + CSO + (Aresid267 +   F  || abbrev),data=dfModel[selectedRows,])

# m <- lmer(logLachno ~  F*cosDate + F*sinDate + Turbidity_mean + (F  + Turbidity_mean | abbrev),data=dfModel[selectedRows,])
# 
# m <- lmer(logLachno ~  Turbidity_mean + (Turbidity_mean | abbrev),data=dfModel[selectedRows,])
# 
# m <- lmer(logLachno ~ T*sinDate + T*cosDate + F*cosDate + F*sinDate + Turbidity_mean + ( T + F + Turbidity_mean | abbrev),data=dfModel[selectedRows,])
# 
# m <- lm(logLachno ~ Aresids*cosDate + Aresids*sinDate  + F*cosDate + F*sinDate + Turbidity_mean ,data=dfModel[selectedRows,])

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
mtext(paste("Linear mixed effects moedel for GLRI watersheds",response),side=3,line=2,font=2,cex=1)
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
