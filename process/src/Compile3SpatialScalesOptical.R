## NEED TO LOOK INTO SAMPLE IN ROW 276 WWKKS57-B2. LOOKS LIKE A BLANK


library(USGSHydroOpt)
library(dplyr)
library(data.table)


setwd("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/Merged/")
#load('GLPFGLRIVectorized02242016.Rdata')
fileSuffix <- 'GLPFWWAug312016'
fileSuffixVectorized <- 'GLPFWWAug312016'
SummaryDir <- "//igsarmewfsapa/projects/QW Monitoring Team/Optical sensors/AqualogProcessing/SummaryVariables/GLPF/"

##### Load summary + bacteria data ##########
# dfall <- readRDS("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/R/cacheMerge/cached_data/merged/mergedData.rds")
# dffl <- readRDS("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/R/cacheMerge/cached_data/optics/dffl.rds")
# dfabs <- readRDS("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/R/cacheMerge/cached_data/optics/dfabs.rds")
dfall <- setDF(fread("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/R/cacheMerge/cached_data/final/mergedData.csv"))
dffl <- setDF(fread("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/R/cacheMerge/cached_data/final/dffl.csv"))
dfabs <- setDF(fread("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/R/cacheMerge/cached_data/final/dfabs.csv"))

#load("//igsarmewfsapa/projects/QW Monitoring Team/GLPF/Data/merged/glriglpf2016-03-16.Rdata")

dfall.orig <- dfall

#dfall <- dfall[which(dfall[,'SampleType9regular2blank7replicate']==9),]
dfall <- dfall[dfall$CAGRnumber!='',]
dfall <- dfall[!is.na(dfall$CAGRnumber),]

#startOpt <- which(names(dfall)=='OB1')
#endOpt <- which(names(dfall)=='logA555')
#dfall <- dfall[,-c(startOpt:endOpt)]

names(dfall)

##########################################################
# 
names(dffl)[1] <- 'ExEmWavs'
names(dfabs)[1] <- 'Wavelength'
names(dffl)[-1] <- substr(names(dffl)[-1],1,7)
optResultsrows <- dfall$CAGRnumber[which(dfall$CAGRnumber %in% names(dffl))]
dfall2 <- dfall[which(dfall$CAGRnumber %in% names(dffl)),]

dfFl <- dffl[,c("ExEmWavs",optResultsrows)]
dfAbs <- dfabs[,c("Wavelength",optResultsrows)]
EEMs3D <- VectorizedTo3DArray(dfFl,"ExEmWavs",grnum = "CAGRnumber")


#save(dfFl,dfAbs,dfall,EEMs3D,file=paste(fileSuffix,"vectorized.RData",sep=""))

#load(file=paste(fileSuffixVectorized,"vectorized.RData",sep=""))

names(EEMs3D[,,1])

dim(EEMs3D)

##############################################################################################
# Read summary signals to extract from Fl and abs info
dfFlSignals <- read.csv(paste(SummaryDir,"ex_ems_meansCA.csv",sep=""),as.is=TRUE)
dfAbsSignals <- read.csv(paste(SummaryDir,"abs_wavsCA.csv",sep=""),as.is=TRUE)
AbsSignals <- as.numeric(dfAbsSignals[,1])
dfSagSignals <- read.csv(paste(SummaryDir,"SagWavesCA.csv",sep=""),as.is=TRUE)
ratioSignalsAbs <- read.csv(paste(SummaryDir,"ratioSignalsAbsCA.csv",sep=""),as.is=TRUE)
ratioSignalsAbs <- ratioSignalsAbs[which(ratioSignalsAbs[2]>0),1]
ratioSignalsSr <- read.csv(paste(SummaryDir,"ratioSignalsSrCA.csv",sep=""),as.is=TRUE)
ratioSignalsSr <- ratioSignalsSr[which(ratioSignalsSr[2]>0),1]
ratioSignalsSniff <- read.csv(paste(SummaryDir,"ratioSignalsSniff.csv",sep=""),as.is=TRUE)
ratioSignalsSniff <- ratioSignalsSniff[which(ratioSignalsSniff[2]>0),1]
logSignals <- read.csv(paste(SummaryDir,"logSignalsCA.csv",sep=""),as.is=TRUE)[,1]
ratioSignalsSniffWetStar <- read.csv(paste(SummaryDir,"ratioSignalsSniffAll.csv",sep=""),as.is=TRUE)[,1]
fractionSignalsAbs <- read.csv(paste(SummaryDir,"fractionSignals.csv",sep=""),as.is=TRUE)


##############################################################################################
######### Add summary variables to summary data frame #######################################

#Fluorescence pairs and means
dfOpt <- getMeanFl(a=EEMs3D,signals=dfFlSignals,Peak="Peak",Ex1="Ex1",Ex2="Ex2",Em1="Em1",Em2="Em2",dataSummary=dfall2,grnum="CAGRnumber")
dfOpt[,dfFlSignals$Peak] <- as.data.frame(lapply(dfOpt[,dfFlSignals$Peak], function(x){replace(x, x <0,0)}))

#HIX, FI, Freshness 
dfOpt <- getIndexes(a=EEMs3D,dataSummary=dfOpt,grnum="CAGRnumber")
                    
#Single absorbance signals
dfOpt <- getAbs(dataAbs=dfAbs,waveCol=,"Wavelength",wavs=dfAbsSignals[,1],colSubsetString="gr",dataSummary=dfOpt,grnum="CAGRnumber")

#Spectral slopes
dfOpt <- getSag(dataAbs=dfAbs,waveCol="Wavelength",sag=dfSagSignals,colSubsetString="gr",dataSummary=dfOpt,grnum="CAGRnumber")

#deviance of abs from exponential regression in the wastewater area
dfOpt <- getExpResid(wavelength=269,rangeReg=c(240,341),rangeGap=c(254,302),dataAbs=dfAbs,
                      waveCol="Wavelength",colSubsetString="gr",dataSummary=dfOpt,grnum="CAGRnumber")

#Ratios of a few things
dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsAbs,grnum="CAGRnumber")
dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSr,grnum="CAGRnumber")
dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSniff,grnum="CAGRnumber")
dfOpt <- getRatios(dataSummary=dfOpt,sigs=ratioSignalsSniffWetStar,grnum="CAGRnumber")

#Compute fraction of summation indices
dfOpt <- getFrcSum(dataSummary = dfOpt,sigs = fractionSignals,grnum = "CAGRnumber")

#write.csv(names(dfOpt),file="dfOptnames.csv")
#log transform where it makes sense
dfOpt <- getLog10(dataSummary=dfOpt,signals=logSignals,grnum="CAGRnumber")

dfOpt$USGSFieldID <- dfOpt$fieldID
dfall2$USGSFieldID <- dfOpt$fieldID

dfOpt <- arrange(dfOpt, USGSFieldID)
#dfall <- arrange(dfall, USGSFieldID)

NaCount <- numeric()
for (i in 1:dim(dfOpt)[2]){
NaCount <- c(NaCount,sum(is.na(dfOpt[,i])))
}
names(NaCount) <- names(dfOpt)
NaCount[which(NaCount>0)]

dfOpt$CAGRnumber[which(is.na(dfOpt$T))]

dfOpt$USGSFieldID

filenm <-  paste(fileSuffix,"summary.RData",sep="")
save(dfOpt,file=filenm)
filenm <-  paste(fileSuffix,"summary.csv",sep="")
write.csv(dfOpt,file=filenm,row.names=FALSE)
  
##############################################################################################
##############################################################################################
# Plot EEMs
a <- EEMs3D
fieldIDs <- dfall[,"FieldID"]
labIDs <- dfall[,"CAGRnumber"]
Ex <- as.numeric(names(a[,1,1]))
Em <- as.numeric(names(a[1,,1]))
nlevels <- 50
Peaks <- ex_ems
peakCol <- "Peak"
peakEx <- "ExCA"
peakEm <- "EmCA"

filenm <- paste(fileSuffix,"EEMs.pdf",sep="")
pdf(filenm)
for (i in 1:dim(dfall)[1]){
#mat <- a[which(Ex>238 & Ex<500),which(Em>238 & Em<750),labIDs[i]]
mat <- a[,,labIDs[i]]
# Ex <- as.numeric(colnames(mat))
# Em <- as.numeric(row.names(mat))

# Ex <- Ex[which(Ex>230 & Ex<500)]
# Em <- Em[which(Em>238 & Em<750)]
mainTitle <- paste("GLPF GLRI ",dfall[i,"FieldID"],dfall[i,"ActivityStartDate"])

EEMsPlot <- plotEEMs(mat=mat,Ex=Ex,Em=Em,nlevels=nlevels,Peaks=Peaks,peakCol=peakCol,
                         peakEx=peakEx,peakEm=peakEm,mainTitle=mainTitle,titleSize = 1.0,
                      xlim = c(239,500), ylim = c(239,670))
}
dev.off()
shell.exec(filenm)

##############################################################################################
##############################################################################################
# Plot Abs

fieldIDs <- dfall[,"FieldID"]
labIDs <- dfall[,"CAGRnumber"]

filenm <- paste(fileSuffix,"Abs.pdf",sep="")
pdf(filenm)
for (i in 1:dim(dfall)[1]){
  mat <- a[,,labIDs[i]]
  mainTitle <- paste("GLPF GLRI",dfall[i,"FieldID"],dfall[i,"ActivityStartDate"])
  
  plotAbs(dataAbs = dfAbs,WaveCol = "Wavelength",absCol = labIDs[i],main=mainTitle)
 
}
dev.off()
shell.exec(filenm)

