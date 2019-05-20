run_suite <- function(df_summary, df_fl, df_abs, glpf=FALSE){
  
  EEMs <- VectorizedTo3DArray_new(df_fl, "exem", "GRnumber")
  
  SummaryDir <- file.path("raw","Optical summary definitions")
  
  if(glpf){
    dfFlSignals <- data.table::fread(file.path(SummaryDir,"ex_ems_meansCA.csv"), data.table = FALSE)
    dfAbsSignals <- data.table::fread(file.path(SummaryDir,"abs_wavsCA.csv"), data.table = FALSE)
    ratioSignalsAbs <- read.csv(file.path(SummaryDir,"ratioSignalsAbsCA.csv"),as.is=TRUE)
    ratioSignalsAbs <- ratioSignalsAbs[which(ratioSignalsAbs[2]>0),1]
    ratioSignalsSr <- data.table::fread(file.path(SummaryDir,"ratioSignalsSrCA.csv"), data.table = FALSE)
    ratioSignalsSr <- ratioSignalsSr[which(ratioSignalsSr[2]>0),1,drop=FALSE]
    logSignals <- data.table::fread(file.path(SummaryDir,"logSignalsCA.csv"),data.table = FALSE)[,1]
  } else {
    dfFlSignals <- data.table::fread(file.path(SummaryDir,"ex_ems_means.csv"), data.table = FALSE)
    dfAbsSignals <- data.table::fread(file.path(SummaryDir,"abs_wavs.csv"), data.table = FALSE)
    ratioSignalsAbs <- read.csv(file.path(SummaryDir,"ratioSignalsAbs.csv"),as.is=TRUE)
    ratioSignalsAbs <- ratioSignalsAbs[which(ratioSignalsAbs[2]>0),1]
    ratioSignalsSr <- data.table::fread(file.path(SummaryDir,"ratioSignalsSr.csv"), data.table = FALSE)
    ratioSignalsSr <- ratioSignalsSr[which(ratioSignalsSr[2]>0),1,drop=FALSE]
    logSignals <- data.table::fread(file.path(SummaryDir,"logSignals.csv"),data.table = FALSE)[,1]
  }
 
  dfSagSignals <- data.table::fread(file.path(SummaryDir,"SagWaves.csv"), data.table = FALSE)

  ratioSignalsSniff <- data.table::fread(file.path(SummaryDir,"ratioSignalsSniff.csv"),data.table = FALSE)
  ratioSignalsSniff <- ratioSignalsSniff[which(ratioSignalsSniff[2]>0),1,drop=FALSE]
  
  ratioOrder <- readRDS(file.path(SummaryDir,"RatioOrder.rds"))
  
  x <- getMeanFl(a=EEMs,
                 signals=dfFlSignals,
                 Peak="Peak",Ex1="Ex1",Ex2="Ex2",
                 Em1="Em1",Em2="Em2",
                 dataSummary = df_summary,
                 grnum="GRnumber")
  
  #HIX, FI, Freshness 
  x <- getIndexes(a=EEMs,
                  dataSummary=x,
                  grnum="GRnumber")
  
  #Single absorbance signals
  x <- getAbs(dataAbs=df_abs,
              waveCol="Wavelength",
              wavs=dfAbsSignals[,1],
              colSubsetString="gr",
              dataSummary=x,
              grnum="GRnumber")
  
  #Spectral slopes
  x <- getSag(dataAbs=df_abs,
              waveCol="Wavelength",
              sag=dfSagSignals,
              colSubsetString="gr",
              dataSummary=x,
              grnum="GRnumber")
  
  #deviance of abs from exponential regression in the wastewater area
  x <- getExpResid(wavelength=267,
                   rangeReg=c(240,340),
                   rangeGap=c(255,300),
                   dataAbs=df_abs,
                   waveCol="Wavelength",
                   colSubsetString="gr",
                   dataSummary=x,
                   grnum="GRnumber")
  
  #Ratios of a few things
  x <- getRatios(dataSummary=x,
                 grnum="GRnumber",
                 specifyOrder = TRUE,
                 ratioVars = ratioOrder)
  
  #log transform where it makes sense
  logSignals <- logSignals[logSignals %in% names(x)]
  x <- getLog10(dataSummary=x,
                signals=logSignals,
                grnum="GRnumber")
  
  return(x)
}

VectorizedTo3DArray_new <- function(df, ExEm, grnum){
  
  df_new <- df %>%
    separate(!!ExEm, into = c("Ex","Em"))
  
  m <- reshape2::melt(data = df_new, id = c("Ex", "Em"))
  colnames(m)[3] <- grnum
  a <- reshape2::acast(m, m[, 1] ~ m[, 2] ~ m[, 3])
  return(a)
}
