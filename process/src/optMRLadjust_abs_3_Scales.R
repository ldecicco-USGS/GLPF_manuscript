source(file.path("process","src","optMRL.R"))
source(file.path("process","src","optMRLAdjust.R"))

dfabsSum <- readRDS(file.path("raw","GLPF","summary_QA.rds"))
dfabs <- readRDS(file.path("raw","GLPF","Optics","dfabs_QA.rds"))

###!!!!! This file has WWTP influent data in it!!!!!!!!!!! Need to find the noQAnoWWTP version
dfSampleSum <- readRDS(file.path("raw","GLPF","summary_noQA.rds"))

blankGRnums <- grep("blank",dfabsSum$Comments,ignore.case = TRUE)
sampleGRnums <- 

dfMRLabs <- optMRL(df=dfabs,Wavelength = "nm", blankGRnums = blankGRnums)
summary(dfMRLabs$MRL)

dfabs_adj <- optMRLAdjust(df=dfabs,dfMRLs = dfMRLabs,Wavelength = "nm",sampleGRnums = )