library(USGSHydroOpt)
library(tidyverse)
library(readxl)

source(file.path("process","src","run_suite.R"))

get_summaries <- function(multiplier = 1){
  
  glri <- readRDS(file.path("process","out",paste0("glri_fl_MRL_adjusted_",multiplier,"_dl.rds")))
  mmsd <- readRDS(file.path("process","out",paste0("mmsd_fl_MRL_adjusted_",multiplier,"_dl.rds")))
  #glpf <- readRDS(file.path("process","out",paste0("glpf_fl_MRL_adjusted_",multiplier,"_dl.rds")))
  
  glri_fl <- glri$df_adjusted
  mmsd_fl <- mmsd$df_adjusted
  #glpf_fl <- glpf$df2
  
  rm(glri,mmsd)
  #rm(glpf, glri, mmsd)
  
  glri_abs <- readRDS(file.path("process","out",paste0("glri_abs_MRL_adjusted_",multiplier,"_dl.rds")))
  mmsd_abs <- readRDS(file.path("process","out",paste0("mmsd_abs_MRL_adjusted_",multiplier,"_dl.rds")))
  #glpf_abs <- readRDS(file.path("process","out",paste0("glpf_abs_MRL_adjusted_",multiplier,"_dl.rds")))
  
  glri_abs <- glri_abs$df_adjusted
  mmsd_abs <- mmsd_abs$df_adjusted
 # glpf_abs <- glpf_abs$df2
  
  load(file.path("raw","MMSD","PhaseIII","MMSDabsEEMs.RData"))
  rm(MMSD3DEEMs)
  
  phaseIII <- names(dfabs)[-1][!(names(dfabs)[-1] %in% names(mmsd_abs)[-1])]
  phaseIII <- phaseIII[phaseIII != "X"]

  dfabs <- dfabs[,c("Wavelength", phaseIII)]
  dfFluor <- dfFluor[,c("Wavelength.Pairs",phaseIII)]
    
  phaseIV <- names(mmsd_abs)[-1][!(names(mmsd_abs)[-1] %in% names(dfabs)[-1])]
  
  mmsd_abs <- mmsd_abs[,c("Wavelength", phaseIV)]
  mmsd_fl <- mmsd_fl[,c("exem",phaseIV)]
  
  mmsd_abs_total <- mmsd_abs %>%
    left_join(dfabs, by="Wavelength", )
  
  mmsd_fl_total <- mmsd_fl %>%
    left_join(dfFluor, by=c("exem"="Wavelength.Pairs"))
  
  # GLRI Summary:
  df_glri <- readRDS(file.path("process","out","glri_summary_input.rds"))
  df_glri$NWISSiteID <- dataRetrieval::zeroPad(df_glri$NWISSiteID, 8)
  # GLPF Summary:
  #df_glpf <- readRDS(file.path("process","out","glpf_summary_input.rds"))
  # df_glpf_QA <- readRDS(file.path("raw","GLPF","summary_QA.rds"))
  
  # MMSD Summary:
  df_mmsd <- readRDS(file.path("process","out","mmsd_summary_input.rds"))
  
  dir.create(file.path("process","out"),showWarnings = FALSE)  
  
  glri_sum <- run_suite(df_glri, glri_fl, glri_abs)
  saveRDS(glri_sum, file = file.path("process","out",paste0("glri_summary_",multiplier,"_dl.rds")))
  
  #glpf_sum <- run_suite(df_glpf, glpf_fl, glpf_abs, glpf = TRUE)
  #saveRDS(glpf_sum, file = file.path("process","out",paste0("glpf_summary_",multiplier,"_dl.rds")))
  
  mmsd_sum <- run_suite(df_mmsd, mmsd_fl_total, mmsd_abs_total)
  saveRDS(mmsd_sum, file = file.path("process","out",paste0("mmsd_summary_",multiplier,"_dl.rds")))

}
  
