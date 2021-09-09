

apply_MRLs <- function() {
  library(dplyr)
  #Load data
  
  load(file = file.path("process","out","glri_optical_Dec_2018_no_MRLs.RData"))
  load(file = file.path("process","out","mmsd_optical_Dec_2018_no_MRLs.RData"))
  
  #load(file = file.path("process","out","glpf_optical_Dec_2018_no_MRLs.RData"))
  glpf_abs <- readRDS(file.path("raw","GLPF","optics","dfabs_noQA.rds"))
  names(glpf_abs)[1] <- "Wavelength"
  glpf_fl <- readRDS(file.path("raw","GLPF","optics","dffl_noQA.rds"))
  
  # read bacteria and header data
  scales <- c("watershed","subwatershed","small")
  df_HB <- readRDS(file=file.path("process","out","combined_human_markers.rds"))

  glri_grnums <- df_HB[which(df_HB$scale == scales[1]),"GRnumber"]
  mmsd_grnums <- df_HB[which(df_HB$scale == scales[2]),"GRnumber"]
  glpf_grnums <- df_HB[which(df_HB$scale == scales[3]),"GRnumber"]
  
  # load MRLs
  abs_MRL <- readRDS(file.path("process","out","abs_MRLs.rds"))
  fl_MRL <- readRDS(file.path("process","out","fl_MRLs.rds"))
  
  # Adjust abs data with MRLs
  #
  # abs
  #GLRI
  glri_abs_MRL_adjusted <- optMRLAdjust(df = glri_abs,dfMRLs = abs_MRL,Wavelength = "Wavelength",sampleGRnums = glri_grnums)
  
  #MMSD
  abs_MRL_temp <- abs_MRL[which(abs_MRL$Wavelength %in% mmsd_abs$Wavelength),]
  mmsd_grnums_abs <- mmsd_grnums[mmsd_grnums %in% names(mmsd_abs)]
  mmsd_grnums_abs <- mmsd_grnums_abs[mmsd_grnums_abs %in% names(mmsd_abs)]
  mmsd_abs_MRL_adjusted <- optMRLAdjust(df = mmsd_abs,dfMRLs = abs_MRL_temp,Wavelength = "Wavelength",sampleGRnums = mmsd_grnums_abs)
  
  #GLPF
  abs_MRL_temp <- abs_MRL[which(abs_MRL$Wavelength %in% glpf_abs$Wavelength),]
  glpf_abs_MRL_adjusted <- optMRLAdjust(df = glpf_abs,dfMRLs = abs_MRL_temp,Wavelength = "Wavelength",sampleGRnums = glpf_grnums)
  
  saveRDS(glri_abs_MRL_adjusted,file=file.path("process","out","glri_abs_MRL_adjusted.rds"))
  saveRDS(mmsd_abs_MRL_adjusted, file=file.path("process","out","mmsd_abs_MRL_adjusted.rds"))
  saveRDS(glpf_abs_MRL_adjusted, file=file.path("process","out","glpf_abs_MRL_adjusted.rds"))
  
  # fl
  fl_MRL_temp <- fl_MRL[which(as.character(fl_MRL$Wavelength) %in% as.character(mmsd_fl$exem)),]
  mmsd_grnums <- mmsd_grnums[mmsd_grnums %in% names(mmsd_fl)]
  mmsd_fl_MRL_adjusted <- optMRLAdjust(df = mmsd_fl,dfMRLs = fl_MRL_temp,Wavelength = "exem",sampleGRnums = mmsd_grnums)

  fl_MRL_temp <- fl_MRL[which(as.character(fl_MRL$Wavelength) %in% as.character(glri_fl$exem)),]
  glri_fl_MRL_adjusted <- optMRLAdjust(df = glri_fl,dfMRLs = fl_MRL,Wavelength = "exem",sampleGRnums = glri_grnums)

  #!!!!!!!!!!!!!Need to trouble shoot this command. Possibly wrong fl file with inconsistent wavelengths!!!!!!!!!!!!!
  glpf_fl_MRL_adjusted <- optMRLAdjust(df = glpf_fl,dfMRLs = fl_MRL_temp,Wavelength = "exem",sampleGRnums = glpf_grnums)
  
  saveRDS(glri_fl_MRL_adjusted,file=file.path("process","out","glri_fl_MRL_adjusted.rds"))
  saveRDS(mmsd_fl_MRL_adjusted, file=file.path("process","out","mmsd_fl_MRL_adjusted.rds"))
  saveRDS(glpf_fl_MRL_adjusted, file=file.path("process","out","glpf_fl_MRL_adjusted.rds"))
}
