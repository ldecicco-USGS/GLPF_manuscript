#defineMRLs.R

library(dplyr)
library(USGSHydroOpt)
library(readxl)

define_MRLs <- function() {
  
  # Use blank samples from all three spatial scales to define MRLs
  
  
  # Read in raw data files from three spatial scales
  
  #MMSD
  load(file.path("raw","MMSD","PhaseIV","MMSDOpticalData.RData"))
  mmsd_abs <- dfabs
  names(mmsd_abs)[1] <- "Wavelength"
  row.names(mmsd_abs) <- mmsd_abs$Wavelength
  
  #GLRI
  glri_abs <- read.csv(file.path("raw","GLRI","compiled_absorbance_Corr_May2014.csv"),stringsAsFactors = FALSE)
  #glri_abs <- read_xlsx(file.path("raw","GLRI","GLRI_102714 data dump_vectorized_plus ABS vect.xlsx"),sheet = "Absorbance Aqua_GLRI Phase II")
  #names(glri_abs) <- substr(names(glri_abs),1,7)
  #load(file.path("raw","GLRI","GLRI3DEEMs.RData"))
  names(glri_abs)[1] <- "Wavelength"
  row.names(glri_abs) <- glri_abs$Wavelength
  
  #GLPF
  glpf_abs <- readRDS(file.path("raw","GLPF","optics","dfabs_QA.rds"))
  glpf_fl <- readRDS(file.path("raw","GLPF","optics","dffl_QA.rds"))
  GLPF3DEEMs <- VectorizedTo3DArray(df = glpf_fl, "exem",grnum = "GRnumber")
  names(glpf_abs)[1] <- "Wavelength"
  row.names(glpf_abs) <- glpf_abs$Wavelength
  
  # read GR numbers for each data set
  mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
  mmsd_GRnumbers$GRnumbers <- as.character(mmsd_GRnumbers$GRnumbers)
  glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
  glri_GRnumbers$GRnumbers <- as.character(glri_GRnumbers$GRnumbers)
  glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))
  glpf_GRnumbers$GRnumbers <- as.character(glpf_GRnumbers$GRnumbers)
  
  # Combine abs blanks
  
  mmsd_blank_cols <- which(names(mmsd_abs) %in% mmsd_GRnumbers$GRnumbers)
  glri_blank_cols <- which(names(glri_abs) %in% glri_GRnumbers$GRnumbers)
  glpf_blank_cols <- which(names(glpf_abs) %in% glpf_GRnumbers$GRnumbers)
  
  names(mmsd_abs)[mmsd_blank_cols]
  mmsd_blanks <- mmsd_abs[,c(1,mmsd_blank_cols)]
  glri_blanks <- glri_abs[,c(1,glri_blank_cols)]
  glpf_blanks <- glpf_abs[,c(1,glpf_blank_cols)]
  
  df_blanks <- left_join(mmsd_blanks,glri_blanks)
  df_blanks <- left_join(df_blanks,glpf_blanks)
  
 
  abs_MRL <- optMRL(df = df_blanks,Wavelength = "Wavelength",blankGRnums = names(df_blanks)[-1])
  plot(test$MRL)
  
  saveRDS(abs_MRLs, file = file.path("processed","out","abs_MRLs.rds"))
  #saveRDS(fl_MRLs, file = file.path("processed","out","fl_MRLs.rds"))
  
}

# 
# glpf_IN_glri <- glpf_GRnumbers$GRnumbers[glpf_GRnumbers$GRnumbers %in% glri_GRnumbers$GRnumbers]
# 
# mmsd_IN_glri <- mmsd_GRnumbers$GRnumbers[mmsd_GRnumbers$GRnumbers %in% glri_GRnumbers$GRnumbers]
# glri_IN_mmsd <- glri_GRnumbers$GRnumbers[glri_GRnumbers$GRnumbers %in% mmsd_GRnumbers$GRnumbers]
# 
# glpf_GRnumbers$GRnumbers[glpf_GRnumbers$GRnumbers %in% mmsd_GRnumbers$GRnumbers]
# 
# 
# 
# #look into missing blank samples in abs file
# missing <- which((glri_GRnumbers$GRnumbers %in% names(glri_abs))) #!!!! Not all blanks are in the abs 
# 
# 
# 
# test <- df_blanks[!(df_blanks$GRnumber %in% as.character(glri_GRnumbers[missing,])),]
# 



