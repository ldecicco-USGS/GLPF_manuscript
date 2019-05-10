#defineMRLs.R

library(dplyr)
library(USGSHydroOpt)
library(readxl)

define_MRLs <- function() {
  
  # Use blank samples from all three spatial scales to define MRLs
  
  # Read in raw data files from three spatial scales
  
  #MMSD
  load(file.path("raw","MMSD","PhaseIV","MMSDOpticalData.RData"))
  mmsd_fl <- dffl
  mmsd_abs <- dfabs
  names(mmsd_abs)[1] <- "Wavelength"
  row.names(mmsd_abs) <- mmsd_abs$Wavelength
  names(mmsd_fl) <- substr(names(mmsd_fl),1,7)
  names(mmsd_fl)[1] <- "exem"
  
  #GLRI
  glri_abs <- read.csv(file.path("raw","GLRI","compiled_absorbance_Corr_May2014.csv"),stringsAsFactors = FALSE)
  glri_fl <- read_xlsx(file.path("raw","GLRI","GLRI_CompleteData_091913.xlsx"),sheet = "Vectorized Flourescence_Corr", skip = 1)
  names(glri_abs)[1] <- "Wavelength"
  row.names(glri_abs) <- glri_abs$Wavelength
  names(glri_fl)[1] <- "exem"
  
  #GLPF
  glpf_abs <- readRDS(file.path("raw","GLPF","optics","dfabs_QA.rds"))
  glpf_fl <- readRDS(file.path("raw","GLPF","optics","dffl_QA.rds"))
  #GLPF3DEEMs <- VectorizedTo3DArray(df = glpf_fl, "exem",grnum = "GRnumber")
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
  
  df_blanks <- left_join(mmsd_blanks,glri_blanks, by="Wavelength")
  # Note, all glpf wavelengths are in the glri/mmsd (but some are missing)
  df_blanks <- left_join(df_blanks,glpf_blanks, by="Wavelength")

  abs_MRL <- optMRL(df = df_blanks,Wavelength = "Wavelength",blankGRnums = names(df_blanks)[-1])
  #plot(abs_MRL$MRL~abs_MRL$Wavelength)
  
  ### Combine fl blanks
  # Combine abs blanks
  
  mmsd_blank_cols <- which(names(mmsd_fl) %in% mmsd_GRnumbers$GRnumbers)
  glri_blank_cols <- which(names(glri_fl) %in% glri_GRnumbers$GRnumbers)
  glpf_blank_cols <- which(names(glpf_fl) %in% glpf_GRnumbers$GRnumbers)
  
  names(mmsd_abs)[mmsd_blank_cols]
  mmsd_blanks <- mmsd_fl[,c(1,mmsd_blank_cols)]
  glri_blanks <- glri_fl[,c(1,glri_blank_cols)]
  glpf_blanks <- glpf_fl[,c(1,glpf_blank_cols)]
  
  df_blanks <- left_join(mmsd_blanks,glri_blanks, by="exem")
  df_blanks <- full_join(df_blanks,glpf_blanks)
  
  fl_MRL <- optMRL(df = df_blanks,Wavelength = "exem",blankGRnums = names(df_blanks)[-1])
  
  saveRDS(abs_MRL, file = file.path("process","out","abs_MRLs.rds"))
  saveRDS(fl_MRL, file = file.path("process","out","fl_MRLs.rds"))
  
  save(mmsd_abs,mmsd_fl,file = file.path("process","out","mmsd_optical_Dec_2018_no_MRLs.RData"))
  save(glri_abs,glri_fl,file = file.path("process","out","glri_optical_Dec_2018_no_MRLs.RData"))
  save(glpf_abs,glpf_fl,file = file.path("process","out","glpf_optical_Dec_2018_no_MRLs.RData"))
}

