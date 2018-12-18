#defineMRLs.R

library(dplyr)
library(USGSHydroOpt)

define_MRLs <- function() {

# Use blank samples from all three spatial scales to define MRLs


# Read in raw data files from three spatial scales

#MMSD
load(file.path("raw","MMSD","PhaseIV","MMSDOpticalData.RData"))
mmsd_abs <- dfabs

#GLRI
glri_abs <- read.csv(file.path("raw","GLRI","compiled_absorbance_Corr_May2014.csv"),stringsAsFactors = FALSE)
load(file.path("raw","GLRI","GLRI3DEEMs.RData"))

#GLPF
glpf_abs <- readRDS(file.path("raw","GLPF","optics","dfabs_QA.rds"))
glpf_fl <- readRDS(file.path("raw","GLPF","optics","dffl_QA.rds"))
GLPF3DEEMs <- VectorizedTo3DArray(df = glpf_fl, "exem",grnum = "GRnumber")


# read GR numbers for each data set
mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))

# Combine abs blanks

mmsd_blank_rows <- which(names(mmsd_abs) %in% mmsd_GRnumbers$GRnumbers)
glri_blank_rows <- which(names(glri_abs) %in% glri_GRnumbers$GRnumbers) #!!!! Not all blanks are in the abs file!!!!!
glpf_blank_rows <- which(names(glpf_abs) %in% glpf_GRnumbers$GRnumbers)


test <- merge(mmsd_abs[mmsd_blank_rows,],glri_abs[glri_blank_rows,],by="exem")

saveRDS(abs_MRLs, file = file.path("processed","out","abs_MRLs.rds"))
saveRDS(fl_MRLs, file = file.path("processed","out","fl_MRLs.rds"))

              

which(!(glri_GRnumbers$GRnumbers %in% names(glri_abs)))
}
