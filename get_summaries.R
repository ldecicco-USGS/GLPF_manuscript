library(USGSHydroOpt)
library(tidyverse)
library(readxl)

source(file.path("process","src","run_suite.R"))

glri <- readRDS(file.path("process","out","glri_fl_MRL_adjusted.rds"))
mmsd <- readRDS(file.path("process","out","mmsd_fl_MRL_adjusted.rds"))
glpf <- readRDS(file.path("process","out","glpf_fl_MRL_adjusted.rds"))

glri_fl <- glri$df2
mmsd_fl <- mmsd$df2
glpf_fl <- glpf$df2

rm(glpf, glri, mmsd)

glri_abs <- readRDS(file.path("process","out","glri_abs_MRL_adjusted.rds"))
mmsd_abs <- readRDS(file.path("process","out","mmsd_abs_MRL_adjusted.rds"))
glpf_abs <- readRDS(file.path("process","out","glpf_abs_MRL_adjusted.rds"))

glri_abs <- glri_abs$df2
mmsd_abs <- mmsd_abs$df2
glpf_abs <- glpf_abs$df2

# GLRI Summary:
df_glri <- read_xlsx(file.path("raw","GLRI","GLRI_102714 data dump_vectorized_plus ABS vect.xlsx"),sheet = "qryCompDataQuery_KO")
df_glri <- df_glri[,1:18]

df_glri <- df_glri %>%
  filter(GRnumber %in% names(glri_fl)[-1]) %>%
  data.frame()

# GLPF Summary:
df_glpf <- readRDS(file.path("raw","GLPF","summary_noQA.rds"))
df_glpf <- df_glpf[,1:(which(names(df_glpf) == "OB1")-1)]
df_glpf <- df_glpf %>% 
  rename(GRnumber=CAGRnumber) %>%
  filter(GRnumber %in% names(glpf_fl)[-1])

# MMSD Summary:
load(file.path("raw","MMSD","PhaseIII","dfOptAnalysisDataMMSDJan2015.RData"))
mmsd_iv <- data.table::fread(file.path("raw","MMSD","PhaseIV","MMSDOptSummary.csv"), data.table = FALSE, skip = 1)

df_mmsd_iii <- dfOptSumAll[,-(which(names(dfOptSumAll)=="OB1"):dim(dfOptSumAll)[2])]
mmsd_fl_iv <- mmsd_fl[,c("exem",mmsd_iv$GRnumber[mmsd_iv$GRnumber %in% names(mmsd_fl)])]
mmsd_abs_iv <- mmsd_abs[,c("Wavelength",mmsd_iv$GRnumber[mmsd_iv$GRnumber %in% names(mmsd_abs)])]
mmsd_iv <- mmsd_iv[,1:29]
mmsd_iv <- mmsd_iv %>%
  filter(GRnumber %in% names(mmsd_fl_iv)[-1] &
           GRnumber %in% names(mmsd_abs[-1]))

dir.create(file.path("process","out"),showWarnings = FALSE)  

glri_sum <- run_suite(df_glri, glri_fl, glri_abs)
saveRDS(glri_sum, file = file.path("process","out","glri_summary.rds"))

glpf_sum <- run_suite(df_glpf, glpf_fl, glpf_abs)
saveRDS(glpf_sum, file = file.path("process","out","glpf_summary.rds"))

mmsd_iv_sum <- run_suite(mmsd_iv, mmsd_fl_iv, mmsd_abs_iv)
saveRDS(mmsd_iv_sum, file = file.path("process","out","mmsd_iv_summary.rds"))

# rm(list = ls())
# glri_sum <- readRDS(file.path("process","out","glri_summary.rds"))
# glpf_sum <- readRDS(file.path("process","out","glpf_summary.rds"))
# mmsd_sum <- readRDS(file.path("process","out","mmsd_iv_summary.rds"))                  
