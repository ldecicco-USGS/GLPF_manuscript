
library(dplyr)
source(file.path("model","src","rmse_model_selection.R"))




#### GLRI ###
#Evaluate GLRI LMER models:

site_combos <- list()
site_combos[[1]] <- c("CL", "RO")
site_combos[[2]] <- c("PO", "MA", "RM")
names(site_combos) <- c("CL_RO","Agriculture")
sites <- names(site_combos)[2]

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_9_",sites,".rds",sep=""))
df <- readRDS(file=filenm)

GLRI_LMER_selection <- rmse_model_selection(df)

#non-correlated extra variables
filenm <- file.path("model","out","rmse_and_sites_Oct_10_no_corrRO.rds",sep="")
df <- readRDS(file=filenm)

GLRI_LMER_non_cor_selection <- rmse_model_selection(df)


#Evaluate GLRI single site OLS models

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_10_","RO",".rds",sep=""))
df <- readRDS(file=filenm)

GLRI_OLS_selection <- rmse_model_selection(df)


#non-correlated extra variables for single sites

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_10_no_corr","RO",".rds",sep=""))
df <- readRDS(file=filenm)

GLRI_OLS_non_cor_selection <- rmse_model_selection(df)


### MMSD ###

#LMER

filenm <- file.path("model","out","rmse_and_sites_Oct_9_3-sites.rds",sep="")
df <- readRDS(file=filenm)

MMSD_selection <- rmse_model_selection(df)


#non-correlated extra variables

filenm <- file.path("model","out","")
df <- readRDS(file=filenm)

GLRI_OLS_non_cor_selection <- rmse_model_selection(df)

