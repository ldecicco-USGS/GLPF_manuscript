# Workflow to test the impact the choice to use the detection level for censored values.

### 
#Outline of workflow for the three different spatial scales
#
#1. Data processing: 
#  a. MRL determination for optical data
#      -Question: can we determine a common MRL for all data sets or separate?
#    i.   Locate blanks for each data set
#    ii.  Compute MRLs
#    iii. Apply MRLs to each raw data set
#   
#  b. Compute summary optical data parameters
#    i.   Develop common set of parameters needed
#       - determine frequency of censored values for each parameter for each data set
#       - decide which parameters to keep 
#       - modify Optical summary definitions
#    ii.  Use HydroOpt routines to compute 
#    iii. Result: summary optical data sets
#  c. Combine summary optical data sets with bacteria data
#    i.   Use GR numbers form CA lab and FT numbers from UWM lab. These are 
#         all joined already from previous data tasks
#    ii.  Use optical parameters from the final GLPF data set to define
#         optical parameters for all three spatial scales
#
#2. Data description
#  a. Generate plot (figure 2) with concentration and occurrence of HB
#    i. Done: script = Figure 2.R, Results.Rmd
#  b. Determine numbers of samples and such for adding to text
#    ii. Began this task: script = Results.Rmd
#
#3. Modeling
#  a. Large watersheds
#    i.   Begin with LME modeling of common parameters
#    ii.  Choose groups of sites where this works
#    iii. Explore additional parameters for watersheds where that doesn't work
#    iv.  Develop summary table of models
#  b. Subwatersheds
#    i.  LME modeling with common parameters
#    ii. Include in modeling table with Large watersheds
#  c. Small 
#



##########################################
# Project Setup
##########################################

# Packages in all scripts included here

library(tidyverse)
library(USGSHydroOpt)
library(scales)
library(USGSHydroTools)
library(lme4)
library(smwrBase)


## Don't run again. Files have been saved ##
#    C. Functions for adjusting raw data to include MRLs
source(file = file.path("process","src","applyMRLs_detection_level_test.R"))
source(file = file.path("process","src","optMRLAdjust.R"))

#   Apply MRLs
apply_MRLs(multiplier = 0.1)



#Add summary variables with modified censored values (0.5 * detection)
source(file.path("process", "src","get_summaries_0.5_dl.R"))
get_summaries(multiplier = 0.1)


GLRI_formulas <- readRDS(file.path("process","out","GLRI_formulas.rds"))
names(GLRI_formulas)

#Read GLRI data
glri <- readRDS(file.path("process","out","glri_summary.rds"))
glri_0.1 <- readRDS(file.path("process","out","glri_summary_0.1_dl.rds"))
glri_1 <- readRDS(file.path("process","out","glri_summary_1_dl.rds"))

# * Transform seasonal variables
glri$sinDate <- fourier(glri$psdate)[,1]
glri$cosDate <- fourier(glri$psdate)[,2]

glri_0.1$sinDate <- fourier(glri_0.1$psdate)[,1]
glri_0.1$cosDate <- fourier(glri_0.1$psdate)[,2]

glri_1$sinDate <- fourier(glri_1$psdate)[,1]
glri_1$cosDate <- fourier(glri_1$psdate)[,2]

#test GLRI ag models for all organisms
response <- c("BACHUM.cn.100mls", "Lachno.2.cn.100ml","ENTERO.cn.100mls", "Entero.CFUs.100ml","E..coli.CFUs.100ml")
ag_models <- c("Turb_F_T","Turb_F_T","Turb_F","Turb_F","Turb_F")
bact_DLs <- c(225,225,225,1,1)
names(ag_models) <- response
names(bact_DLs) <- response



############ Explore different substitutions for optical (original, 0.1, and 1) #############################
for(i in 1:length(response)) {
  glri$log_response <- log10(glri[,response[i]])
  glri_0.1$log_response <- log10(glri_0.1[,response[i]])
  glri_1$log_response <- log10(glri_1[,response[i]])
  
  sites <- c("MA","PO","RM")
  
  form <- formula(as.character(GLRI_formulas[ag_models[response[i]]]))
  
  m <- lmer(form, data = (glri %>% filter(abbrev %in% sites)))
  m_0.1 <- lmer(form, data = (glri_0.1 %>% filter(abbrev %in% sites)))
  m_1 <- lmer(form, data = (glri_1 %>% filter(abbrev %in% sites)))
  
  plot(predict(m_0.1),predict(m_1))
  summary(predict(m_0.1)/predict(m_1))
  summary(predict(m)/predict(m_1))
  summary(predict(m)/predict(m_0.1))
  
  if(i == 1) {
    df_predict <- data.frame(response = response[i],
                             model = "Ag",
                             predict_orig = predict(m), 
                             predict_0.1 = predict(m_0.1),
                             predict_1 = predict(m_1))
  } else {
    df_predict <- bind_rows(df_predict,data.frame(response = response[i],
                                                  model = "Ag",
                                                  predict_orig = predict(m), 
                                                  predict_0.1 = predict(m_0.1),
                                                  predict_1 = predict(m_1)))
    
    
  }
}

df_predict_optical_summary <- df_predict %>%
  mutate(ratio_0.1 = predict_0.1/predict_orig,
         ratio_1 = predict_1/predict_orig,
         diff_0.1 = predict_0.1 - predict_orig,
         diff_1 = predict_1 - predict_orig) %>%
  group_by(model,response) %>%
  summarise(mean_0.1 = mean(ratio_0.1),
            median_0.1 = median(ratio_0.1),
            stdev_0.01 = sd(ratio_0.1),
            mean_1 = mean(ratio_1),
            median_1 = median(ratio_1),
            stdev_1 = sd(ratio_1),            
            mean_diff0.1 = mean(diff_0.1),
            median_diff_0.1 = median(diff_0.1),
            stdev_diff_1 = sd(diff_1),
            median_diff_1 = median(diff_1),
            stdev_diff_1 = sd(diff_1))

##############################################################################

######## Now try 0.1 * LOD vs LOD for bacteria ####################################


for(i in 1:length(response)) {
  glri$response <- glri[,response[i]]
  censored <- glri$response <= bact_DLs[response[i]]
  glri_0.1 <- glri
  multiplier <- 0.1
  glri$log_response_0.1 <- log10(ifelse(censored,bact_DLs[response[i]]*multiplier,glri$response))
  multiplier <- 1
  glri$log_response_1 <- log10(ifelse(censored,bact_DLs[response[i]]*multiplier,glri$response))
  
  sites <- c("MA","PO","RM")
  
  form <- formula(as.character(GLRI_formulas[ag_models[response[i]]]))
  
  glri$log_response <- log10(glri[,response[i]])
  m <- lmer(form, data = (glri %>% filter(abbrev %in% sites)))
  
  glri$log_response <- glri$log_response_0.1
  m_0.1 <- lmer(form, data = (glri %>% filter(abbrev %in% sites)))
  
  glri$log_response <- glri$log_response_1
  m_1 <- lmer(form, data = (glri %>% filter(abbrev %in% sites)))
  
  plot(predict(m_0.1),predict(m_1))
  summary(predict(m_0.1)/predict(m_1))
  summary(predict(m)/predict(m_1))
  summary(predict(m)/predict(m_0.1))
  
  if(i == 1) {
    df_predict <- data.frame(response = response[i],
                             model = "Ag",
                             predict_orig = predict(m), 
                             predict_0.1 = predict(m_0.1),
                             predict_1 = predict(m_1))
  } else {
    df_predict <- bind_rows(df_predict,data.frame(response = response[i],
                                                  model = "Ag",
                                                  predict_orig = predict(m), 
                                                  predict_0.1 = predict(m_0.1),
                                                  predict_1 = predict(m_1)))
    
    
  }
}

df_predict_bact_summary <- df_predict %>%
  mutate(ratio_0.1 = predict_0.1/predict_orig,
         ratio_1 = predict_1/predict_orig,
         diff_0.1 = predict_0.1 - predict_1) %>%
  group_by(model,response) %>%
  summarise(mean_0.1 = mean(ratio_0.1),
            median_0.1 = median(ratio_0.1),
            stdev_0.01 = sd(ratio_0.1),
            mean_1 = mean(ratio_1),
            median_1 = median(ratio_1),
            stdev_1 = sd(ratio_1),            
            mean_diff0.1 = mean(diff_0.1),
            median_diff_0.1 = median(diff_0.1))

glri$log_response <- log10(glri$response)
df_log_response <- glri[,grep("response",names(glri),value = TRUE)]


