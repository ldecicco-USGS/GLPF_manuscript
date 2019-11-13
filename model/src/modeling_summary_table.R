# Prepare final model summary table for large and sub-watershed scales
library(tidyverse)

GLRI_LMER <- readRDS(file.path("model","out","GLRI_LMER_model_rankings.rds"))
GLRI_OLS <- readRDS(file.path("model","out","GLRI_OLS_model_rankings.rds"))
MMSD <- readRDS(file.path("model","out","MMSD_LMER_model_rankings.rds"))

GLRI_response <- c("BACHUM.cn.100mls","Lachno.2.cn.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml","E..coli.CFUs.100ml")
GLRI_LMER$response <- factor(GLRI_LMER$response,levels = GLRI_response)
GLRI_LMER$model_run <- factor(GLRI_LMER$model_run,levels = c("sensors","non-cor"))


GLRI_OLS$response <- factor(GLRI_OLS$response,levels = GLRI_response)
GLRI_OLS$model_run <- factor(GLRI_OLS$model_run,levels = c("sensors","non-cor"))


#GLRI LMER models
site_combos <- unique(GLRI_LMER$site_combo)
response <- unique(GLRI_LMER$response)

GLRI_LMER_models <- GLRI_LMER %>%
  group_by(site_combo,response,model_run) %>%
  filter(rmse_median == min(rmse_median)) %>%
  arrange(site_combo,response,model_run) %>%
  select(model_run,site_combo,response,model,rmse_median)

GLRI_OLS_models <- GLRI_OLS %>%
  filter(site_combo == "JI") %>%
  group_by(site_combo,response,model_run) %>%
  filter(rmse_median == min(rmse_median)) %>%
  arrange(site_combo,response,model_run) %>%
  select(model_run,site_combo,response,model,rmse_median) %>%
  mutate(rmse_median = round(rmse_median,2))
  

GLRI_models <- as.data.frame(rbind(GLRI_LMER_models,GLRI_OLS_models)) %>%
#  mutate(organism = paste(response,model_run,sep="_")) %>%
  select(site_combo,model_run,model,response,rmse_median)

test <- pivot_wider(data = GLRI_models,names_from = response, values_from = c(model,rmse_median))



df <- data.frame(site_combos = c(rep("n1",8),rep("n2",8)),model_run = rep(c("x","y"),8),response = c(rep(c(org1,org2,letters[1:8]),model = 1:16,rmse = 101:116)

duh <- pivot_wider(df,names_from = response,values_from = c(model,rmse))
duh

fish_encounters
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)
