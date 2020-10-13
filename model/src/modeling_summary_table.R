# Prepare final model summary table for large and sub-watershed scales
library(tidyverse)

modeling_summary_table <- function() {
  GLRI_LMER <- readRDS(file.path("model","out","GLRI_LMER_model_rankings.rds"))
  GLRI_OLS <- readRDS(file.path("model","out","GLRI_OLS_model_rankings.rds"))
  MMSD <- readRDS(file.path("model","out","MMSD_LMER_model_rankings.rds"))
  MMSD_2_site <- readRDS(file.path("model","out","MMSD_LMER_model_rankings_2_site.rds"))
  
  GLRI_response <- c("BACHUM.cn.100mls","Lachno.2.cn.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml","E..coli.CFUs.100ml")
  GLRI_LMER$response <- factor(GLRI_LMER$response,levels = GLRI_response)
  GLRI_LMER$model_run <- factor(GLRI_LMER$model_run,levels = c("sensors","non-cor"))
  
  
  GLRI_OLS$response <- factor(GLRI_OLS$response,levels = GLRI_response)
  GLRI_OLS$model_run <- factor(GLRI_OLS$model_run,levels = c("sensors","non-cor"))
  
  
  #GLRI LMER models
  site_combos <- unique(GLRI_LMER$site_combo)
  response <- levels(GLRI_LMER$response)
  
  GLRI_LMER_models <- GLRI_LMER %>%
    group_by(site_combo,response,model_run) %>%
    filter(rmse_median == min(rmse_median)) %>%
    arrange(site_combo,response,model_run) %>%
    select(model_run,site_combo,response,model,rmse_median) %>%
    mutate(rmse_median = round(rmse_median,2))
  
  
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
  
  GLRI_models_wide <- pivot_wider(data = GLRI_models,names_from = response, values_from = c(model,rmse_median))
  
  GLRI_models_wide  <- GLRI_models_wide[,c(
    names(GLRI_models_wide)[1:2],
    grep("BACHUM",names(GLRI_models_wide),value = TRUE),
    grep("Lachno",names(GLRI_models_wide),value = TRUE),
    grep("ENTERO.cn",names(GLRI_models_wide),value = TRUE),
    grep("Entero.CFU",names(GLRI_models_wide),value = TRUE),
    grep("coli",names(GLRI_models_wide),value = TRUE))]
  
  names(GLRI_models_wide)
  
  GLRI_names <- c("Sites","Parameter Category","Bachuman","RMSE Bachuman","Lachno","RMSE Lachno", "Entero","RMSE Entero",
                  "Entero Culture","RMSE Entero Culture","E. Coli","RMSE E. Coli")
  names(GLRI_names) <- names(GLRI_models_wide)
  
  names(GLRI_models_wide) <- GLRI_names
  
  
  
  # MMSD
  
  #
  MMSD_response <- c("bacHum","lachno2","ent","eColi")
  MMSD$response <- factor(MMSD$response,levels = MMSD_response)
  MMSD$model_run <- factor(MMSD$model_run,levels = c("sensors","non-cor"))
  
  MMSD_2_site$response <- factor(MMSD_2_site$response,levels = MMSD_response)
  MMSD_2_site$model_run <- factor(MMSD_2_site$model_run,levels = c("sensors","non-cor"))
  
  
  site_combos <- unique(MMSD$site_combo)
  response <- levels(MMSD$response)
  
  MMSD_models_3_site <- MMSD %>%
    group_by(site_combo,response,model_run) %>%
    filter(rmse_median == min(rmse_median)) %>%
    arrange(site_combo,response,model_run) %>%
    select(model_run,site_combo,response,model,rmse_median) %>%
    mutate(rmse_median = round(rmse_median,2)) %>%
    select(site_combo,model_run,model,response,rmse_median)
  
  MMSD_models_2_site <- MMSD_2_site %>%
    group_by(site_combo,response,model_run) %>%
    filter(rmse_median == min(rmse_median)) %>%
    arrange(site_combo,response,model_run) %>%
    select(model_run,site_combo,response,model,rmse_median) %>%
    mutate(rmse_median = round(rmse_median,2)) %>%
    select(site_combo,model_run,model,response,rmse_median)
  
  remove_rows <- grep("Turb",MMSD_models_2_site$model)
  if(length(remove_rows) > 0) MMSD_models_2_site <- MMSD_models_2_site[-remove_rows,]
  
  MMSD_models <- as.data.frame(rbind(MMSD_models_2_site,MMSD_models_3_site)) %>%
    select(site_combo,model_run,model,response,rmse_median)
  
  MMSD_models_wide <- pivot_wider(data = MMSD_models,names_from = response, values_from = c(model,rmse_median))
  
  MMSD_models_wide  <- MMSD_models_wide[,c(
    names(MMSD_models_wide)[1:2],
    grep(response[1],names(MMSD_models_wide),value = TRUE),
    grep(response[2],names(MMSD_models_wide),value = TRUE),
    grep(response[3],names(MMSD_models_wide),value = TRUE),
    grep(response[4],names(MMSD_models_wide),value = TRUE))]
  
  names(MMSD_models_wide)

  
  MMSD_names <- c("Sites","Parameter Category","Bachuman","RMSE Bachuman","Lachno","RMSE Lachno", "Entero","RMSE Entero",
                  "E. Coli","RMSE E. Coli")
  names(MMSD_names) <- names(MMSD_models_wide)
  
  names(MMSD_models_wide) <- MMSD_names
  
  models_wide <- full_join(GLRI_models_wide,MMSD_models_wide)
  return(models_wide)
}
