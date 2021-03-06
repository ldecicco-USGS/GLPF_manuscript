# Identify final GLRI models and save as model objects

save_glri_model_objects <- function() {
  library(tidyverse)
  source(file.path("model","src","variable_correlations.R"))
  
  
  #  * Define response variables
  response <- c("lachno2","bacHum")
  names(response) <- c("Lachno","Bachuman")
  
  # Define models

  form <- list()
  form[[1]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[2]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[3]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[4]] <- formula("log_response ~ Turbidity_mean + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[5]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[6]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[7]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[8]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[9]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[10]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[12]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[13]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
  form[[16]] <- formula("log_response ~ M * cosDate + M * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
  
  
  form_names <- c("Turb","Turb_F","Turb_T","Turb_M","Turb_F2","Turb_T2","Turb_M2",
                  "Turb_F_T","Turb_F_M","Turb_T_M","Turb_F_T2","Turb_F_M2","Turb_T_M2",
                  "F_T","F_M","T_M")
  names(form) <- form_names
  
  # Read list of selected models for human indicators
  model_table <- readRDS(file.path("model","out","modeling_summary_table.rds"))
  
  #Agriculture sites
  sites_cat <- "Agriculture"
  parm_cat <- "sensors"
  
  #Lachno: 
  HIB <- "Lachno"
  row_num <- which(model_table$Sites == sites_cat & model_table$`Parameter Category` == parm_cat)
  model_lachno_name <- first(model_table[names(response[HIB])])[row_num]
  model_lachno <- form[model_lachno_name]
  
  
  #Bachuman: 
  HIB <- "Bachuman"
  row_num <- which(model_table$Sites == sites_cat & model_table$`Parameter Category` == parm_cat)
  model_bh_name <- first(model_table[names(response[HIB])])[row_num]
  model_bh <- form[model_bh_name]
  
  glri_models <- list()
  glri_models[[paste0(sites_cat,"Lachno")]] <- model_lachno
  glri_models[[paste0(sites_cat,"Bachuman")]] <- model_bh
  
  
  #CL_RO sites
  sites_cat <- "CL_RO"
  parm_cat <- "sensors"
  
  #Lachno: 
  HIB <- "Lachno"
  row_num <- which(model_table$Sites == sites_cat & model_table$`Parameter Category` == parm_cat)
  model_lachno_name <- first(model_table[names(response[HIB])])[row_num]
  model_lachno <- form[model_lachno_name]
  
  
  #Bachuman: 
  HIB <- "Bachuman"
  row_num <- which(model_table$Sites == sites_cat & model_table$`Parameter Category` == parm_cat)
  model_bh_name <- first(model_table[names(response[HIB])])[row_num]
  model_bh <- form[model_bh_name]
  
  glri_models[[paste0(sites_cat,"Lachno")]] <- model_lachno
  glri_models[[paste0(sites_cat,"Bachuman")]] <- model_bh
  
  
  #JI 
  sites_cat <- "JI"
  parm_cat <- "sensors"
  
  site_combos <- list()
  site_combos[[1]] <- c("JI")
 
  names(site_combos) <- c("JI")
  
  
  form <- list()
  form[[1]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate")
  form[[2]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + sinDate + cosDate")
  form[[3]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate")
  form[[4]] <- formula("log_response ~ Turbidity_mean + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[5]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + sinDate + cosDate")
  form[[6]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
  form[[7]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[8]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
  form[[9]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[10]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
  form[[12]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[13]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
  form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
  form[[16]] <- formula("log_response ~ M * cosDate + M * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
  
  
  form_names <- c("Turb","Turb_F","Turb_T","Turb_M","Turb_F2","Turb_T2","Turb_M2",
                  "Turb_F_T","Turb_F_M","Turb_T_M","Turb_F_T2","Turb_F_M2","Turb_T_M2",
                  "F_T","F_M","T_M")
  names(form) <- form_names
  
  #Lachno: 
  HIB <- "Lachno"
  row_num <- which(model_table$Sites == sites_cat & model_table$`Parameter Category` == parm_cat)
  model_lachno_name <- first(model_table[names(response[HIB])])[row_num]
  model_lachno <- form[model_lachno_name]
  
  
  #Bachuman: 
  HIB <- "Bachuman"
  row_num <- which(model_table$Sites == sites_cat & model_table$`Parameter Category` == parm_cat)
  model_bh_name <- first(model_table[names(response[HIB])])[row_num]
  model_bh <- form[model_bh_name]
  
#  glri_models <- list()
  glri_models[[paste0(sites_cat,"Lachno")]] <- model_lachno
  glri_models[[paste0(sites_cat,"Bachuman")]] <- model_bh
  
  #run models and write to model object list
  source(file.path("model","src","Generate_final_model_objects.R"))
  glri_model_objects <- generate_final_model_objects("glri_summary.rds","GLRI",glri_models)
  
  return(glri_model_objects)
}
