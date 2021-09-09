# Model archive for "Optical properties of water for prediction of wastewater contamination, human-associated bacteria, and fecal indicator bacteria in surface water"

library(tidyverse)
library(smwrBase)

source(file.path("model_archive","src","get_formulas.R"))
source(file.path("model","src","variable_correlations.R"))

# Read model table
model_table <- readRDS("model/out/modeling_summary_table.rds")
model_table$project <- c(rep("GLRI",6),rep("MMSD",4))



# Model setup
## 1. Define project
### a. define project data set
### b. define response variables'
### c. define sites
### d. subset data to chosen sites
## 2. transform variables (season)
## 3. Modeling 
### a. Step through response organisms
### b. define model type (sensor, non-cor)
### c. define model list
### d. choose specific model formula for current sites/model type
## 4. 

#Run models
#

j <- 6

## MODELING ROUTINE ##
groupings <- c("abbrev")

#for(j in 1:dim(model_table)[1]){  ###### Begin model table loop  ################
for(j in 5:6){
  project <- model_table$project[j]
  parm_category <- model_table[j,"Parameter Category"]
  
  # Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
  ## 1. Define project: data, response variables, and sites
  if(project == "MMSD"){
    
    ### a. define project data set
    #### Load data
    df_MMSD <- readRDS(file.path("process","out","mmsd_summary.rds"))
    df_orig <- df_MMSD
    
    #  * Define response variables
    response <- c("bacHum","lachno2","ent","eColi")
    response_header_name <- c("Bachuman","Lachno","Entero","E. Coli")
    
    site_combos <- list()
    site_combos[[1]] <- c("CG", "BK")
    site_combos[[2]] <- c("UW","MW","MC")
    names(site_combos) <- c("2-sites","3-sites")
    
  }else if(project == "GLRI"){
    df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
    df_orig <- df_GLRI
    
    response <- c("BACHUM.cn.100mls","Lachno.2.cn.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml","E..coli.CFUs.100ml")
    response_header_name <- c("Bachuman","Lachno","Entero", "Entero Culture","E. Coli")
    
    site_combos <- list()
    site_combos[[1]] <- c("CL", "RO")
    site_combos[[2]] <- c("PO", "MA", "RM")
    site_combos[[3]] <- c("JI")
    names(site_combos) <- c("CL_RO","Agriculture","JI")
    
  }
  names(response) <- response_header_name
  df <- df_orig
  
  # General modeling setup:
  
  ## 2. Transform variables (season)
  df$sinDate <- fourier(df$psdate)[,1]
  df$cosDate <- fourier(df$psdate)[,2]
  
  ## 3. Modeling: Step through response variables 
  ## 3. Define model formula
  
  #Define model formulas
  sites <- pull(model_table[j,"Sites"])
  parm_category <- as.character(pull(model_table[j,"Parameter Category"]))
  formulas <- get_formulas(sites,parm_category)
  
  
  
  #   * Filter data to sites and make model df
  model_df <- df %>%
    filter(abbrev %in% site_combos[sites])
  
  
  ### START MODELING  ###  
  if(j == 5) median_rmse <- data.frame(rmspe_median = numeric(),variables=character(),response=character(),type =character())
  
  if(sites == "JI") {
    # OLS modeling for JI
    median_rmse <- bind_rows(median_rmse,
      OLS_modeling(df, parm_category, formulas, response, model_table, sites)) #output = # dataframe with 
    #data frame with one row that has the equivalent of the model_table.
    
  }else {
    lmer_modeling()
  }
  
}

