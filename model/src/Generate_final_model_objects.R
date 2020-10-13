# Generate final model objects from formulas and respective data sets

# filenm <- "mmsd_summary.rds"
# project <- "MMSD"
# formulas <- mmsd_models

# filenm <- "glri_summary.rds"
# project <- "GLRI"
# formulas <- glri_models


generate_final_model_objects <- function(filenm, project, formulas){
  library(lme4)
  library(smwrBase)
  library(tidyverse)
  
  
  #Set up data for modeling
  # 1. Load data
  df <- readRDS(file.path("process","out",filenm))
  if(project == "GLRI") df <- rename(df,humanVirus = HumanVirus)
  df$virus_occur <- ifelse(df$humanVirus >= 0.01,1,0)
  
  # 2. General modeling setup:
  
  #  * Define response variables
  if(project == "MMSD") {
    response <- c("lachno2","bacHum")  
    }else{
      response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls")
    }

  # * Transform seasonal variables
  df$sinDate <- fourier(df$psdate)[,1]
  df$cosDate <- fourier(df$psdate)[,2]
  
  # * Define predictors and interaction terms
  predictors<- c("Turbidity_mean", "T", "F","M")

    
  
  #non_int_predictors <- c("CSO")
  interactors <- c("sinDate","cosDate")
  
  # Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
  groupings <- c("abbrev")
  
  site_combos <- list()
  if(project == "MMSD") {
    site_combos[[1]] <- c("MC", "MW", "UW")
    site_combos[[2]] <- c("BK", "CG")
    names(site_combos) <- c("3-sites","2-sites")
  }else{
    site_combos[[1]] <- c("CL", "RO")
    site_combos[[2]] <- c("PO", "MA", "RM")
    site_combos[[3]] <- c("JI")
    names(site_combos) <- c("CL_RO","Agriculture","JI")
  }
  
  

  models <- list()
  
  f <- 0
  for (s in 1:length(site_combos)) {  
    #   * Choose sites or states to be included
    sites <- site_combos[[s]]
    if("CG" %in% sites)  predictors<- c("T", "F","M")
    
    for (i in 1:length(response)) {
      f <- f + 1
      form <- formulas[[f]][[1]]
      form_name <- names(formulas[[f]])
      model_name <- names(formulas)[[f]]

      #filenm <- paste("GLRI_predictions_",response[i],".pdf",sep="")
      #  pdf(filenm)
      
      #   * transform response variable
      df$log_response <- log10(df[,response[i]])
      #   * Filter data to sites and make model df
      model_rows <- which(df[,groupings] %in% sites)
      model_columns <- c("log_response", response,predictors,interactors,groupings,"virus_occur")
      model_df <- df[model_rows,model_columns]
      model_df <- na.exclude(model_df)
      
      # develop dataframe of model variables
      #   -start with predictors
      x <- as.data.frame(scale(model_df[,predictors]))
      names(x) <- predictors
      
      #   -add in response, interactors, and groups
      model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,c(interactors, groupings,"virus_occur")])
      names(model_df_scaled) <- c("log_response",predictors,interactors,groupings,"virus_occur")
      
      #   * Run model
      if(length(grep("JI",model_name)) > 0){
        m <- m <- lm(form,data=model_df_scaled)
        }else{m <- lmer(form,data=model_df_scaled)
        }
      
      model_df$predictions <- predict(m,newdata=model_df_scaled)
      model_df$response <- response[i]
      model_df$sites <- names(site_combos[s])
      
      #Add model to list
      m_list <- list(m,model_df,model_df_scaled)
      models[[paste0(names(site_combos[s]),"_",response[i],"_",model_name,"_",form_name)]] <- m_list
      
    }
  }
  return(models)
}