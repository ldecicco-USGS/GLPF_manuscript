# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R

# Intro to Linear mixed models: https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/
# lme4 package vignette describing method with Table 2 describing grouping options:
#   https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

# 1. Load data
# 2. General modeling setup:
#   * Define response variables
#     * Transform response variables
#   * Define predictors
#     * Define interactions
#     * Define grouping variables (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
#         *Using this form: x + (x || g) 1 + x + (1 | g) + (0 + x | g) Uncorrelated random intercept and slope.

#   * Construct formula
#   * Choose sites or states to be included
#   * Filter data to sites and make model df

# 3. Run LME model for all response variables
# 4. Graph results


library(lme4)
library(smwrBase)
library(car)
library(dplyr)
library(cvTools)

source(file.path("model","src","plot_model_cv.R"))

# 1. Load data
df_GLPF <- readRDS(file.path("process","out","GLPF_summary.rds"))
df <- df_GLPF

# 2. General modeling setup:

#  * Define response variables
response <- c("lachno","bacHum","eColi","ent")
#response <- c("Lachno.2.cn.100ml") #,"BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")

# * Transform seasonal variables
df$sinDate <- fourier(df$pdate)[,1]
df$cosDate <- fourier(df$pdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","rS1.25_T","S1.25")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("state")
# 
# #   * Construct formula
# form <- paste("")
# for(j in 1:length(interactors)){ 
#   form_temp <- paste(paste(predictors,"*",interactors[j],sep=""),collapse = " + ")
#   if(j>1) {form <- paste(form_temp, " + ", form) 
#   }else form <- form_temp
# }
# 
# form <- paste("log_response ~ ",form)
# 
# 
# # Add groupings
# 
# for(i in 1:length(groupings)) {
#   
#   group_form <- paste("(",paste(predictors, collapse = " + ")," | ",groupings[i], ")")
#   form <- formula(paste(form, " + ", group_form))
#   
# }

form <- list()
form[[1]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate + (F + T | abbrev)")
form[[2]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate + (F + Turbidity_mean | abbrev)")
form[[3]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate + (T + Turbidity_mean | abbrev)")
form[[4]] <- formula("log_response ~ F * cosDate + F * sinDate + (F | abbrev)")
form[[5]] <- formula("log_response ~ T * cosDate + T * sinDate  + (T | abbrev)")
form[[6]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate  + (Turbidity_mean | abbrev)")
# form[[6]] <- formula("log_response ~ rS1.25_T * cosDate + rS1.25_T * sinDate  + (rS1.25_T | abbrev)")
# form[[7]] <- formula("log_response ~ T * cosDate + T * sinDate + rS1.25_T * cosDate + rS1.25_T * sinDate  + (rS1.25_T + T | abbrev)")
# form[[8]] <- formula("log_response ~ T * cosDate + T * sinDate + S1.25 * cosDate + S1.25 * sinDate  + (S1.25 + T | abbrev)")

# # 3. Run LME model for all response variables

for (i in 1:length(response)) {
  
  filenm <- paste("GLPF_predictions_cv_",response[i],".pdf",sep="")
  pdf(filenm)
  
  #   * transform response variable
  df$log_response <- log10(df[,response[i]])
  #   * Choose sites or states to be included
  sites <- c("JI", "PO", "MA", "CL", "RO", "RM")
  
  #   * Filter data to sites and make model df
  model_rows <- which(df[,groupings] %in% sites)
  model_columns <- c("log_response", response,predictors,interactors,groupings)
  model_df <- df[model_rows,model_columns]
  model_df <- na.exclude(model_df)
  
  
  #   
  x <- as.data.frame(scale(model_df[,predictors]))
  names(x) <- predictors
  
  model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,c(interactors, groupings)])
  names(model_df_scaled) <- c("log_response",predictors,interactors,groupings)
  
  running_mean_cv_rmspe_list <- list()
  for(f in 1:length(form)){
    n_folds <- 10
    n_replications <- 50
    cv_rmspe = numeric()
    running_mean_cv_rmspe <- numeric()
    folds <- cvFolds(nrow(model_df), K=n_folds, R = n_replications)
    
    for (r in 1:n_replications){
      
      for(j in 1:n_folds){
        include <- folds[["subsets"]][which(folds[["which"]]!=j),r]
        df_cv <- model_df_scaled[include,]
        df_predict <- model_df_scaled[-include,]
        #   * Run model
        m <- lmer(form[[f]],data=df_cv)
        df_predict$predictions <- predict(m,newdata=df_predict)
        if(j == 1) {df_predictions <- df_predict
        }else{df_predictions <- rbind(df_predictions,df_predict)
        }
        
      }
      
      cv_rmspe <- c(cv_rmspe,rmspe(df_predict$log_response,df_predict$predictions))
      running_mean_cv_rmspe <- c(running_mean_cv_rmspe,mean(cv_rmspe))
      plot_model_cv(df_predictions,form[[f]])
      
    }
    if(f==1) {
      df_cv_rmspe <- data.frame(form1 = cv_rmspe)
      df_running_mean_cv_rmspe <- data.frame(form1 = running_mean_cv_rmspe)
    }else{
      df_cv_rmspe <- cbind(df_cv_rmspe, cv_rmspe)
      df_running_mean_cv_rmspe <- cbind(df_running_mean_cv_rmspe, running_mean_cv_rmspe)
      
    }
    running_mean_cv_rmspe_list[[f]] <- running_mean_cv_rmspe
    
  }
  names(df_running_mean_cv_rmspe) <-  paste("form",c(1:length(form)),sep="_")
  
  #}
  dev.off()
  shell.exec(filenm)
  
  
  filenm <- paste("running_mean_rmspe_GLPF_",response[i],".pdf",sep="")
  pdf(filenm)
  for(f in 1:length(form)) {
    plot(running_mean_cv_rmspe_list[[f]])
    mtext(form[[f]],cex=0.75)
    mtext(response[i],line=1.5)
  }
  dev.off()
  shell.exec(filenm)
  
  
  filenm <- paste("model_options_GLPF_",response[i],".pdf",sep="")
  model_AICs <- numeric()
  pdf(filenm)
  for(f in 1:length(form)){
    m <- lmer(form[[f]],data=model_df_scaled)
    model_df_scaled$predictions <- predict(m,newdata=model_df_scaled)
    plot_model_cv(model_df_scaled,form[[f]])
    model_AICs <- c(model_AICs,AIC(m))
    
  }

dev.off()
shell.exec(filenm)
}
