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
library(ggplot2)

source(file.path("model","src","plot_model_cv.R"))

# 1. Load data
df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI

# 2. General modeling setup:

#  * Define response variables
response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")

# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","OB1")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")
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

form_names <- c("F,T","F,Turb","T,Turb","F","T","Turb")
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

names(form) <- form_names
# # 3. Run LME model for all response variables

wd <- getwd()
setwd("./plots/out/model_results")

for (i in 1:length(response)) {
  
  filenm <- paste("GLRI_predictions_cv_",response[i],".pdf",sep="")
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
    n_replications <- 3
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
    
    if(f==1) {running_mean_cv_rmspe_df <- 
      data.frame(rmspe = running_mean_cv_rmspe,variables=form_names[f],response=response[i])
    }else{
      running_mean_cv_rmspe_df <- 
        rbind(running_mean_cv_rmspe_df, 
              data.frame(rmspe = running_mean_cv_rmspe,variables=form_names[f],response=response[i]))
    }
    
  }
  names(df_running_mean_cv_rmspe) <-  paste("form",c(1:length(form)),sep="_")
  
  #}
  dev.off()
  shell.exec(filenm)
  
  
  filenm <- paste("GLRI_model_options_",response[i],".pdf",sep="")
  pdf(filenm,width = 11,height = 8)
  
  plot_df <- do.call(cbind,running_mean_cv_rmspe_list)
  plot_df <- as.data.frame(plot_df)
  names(plot_df) <- names(form)
  
  plot_df <- tidyr::gather(plot_df)
  ggplot(data=plot_df,aes(x=key,y=value)) + 
    geom_boxplot() + 
    ggtitle(paste0(response[i],":    Root Mean Square Prediction Error for ",n_replications," replications of each Model Option")) +
    theme(plot.title = element_text(size = 12))
  
  # boxplot(plot_df,las=2)
  # mtext(response[i],line=1.5)
  # mtext(paste("Root Mean Square Prediction Error for ",n_replications,"replications of each Model Option"))
  
  # Plot observed vs predicted models
  # Compile fitted vs observed in df with three columns
  # Fitted, observed, model description (response & predictors)
  colorOptions <- c("orange","yellow2","skyblue","black","springgreen4","blue","grey","darkorchid1")
  names(colorOptions) <- legend_names <- sort(unique(model_df$abbrev))
  plot_Colors <- colorOptions[model_df[,"abbrev"]]
  abbrev <- model_df$abbrev
  
  model_AICs <- numeric()
  model_results_df <- NULL
  for(f in 1:length(form)){
    m <- lmer(form[[f]],data=model_df_scaled)
    predicted <- predict(m,newdata=model_df_scaled)
    observed <- model_df_scaled$log_response
    model_name <- paste(response[i],form_names[f],sep=" ~ ")
    model_results_df <- rbind(model_results_df,data.frame(predicted,observed,model_name,plot_Colors,abbrev))
    model_AICs <- c(model_AICs,AIC(m))
    names(model_AICs)[f] <- form[f]
  }
  names(model_results_df) <- c("predicted","observed","model_name","Plot_colors","abbrev")
  model_plot <- ggplot(data = model_results_df,aes(x = observed,y = predicted,, color=abbrev)) + 
    geom_point() +
    #    geom_point(colour=model_results_df$Plot_colors) +
    scale_color_brewer(palette="Dark2") +
    geom_abline(intercept = 0, slope = 1, color="blue", 
                linetype="dashed", size=0.5) +
    facet_wrap(~ model_name)
  model_plot  
  
  dev.off()
  shell.exec(filenm)
}

setwd(wd)
