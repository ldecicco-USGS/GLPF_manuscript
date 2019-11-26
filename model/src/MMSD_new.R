# OLS regression for single sites, including Jones Island, Rouige, and Clinton
# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R
#

# NOT RELEVANT FOR SINGLE SITE MODEL
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
library(ggpubr)


source(file.path("model","src","plot_model_cv.R"))
source(file.path("model","src","variable_correlations.R"))

# 1. Load data
df_MMSD <- data.table::fread("raw/MMSD_new/MMSDOsummary_environmental_BAC_FLOWTempTurb.csv", data.table = FALSE)
df_MMSD$psdate <- lubridate::parse_date_time(paste(df_MMSD$ActivityStartDate,
                                                   df_MMSD$ActivityStartTime.Time), 
                                             c("%m/%d/%Y %H:%M:%S"), tz = "America/Chicago")
df <- df_MMSD

# 2. General modeling setup:

#  * Define response variables

response <- c("entero..cfu.100.ml.", "EC..cfu.100ml.",                          
              "FC..cfu.100ml.", "HF183.HUMAN.BACTEROIDES..CN.100ml.",
              "ENTEROCOCCUS..CN.100ml.", "E..COLI..CN.100ml.",
              "LACHNO3.HUMAN.LACHNOSPIRACEAE..CN.100ml.")
#Set censored values to detection limit
# MDL <- c(225,225,1,225,1)
MDL <- c(226, 226, 226, 1, 1, 1, 1)
names(MDL) <- response

for(i in response){
  df[,i] <- as.numeric(df[,i])
  cat(min(df[[i]], na.rm = TRUE), "\n")
  plot(df[[i]], ylab = i) 
  df[,i] <- ifelse(df[, i] <= MDL[i],
                             MDL[i],
                             df[,i])
}

# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("TURB__mean15", "T", "F","M")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate","psdate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA



form <- list()
form[[1]] <- formula("log_response ~ TURB__mean15 + fourier(psdate) + TURB__mean15*fourier(psdate)")
form[[2]] <- formula("log_response ~ TURB__mean15 + F * fourier(psdate) + fourier(psdate)")
form[[3]] <- formula("log_response ~ TURB__mean15 + T * fourier(psdate) + fourier(psdate)")
form[[4]] <- formula("log_response ~ TURB__mean15 + M * fourier(psdate) + fourier(psdate)")
form[[5]] <- formula("log_response ~ TURB__mean15 * fourier(psdate) + F * fourier(psdate) + fourier(psdate)")
form[[6]] <- formula("log_response ~ TURB__mean15 * fourier(psdate) + T * fourier(psdate) + fourier(psdate)")
form[[7]] <- formula("log_response ~ TURB__mean15 * fourier(psdate) + M * fourier(psdate) + fourier(psdate)")
form[[8]] <- formula("log_response ~ TURB__mean15 + F * fourier(psdate) + T * fourier(psdate) + fourier(psdate)")
form[[9]] <- formula("log_response ~ TURB__mean15 + F * fourier(psdate) + M * fourier(psdate) + fourier(psdate)")
form[[10]] <- formula("log_response ~ TURB__mean15 + T * fourier(psdate) + M * fourier(psdate) + fourier(psdate)")
form[[11]] <- formula("log_response ~ TURB__mean15 * fourier(psdate) + F * fourier(psdate) + T * fourier(psdate) + fourier(psdate)")
form[[12]] <- formula("log_response ~ TURB__mean15 * fourier(psdate) + F * fourier(psdate) + M * fourier(psdate) + fourier(psdate)")
form[[13]] <- formula("log_response ~ TURB__mean15 * fourier(psdate) + T * fourier(psdate) + M * fourier(psdate) + fourier(psdate)")
form[[14]] <- formula("log_response ~ F * fourier(psdate) + T * fourier(psdate) + fourier(psdate)")
form[[15]] <- formula("log_response ~ F * fourier(psdate) + M * fourier(psdate) + fourier(psdate)")
form[[16]] <- formula("log_response ~ M * fourier(psdate) + T * fourier(psdate) + fourier(psdate)")

form_names <- c("Turb","Turb_F","Turb_T","Turb_M","Turb_F2","Turb_T2","Turb_M2",
                "Turb_F_T","Turb_F_M","Turb_T_M","Turb_F_T2","Turb_F_M2","Turb_T_M2",
                "F_T","F_M","T_M")

#df_cor <- correlated_to_primary_signals()
#variables <- reduce_correlated_variables(df_cor)
sensors <- c("F","T","M")

turb <- "TURB__mean15"

names(form) <- form_names[1:length(form)]
# # 3. Run LME model for all response variables

# Set boundary tolerance for singularity consistent with "isSingular()"
options(lmerControl(boundary.tol=1e-4))

cv_sites <- data.frame(predictions = numeric(),
                       log_response = numeric(),
                       model=character(),
                       site_combo=character(),
                       response=character(),
                       replication = numeric())

for (i in response) {
  #   * transform response variable
  df$log_response <- log10(df[,i])
  
  model_columns <- c("log_response", i, predictors, interactors)
  model_df <- df[,model_columns]
  model_df <- na.exclude(model_df)
  
  # develop dataframe of model variables
  #   -start with predictors
  x <- as.data.frame(scale(model_df[,predictors]))
  names(x) <- predictors
  
  #   -add in response, interactors, and groups
  model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,interactors])
  names(model_df_scaled) <- c("log_response",predictors,interactors)
  
  running_mean_cv_rmspe_list <- list()
  for(f in 1:length(form)){
    n_folds <- 5
    n_replications <- 50
    cv_rmspe = numeric()
    running_mean_cv_rmspe <- numeric()
    set.seed(1000)  # set seed for reproducibility
    folds <- cvFolds(nrow(model_df), K=n_folds, R = n_replications)
    
    for (r in 1:n_replications){
      
      for(j in 1:n_folds){
        include <- folds[["subsets"]][which(folds[["which"]]!=j),r]
        df_cv <- model_df_scaled[include,]
        df_predict <- model_df_scaled[-include,]
        #   * Run model
        m <- lm(form[[f]],data=df_cv)
        df_predict$predictions <- predict(m,newdata=df_predict)
        if(j == 1) {
          df_predictions <- df_predict
        } else{
          df_predictions <- rbind(df_predictions,df_predict)
        }
        
      }
      
      cv_rmspe <- c(cv_rmspe,rmspe(df_predictions$log_response,df_predictions$predictions))
      cv_sites_addition <- df_predictions[,c("predictions","log_response")]
      cv_sites_addition$model <- form_names[f]
      cv_sites_addition$response <- i
      cv_sites_addition$replication <- r
      cv_sites <- rbind(cv_sites,cv_sites_addition)
      running_mean_cv_rmspe <- c(running_mean_cv_rmspe,mean(cv_rmspe))
      # plot_model_cv(df_predictions,form[[f]])
      
    }
    if(f==1) {
      df_cv_rmspe <- data.frame(form1 = cv_rmspe)
      df_running_mean_cv_rmspe <- data.frame(form1 = running_mean_cv_rmspe)
    } else {
      df_cv_rmspe <- cbind(df_cv_rmspe, cv_rmspe)
      #df_cv_sites <- cbind(df_cv_sites,cv_sites)
      df_running_mean_cv_rmspe <- cbind(df_running_mean_cv_rmspe, running_mean_cv_rmspe)
      
    }
    running_mean_cv_rmspe_list[[f]] <- running_mean_cv_rmspe
    
    if(f==1) {
      running_mean_cv_rmspe_df <- 
        data.frame(rmspe = running_mean_cv_rmspe,
                   variables = form_names[f],
                   response = i)
    } else {
      running_mean_cv_rmspe_df <- 
        rbind(running_mean_cv_rmspe_df, 
              data.frame(rmspe = running_mean_cv_rmspe,
                         variables=form_names[f],
                         response=i))
    }
    
  }
  names(df_cv_rmspe) <- gsub("\\.","_", make.names(names(form)))
  
  names(df_running_mean_cv_rmspe) <-  paste("form",c(1:length(form)),sep="_")
  
  #}
  #  dev.off()
  #  shell.exec(filenm)
  
  #Develop boxplot analysis of RMSE for each of the models for an individual organism
  # Used to choose model with least uncertainty in prediction
  # plot_df <- do.call(cbind,running_mean_cv_rmspe_list)
  # plot_df <- as.data.frame(plot_df)
  # names(plot_df) <- names(form)
  plot_df <- df_cv_rmspe
  
  plot_df <- tidyr::gather(plot_df)
  
  plot_df$key <- factor(plot_df$key,levels = form_names)
  
  rmspeboxplot <- ggplot(data=plot_df,aes(x=key,y=value)) + 
    geom_boxplot() + 
    ggtitle(paste0(i,":    Root Mean Square Prediction Error for ",n_replications," replications of each Model Option")) +
    theme(plot.title = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 45,hjust=1))
  
  names(plot_df) <- c("model_vars",i)
  if(i == response[1]) {
    rmse_df <- plot_df
  } else {
    rmse_df <- cbind(rmse_df,plot_df[,i])
    names(rmse_df)[ncol(rmse_df)] <- i
  }
  
  # Plot observed vs predicted models
  # Compile fitted vs observed in df with three columns
  # Fitted, observed, model description (response & predictors)
  colorOptions <- c("orange","red","skyblue","black","springgreen4","blue","grey","darkorchid1")
  names(colorOptions) <- legend_names <- sort(unique(model_df$abbrev))

  model_AICs <- numeric()
  model_results_df <- NULL
  for(f in 1:length(form)){
    m <- lm(form[[f]],data=model_df_scaled)
    predicted <- predict(m,newdata=model_df_scaled)
    observed <- model_df_scaled$log_response
    model_name <- paste(form_names[f],sep=" ~ ")
    model_results_df <- rbind(model_results_df,data.frame(predicted,observed,model_name))
    model_AICs <- c(model_AICs,AIC(m))
    names(model_AICs)[f] <- form[f]
  }
  names(model_results_df) <- c("predicted","observed","model_name")
  
  model_plot <- ggplot(data = model_results_df,aes(x = observed,y = predicted)) + 
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color="blue", 
                linetype="dashed", size=0.5) +
    facet_wrap(~ model_name)
  
  #model_plot  
  
  multi.page <- ggarrange(model_plot, rmspeboxplot,
                          nrow = 1, ncol = 1)
  
  filenm <- paste("GLRI_Oct_10_",i,".pdf",sep="")
  filenm <- file.path("model","out","plots",filenm)
  ggexport(multi.page, filename = filenm,width = 11,height = 8)
  
  df_cv_rmspe$response <- i
  
  
  #df_rmse_and_sites <- cbind(df_cv_rmspe,df_cv_sites)
  filenm <- file.path("model","out",paste("rmse_MMSD.rds",sep=""))
  saveRDS(rmse_df, file = filenm)
  
  filenm <- file.path("model","out",paste("rmse_and_sites_MMSD.rds",sep=""))
  saveRDS(cv_sites, file = filenm)
  
  
}


