# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R
#

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
df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI

# 2. General modeling setup:

#  * Define response variables
# response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")
response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls")

#Set censored values to detection limit
MDL <- c(225,225,1,225,1)
names(MDL) <- response
for(i in 1:length(response)){df[,response[i]] <- ifelse(df[,response[i]]<=MDL[response[i]],MDL[response[i]],df[,response[i]])}

# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","M")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")

site_combos <- list()
site_combos[[1]] <- c("CL", "RO")
# site_combos[[2]] <- c("PO", "MA", "RM")
# site_combos[[3]] <- c("JI","OC")
#site_combos[[1]] <- c("PO", "MA", "RM","OC","EE")
#site_combos[[1]] <- c("OC","EE")
# site_combos[[3]] <- c("JI","CL", "RO")
# site_combos[[4]] <- c("PO", "MA", "RM","JI")
# site_combos[[5]] <- c("JI","PO", "MA", "CL", "RO", "RM")
# site_combos[[6]] <- c("PO", "MA", "CL", "RO", "RM")

names(site_combos) <- c("CL_RO","Agriculture","South_WI")


#names(site_combos) <- c("All","no_JI","urban","CL_RO","AG_JI","Agricultural")

# form_names <- c("F","F2","F,T","F,Turb","F,S1","F,A254","F,Aresid","T","T2","T,Turb",
#                 "Turb","Turb2","F,T,Turb", "F,T,Turb 2","F,T,Turb 3",
#                 "F,Turb 2","F,T 2","F,Aresid 2","Aresid","Aresid2","T,Turb 2")
# form <- list()
# form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (F + 1 | abbrev)")
# form[[2]] <- formula("log_response ~ F * cosDate + F * sinDate + (1 | abbrev)")
# form[[3]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate + (F + 1 | abbrev)")
# form[[4]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate + (F + 1 | abbrev)")
# form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + S1.25 * cosDate + S1.25 * sinDate  + (S1.25 | abbrev)")
# form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + A254 * cosDate + A254 * sinDate  + (F | abbrev)")
# form[[7]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate  + (Aresid267 | abbrev)")
# form[[8]] <- formula("log_response ~ T * cosDate + T * sinDate  + (T + 1 | abbrev)")
# form[[9]] <- formula("log_response ~ T * cosDate + T * sinDate + (1 | abbrev)")
# form[[10]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate + (Turbidity_mean + 1 | abbrev)")
# form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate  + (Turbidity_mean | abbrev)")
# form[[12]] <- formula("log_response ~ Turbidity_m * cosDate + Turbidity_mean * sinDate + (1 | abbrev)")
# form[[13]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (F + Turbidity_mean | abbrev)")
# form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (F | abbrev)")
# form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (1 | abbrev)")
# form[[16]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate + (1 | abbrev)")
# form[[17]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate + (1 | abbrev)")
# form[[18]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate  + (1 | abbrev)")
# form[[19]] <- formula("log_response ~ Aresid267 * cosDate + Aresid267 * sinDate  + (Aresid267 | abbrev)")
# form[[20]] <- formula("log_response ~ Aresid267 * cosDate + Aresid267 * sinDate  + (1 | abbrev)")
# form[[21]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate + (1 | abbrev)")

# Current sensor variables tried: F, T, S1, Turb. ** S1 was not useful beyond the others
# Easily developed: M
# Current sensors highly correlated with other current sensors: S2 (F), S3 (T), OB (F): r > 0.98
# 
# form <- list()
# form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[2]] <- formula("log_response ~ T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[3]] <- formula("log_response ~ M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[4]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[7]] <- formula("log_response ~ T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[8]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[9]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[10]] <- formula("log_response ~ Turbidity_mean + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[11]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (Turbidity_mean + 1 | abbrev)")
# form[[12]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[13]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[14]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
# form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (F + 1 | abbrev)")
# form[[16]] <- formula("log_response ~ T * cosDate + T * sinDate + sinDate + cosDate + (T + 1 | abbrev)")
# form[[17]] <- formula("log_response ~ M * cosDate + M * sinDate + sinDate + cosDate + (M + 1 | abbrev)")
# form[[18]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate + (Turbidity_mean + 1 | abbrev)")
# form[[19]] <- formula("log_response ~ F + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
# 
# form_names <- c("F","T","M","Turb","F_T","F_M","T_M","Turb_F","Turb_T","Turb_M","F_T_M",
#                 "Turb_F_T","Turb_F_M","Turb_T_M",
#                 "F2","T2","M2","Turb2","F3")


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

#df_cor <- correlated_to_primary_signals()
#variables <- reduce_correlated_variables(df_cor)
sensors <- c("F","T","M")

turb <- "Turbidity_mean"

names(form) <- form_names[1:length(form)]
# # 3. Run LME model for all response variables

# Set boundary tolerance for singularity consistent with "isSingular()"
options(lmerControl(boundary.tol=1e-4))

cv_sites <- data.frame(abbrev=character(),predictions = numeric(),log_response = numeric(),
                       model=character(),site_combo=character(),response=character())

for (s in 1:2) {  #Solo JI doesn't need lmer, but just lm
  #   * Choose sites or states to be included
  sites <- site_combos[[s]]
  for (i in 1:length(response)) {
    
    #filenm <- paste("GLRI_predictions_",response[i],".pdf",sep="")
    #  pdf(filenm)
    
    #   * transform response variable
    df$log_response <- log10(df[,response[i]])
    
    #   * Filter data to sites and make model df
    model_rows <- which(df[,groupings] %in% sites)
    model_columns <- c("log_response", response,predictors,interactors,groupings)
    model_df <- df[model_rows,model_columns]
    model_df <- na.exclude(model_df)
    
    # develop dataframe of model variables
    #   -start with predictors
    x <- as.data.frame(scale(model_df[,predictors]))
    names(x) <- predictors
    
    #   -add in response, interactors, and groups
    model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,c(interactors, groupings)])
    names(model_df_scaled) <- c("log_response",predictors,interactors,groupings)
    
    running_mean_cv_rmspe_list <- list()
    for(f in 1:length(form)){
        n_folds <- 5
      n_replications <- 5
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
        
        cv_rmspe <- c(cv_rmspe,rmspe(df_predictions$log_response,df_predictions$predictions))
        cv_sites_addition <- df_predictions[,c("abbrev","predictions","log_response")]
        cv_sites_addition$model <- form_names[f]
        cv_sites_addition$site_combo <- site_combos[s]
        cv_sites_addition$response <- response[i]
        cv_sites <- rbind(cv_sites,cv_sites_addition)
        running_mean_cv_rmspe <- c(running_mean_cv_rmspe,mean(cv_rmspe))
        # plot_model_cv(df_predictions,form[[f]])
        
      }
      if(f==1) {
        df_cv_rmspe <- data.frame(form1 = cv_rmspe)
        df_cv_sites <- data.frame(sites=cv_sites)
        df_running_mean_cv_rmspe <- data.frame(form1 = running_mean_cv_rmspe)
      }else{
        df_cv_rmspe <- cbind(df_cv_rmspe, cv_rmspe)
        df_cv_sites <- cbind(df_cv_sites,cv_sites)
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
    names(df_cv_rmspe) <- gsub("\\.","_", make.names(names(form)))
    names(df_cv_sites) <- paste0(gsub("\\.","_", make.names(names(form))),"_cv")
    
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
      ggtitle(paste0(response[i],":    Root Mean Square Prediction Error for ",n_replications," replications of each Model Option")) +
      theme(plot.title = element_text(size = 12)) +
      theme(axis.text.x = element_text(angle = 45,hjust=1))
    
    names(plot_df) <- c("model_vars",response[i])
    if(i == 1) {rmse_df <- plot_df
    }else{rmse_df <- cbind(rmse_df,plot_df[,response[i]])
    names(rmse_df)[(i+1)] <- response[i]
    }
    
    # Plot observed vs predicted models
    # Compile fitted vs observed in df with three columns
    # Fitted, observed, model description (response & predictors)
    colorOptions <- c("orange","red","skyblue","black","springgreen4","blue","grey","darkorchid1")
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
    
    model_plot <- ggplot(data = model_results_df,aes(x = observed,y = predicted, color=abbrev)) + 
      geom_point() +
      #    geom_point(colour=model_results_df$Plot_colors) +
      scale_color_manual(values= colorOptions[levels(model_results_df$abbrev)]) +
      #scale_color_brewer(palette="Set2") +
      geom_abline(intercept = 0, slope = 1, color="blue", 
                  linetype="dashed", size=0.5) +
      facet_wrap(~ model_name)
    
    
    
    #model_plot  
    
    multi.page <- ggarrange(model_plot, rmspeboxplot,
                            nrow = 1, ncol = 1)
    
    filenm <- paste("GLRI_Oct_3_",names(site_combos)[s],"_",response[i],".pdf",sep="")
    filenm <- file.path("model","out","plots",filenm)
    ggexport(multi.page, filename = filenm,width = 11,height = 8)
    
    df_cv_rmspe$response <- response[i]
    
  }
  df_rmse_and_sites <- cbind(df_cv_rmspe,df_cv_sites)
  filenm <- file.path("model","out",paste("rmse_Oct_3_",names(site_combos)[s],".rds",sep=""))
  saveRDS(rmse_df, file = filenm)
  
  filenm <- file.path("model","out",paste("rmse_and_sites_Oct_3_",names(site_combos)[s],".rds",sep=""))
  saveRDS(df_rmse_and_sites, file = filenm)
  
  
}


