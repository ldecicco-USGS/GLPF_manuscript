# Hueristic Optical-bacteria regressions for GLRI Jones Island site

# 1. Load data
# 2. General modeling setup:
#   * Define response variables
#     * Transform response variables
#   * Define predictors
#     * Define interactions

#   * Construct formula
#   * Filter data to Jones Island site and make model df

# 3. Run OLS model for all response variables
# 4. Graph results


library(smwrBase)
library(car)
library(dplyr)
library(cvTools)
library(ggplot2)
library(ggpubr)


source(file.path("model","src","plot_model_cv.R"))

# 1. Load data
df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI

# 2. General modeling setup:

#  * Define response variables
response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")
#response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls")

#Set censored values to detection limit
MDL <- c(225,225,1,225,1)
names(MDL) <- response
for(i in 1:length(response)){df[,response[i]] <- ifelse(df[,response[i]]<=MDL[response[i]],MDL[response[i]],df[,response[i]])}

# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
#predictors<- c("Turbidity_mean", "T", "F","M","Aresid267","S1.25","rF_T","A254")
predictors<- c("Turbidity_mean", "T", "F","M")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")

site_combos <- list()
site_combos[[1]] <- c("JI")
site_combos[[1]] <- c("CL")
site_combos[[2]] <- c("RO")

names(site_combos) <- c("JI")
names(site_combos) <- c("CL","RO")

form_names <- c("F","F2","F,T","F,Turb","F,S1","F,A254","F,Aresid","T","T2","T,Turb",
                "Turb","Turb2","F,T,Turb", "F,T,Turb 2","F,T,Turb 3",
                "F,Turb 2","F,T 2","F,Aresid 2","Aresid","Aresid2","T,Turb 2")
# form <- list()
# form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate ")
# form[[2]] <- formula("log_response ~ F * cosDate + F * sinDate ")
# form[[3]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate ")
# form[[4]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate ")
# form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + S1.25 * cosDate + S1.25 * sinDate  ")
# form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + A254 * cosDate + A254 * sinDate  ")
# form[[7]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate  ")
# form[[8]] <- formula("log_response ~ T * cosDate + T * sinDate  ")
# form[[9]] <- formula("log_response ~ T * cosDate + T * sinDate ")
# form[[10]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate ")
# form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate  ")
# form[[12]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate ")
# form[[13]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate ")
# form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate ")
# form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate ")
# form[[16]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate ")
# form[[17]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate ")
# form[[18]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate ")
# form[[19]] <- formula("log_response ~ Aresid267 * cosDate + Aresid267 * sinDate  ")
# form[[20]] <- formula("log_response ~ Aresid267 * cosDate + Aresid267 * sinDate  ")
# form[[21]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate ")


# form <- list()
# form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate")
# form[[2]] <- formula("log_response ~ T * cosDate + T * sinDate + sinDate + cosDate ")
# form[[3]] <- formula("log_response ~ M * cosDate + M * sinDate + sinDate + cosDate")
# form[[4]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate")
# form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
# form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
# form[[7]] <- formula("log_response ~ T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
# form[[8]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + sinDate + cosDate")
# form[[9]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate")
# form[[10]] <- formula("log_response ~ Turbidity_mean + M * cosDate + M * sinDate + sinDate + cosDate")
# form[[11]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
# form[[12]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate")
# form[[13]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
# form[[14]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate")
# form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate")
# form[[16]] <- formula("log_response ~ T * cosDate + T * sinDate + sinDate + cosDate")
# form[[17]] <- formula("log_response ~ M * cosDate + M * sinDate + sinDate + cosDate")
# form[[18]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate")
# form[[19]] <- formula("log_response ~ F + F * cosDate + F * sinDate + sinDate + cosDate")
# 
# form_names <- c("F","T","M","Turb","F_T","F_M","T_M","Turb_F","Turb_T","Turb_M","F_T_M",
#                 "Turb_F_T","Turb_F_M","Turb_T_M",
#                 "F2","T2","M2","Turb2","F3")



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

names(form) <- form_names[1:length(form)]
# # 3. Run OLS model for all response variables


for (s in 1:(length(site_combos))) {

    #   * Choose sites or states to be included
  sites <- site_combos[[s]]
  
  for (i in 1:length(response)) {
    

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
          m <- lm(form[[f]],data=df_cv)
          df_predict$predictions <- predict(m,newdata=df_predict)
          if(j == 1) {df_predictions <- df_predict
          }else{df_predictions <- rbind(df_predictions,df_predict)
          }
          
        }
        
        cv_rmspe <- c(cv_rmspe,rmspe(df_predictions$log_response,df_predictions$predictions))
        running_mean_cv_rmspe <- c(running_mean_cv_rmspe,mean(cv_rmspe))
        # plot_model_cv(df_predictions,form[[f]])
        
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
    names(df_cv_rmspe) <- gsub("\\.","_", make.names(names(form)))
    
    names(df_running_mean_cv_rmspe) <-  paste("form",c(1:length(form)),sep="_")
    

    #Develop boxplot analysis of RMSE for each of the models for an individual organism
    # Used to choose model with least uncertainty in prediction
    # plot_df <- do.call(cbind,running_mean_cv_rmspe_list)
    # plot_df <- as.data.frame(plot_df)
    # names(plot_df) <- names(form)
    plot_df <- df_cv_rmspe
    
    plot_df <- tidyr::gather(plot_df)
    rmspeboxplot <- ggplot(data=plot_df,aes(x=key,y=value)) + 
      geom_boxplot() + 
      ggtitle(paste0(response[i],":    Root Mean Square Prediction Error for ",n_replications," replications of each Model Option")) +
      theme(plot.title = element_text(size = 12),axis.text.x=element_text(angle=45,hjust=1))
   
      
    
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
      m <- lm(form[[f]],data=model_df_scaled)
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
    
    filenm <- paste("GLRI_model_options__Sep_27_",names(site_combos)[s],"_",response[i],".pdf",sep="")
    filenm <- file.path("model","out","plots",filenm)
    ggexport(multi.page, filename = filenm,width = 11,height = 8)
    
  }
  filenm <- file.path("model","out",paste("rmse_Sep_27",names(site_combos)[s],".rds",sep=""))
  saveRDS(rmse_df, file = filenm)  
}


