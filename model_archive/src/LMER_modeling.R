

lmer_modeling <- function(df,model_formula){
  ## Now use model formula to run 50-fold x-val and compute NRMSE for this model instance
  
  # Set boundary tolerance for singularity consistent with "isSingular()"
  options(lmerControl(boundary.tol=1e-4))
  
  cv_sites <- data.frame(abbrev=character(),predictions = numeric(),log_response = numeric(),
                         model=character(),site_combo=character(),response=character(),replication = numeric())
  
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
          m <- lmer(form[[f]],data=df_cv)
          df_predict$predictions <- predict(m,newdata=df_predict)
          if(j == 1) {df_predictions <- df_predict
          }else{df_predictions <- rbind(df_predictions,df_predict)
          }
          
        }
        
        cv_rmspe <- c(cv_rmspe,rmspe(df_predictions$log_response,df_predictions$predictions))
        cv_sites_addition <- df_predictions[,c("abbrev","predictions","log_response")]
        cv_sites_addition$model <- form_names[f]
        cv_sites_addition$site_combo <- names(site_combos)[s]
        cv_sites_addition$response <- response[i]
        cv_sites_addition$replication <- r
        cv_sites <- rbind(cv_sites,cv_sites_addition)
        running_mean_cv_rmspe <- c(running_mean_cv_rmspe,mean(cv_rmspe))
        # plot_model_cv(df_predictions,form[[f]])
        
      }
      if(f==1) {
        df_cv_rmspe <- data.frame(form1 = cv_rmspe)
        #df_cv_sites <- data.frame(sites=cv_sites)
        df_running_mean_cv_rmspe <- data.frame(form1 = running_mean_cv_rmspe)
      }else{
        df_cv_rmspe <- cbind(df_cv_rmspe, cv_rmspe)
        #df_cv_sites <- cbind(df_cv_sites,cv_sites)
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
    #names(df_cv_sites) <- paste0(gsub("\\.","_", make.names(names(form))),"_cv")
    
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
      model_name <- paste(form_names[f],sep=" ~ ")
      model_results_df <- rbind(model_results_df,data.frame(predicted,observed,model_name,plot_Colors,abbrev))
      model_AICs <- c(model_AICs,AIC(m))
      names(model_AICs)[f] <- form[f]
    }
    names(model_results_df) <- c("predicted","observed","model_name","Plot_colors","abbrev")
    
    model_plot <- ggplot(data = model_results_df,aes(x = observed,y = predicted, color=abbrev)) + 
      geom_point() +
      #    geom_point(colour=model_results_df$Plot_colors) +
      scale_color_manual(values= colorOptions[unique(model_results_df$abbrev)]) +
      #scale_color_brewer(palette="Set2") +
      geom_abline(intercept = 0, slope = 1, color="blue", 
                  linetype="dashed", size=0.5) +
      facet_wrap(~ model_name)
    
    
    
    #model_plot  
    
    multi.page <- ggarrange(model_plot, rmspeboxplot,
                            nrow = 1, ncol = 1)
    
    filenm <- paste("MMSD_Oct_9_",names(site_combos)[s],"_",response[i],".pdf",sep="")
    filenm <- file.path("model","out","plots",filenm)
    ggexport(multi.page, filename = filenm,width = 11,height = 8)
    
    df_cv_rmspe$response <- response[i]
    
  }