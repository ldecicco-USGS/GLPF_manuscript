# usage: OLS_modeling(df, parm_category, formulas, response, model_table, sites)


OLS_modeling <- function(df, parm_category, formulas, response, model_table, sites) {
  #JI modeling routine 
  #  i <- 1 #remove when function is complete
  f <- 1 #keep: formula num
  response_header_name <- names(response)
  groupings <- "abbrev"
  interactors <- c("sinDate", "cosDate")
  
  cv_sites <- data.frame(abbrev=character(),predictions = numeric(),log_response = numeric(),
                         model=character(),site_combo=character(),response=character(),replication = numeric())
  running_mean_cv_rmspe_list <- list()
  
  df_cv_rmspe_all <- data.frame(rmspe = numeric(),variables=character(),response=character(),type =character())
  
  for(i in 1:length(response)){
    #   * transform response variable
    df$log_response <- log10(df[,response[i]])
    
    model_row <- which(model_table$Sites == sites & model_table$`Parameter Category` == parm_category)
    form_name <- as.character(as.data.frame(model_table)[model_row,response_header_name[i]])
    form_name;response[i]
    
    form <- formulas[[form_name]]
    
    if(parm_category == "sensors"){ # Define predictors and interaction terms
      predictors<- c("Turbidity_mean", "T", "F","M")
    } else {
      
      
      Initial_predictors<- c("Turbidity_mean", "T", "F","M")
      
      df_cor <- correlated_to_primary_signals(Initial_predictors,filenm="glri_summary.rds")
      sensors <- reduce_correlated_variables(df_cor,filenm="glri_summary.rds",Initial_predictors)
      sensors <- sensors[-grep("rSag",sensors)]
      sensors <- sensors[-grep("A_",sensors)]
      sensors <- sensors[-grep("LA",sensors)] #remove LA signal--high error in models distorts graphics
      sensors <- sensors[-grep("H2",sensors)] #remove H2 signal--high error in models distorts graphics
      
      predictors <- c(sensors,Initial_predictors)
    }
    
    #   * Filter data to sites and make model df
    model_columns <- c("log_response", response[i], predictors,interactors,groupings)
    
    model_df <- df %>%
      select(model_columns) %>%
      na.exclude()
    
    
    # develop dataframe of model variables
    #   -start with predictors
    x <- as.data.frame(scale(model_df[,predictors]))
    names(x) <- predictors
    
    #   -add in response, interactors, and groups
    model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,c(interactors, groupings)])
    names(model_df_scaled) <- c("log_response",predictors,interactors,groupings)
    
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
        m <- lm(form,data=df_cv)
        df_predict$predictions <- predict(m,newdata=df_predict)
        if(j == 1) {df_predictions <- df_predict
        }else{df_predictions <- rbind(df_predictions,df_predict)
        }
        
      }
      
      cv_rmspe <- c(cv_rmspe,rmspe(df_predictions$log_response,df_predictions$predictions))
      cv_sites_addition <- df_predictions[,c("abbrev","predictions","log_response")]
      cv_sites_addition$model <- form_name
      cv_sites_addition$site_combo <- sites
      cv_sites_addition$response <- response[i]
      cv_sites_addition$replication <- r
      cv_sites <- rbind(cv_sites,cv_sites_addition)
      running_mean_cv_rmspe <- c(running_mean_cv_rmspe,mean(cv_rmspe))
      # plot_model_cv(df_predictions,form[[f]])
      
    }
    df_cv_rmspe <- data.frame(form1 = cv_rmspe/sd(df_cv$log_response,na.rm = TRUE))
    df_cv_rmspe_all <- bind_rows(df_cv_rmspe_all,
                                          data.frame(rmspe = df_cv_rmspe$form1,variables=form_name,response=response[i],type = parm_category))
    
    #df_cv_sites <- data.frame(sites=cv_sites)
    df_running_mean_cv_rmspe <- data.frame(form1 = running_mean_cv_rmspe)
    
    running_mean_cv_rmspe_list[[i]] <- running_mean_cv_rmspe
    

  }

  median_rmspe <- df_cv_rmspe_all %>%
    group_by(response,variables, type) %>%
    summarise(rmspe_median = median(rmspe))
  
  return(median_rmspe)
  
}
