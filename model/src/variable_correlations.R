#Test correlation of variables with the common sensors + M
library(dplyr)
library(Hmisc)


reduce_correlated_variables <- function(df_cor,filenm,predictors) {
  # 1. Load data
  df <- readRDS(file.path("process","out",filenm))
  r_thresh <- 0.95
  
  #Determine which variables are highly correlated and remove to get less correlated variables for regressions
  
#  predictors<- c("Turbidity_mean", "T", "F","M") #sensor variables
  
  # Remove varables that are highly correlated to sensor variables 
  df_cor2 <- df_cor[-which(abs(df_cor$Correlations) > r_thresh),]
  df_cor_vars <- as.data.frame(table(df_cor2$signal),stringsAsFactors = FALSE)
  df_cor_vars <- filter(df_cor_vars,Freq == length(predictors))
  
  # Now test whether the remaing variables are highly correlated to each other and reduce
  
  df_corr_test <- df[,df_cor_vars$Var1]
  variables_orig <- names(df_corr_test)
  
  variables <- variables_orig
  variables <- variables[-grep("Mrange.50",variables)]
  
  

  i <- 1
  keep <- variables[1]
  continue <- TRUE
  while(continue == TRUE) {
    test_vars <- character()
    r <- numeric()
    for(j in (i+1):length(variables)){
      mat <- cbind(df[,variables[i]],df[,variables[j]])
      mat <- mat[is.finite(rowSums(mat)),]
      r <- c(r,cor(mat[,1],mat[,2]))
      test_vars <- c(test_vars,variables[j])
    }
    names(r) <- test_vars
    remove_vars <- names(r)[which(r > r_thresh)]
    if(length(remove_vars) > 0)  variables <- variables[-which(variables %in% remove_vars)]
    
    
    #   unique_variables <- c(variables[i])
    # variables2 <- variables(-which(variables %in% remove_variables))
    i <- i+1
    continue <- ifelse(i < length(variables),TRUE,FALSE)
    next()
  }
  
  variables <- variables[-grep("log|Turbidity",variables)]
  
  return(variables)
}


correlated_to_primary_signals <- function(predictors,filenm) {
  # 1. Load data
  df <- readRDS(file.path("process","out",filenm))
  
  if(filenm == "glri_summary.rds") {
  All_signals <- names(df)[258:dim(df)[2]]
  }else {
    if(filenm == "mmsd_summary.rds") {
      All_signals <- names(df)[28:dim(df)[2]]
    }
  }
  
  signals <- All_signals[-which(All_signals %in% predictors)]
  
  Correlations <- numeric()
  predictor <- character()
  signal <- character()
  
  for(j in 1:length(predictors)){
    for(i in 1:length(signals)){
      df_temp <- na.omit(df[,c(predictors[j],signals[i])])
      Correlations <- c(Correlations,cor(df_temp[,predictors[j]],df_temp[,signals[i]]))
      predictor <- c(predictor,predictors[j])
      signal <- c(signal,signals[i])
    }
  }
  
  df_cor <- data.frame(predictor,signal,Correlations,stringsAsFactors = FALSE)
  
  return(df_cor)
  
}
