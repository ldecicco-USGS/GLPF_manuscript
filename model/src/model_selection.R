#Function to help choose final regression models based on cross validation for 
#multiple models with consideration of prediction accuracy at individual sites

model_selection <- function(df) {
  
  
  # 1. Determine overall model rmspe stats
  # 2. filter out models with median RMSPE > deviation threshold (5%?)
  # 3. Determine rmspe stats for each model for individual sites
  # 4. Reduce models by considering deviation threshold from min RMSPE for individual sites
  deviation_threshold <- 0.03
  max_deviation_threshold <- 0.03
  
  # Step 1
  df_rmspe <- df %>%
    group_by(site_combo,response,model,replication) %>%
    summarise(rmse = rmspe(log_response,predictions)) %>%
    group_by(site_combo,response,model) %>%
    summarise(rmse_median = median(rmse),
              rmse_max = max(rmse),
              rmse_90 = quantile(rmse,0.9))
  
  median_deviation <- df_rmspe %>%
    group_by(site_combo,response) %>%
    mutate(ranks = rank(rmse_median),
           rmse_deviation = (rmse_median -min(rmse_median))/min(rmse_median)) %>%
    filter(rmse_deviation < deviation_threshold)
  
  priority_rows <- which(paste0(df$site_combo,df$response,df$model) %in%
                           paste0(median_deviation$site_combo,median_deviation$response,median_deviation$model))
  
  df_priority_models <- df[priority_rows,]
  
  
  df_rmspe_priority_models <- df_priority_models %>%
    group_by(site_combo,response,model,replication,abbrev) %>%
    summarise(rmse = rmspe(log_response,predictions)) %>%
    group_by(site_combo,response,model,abbrev) %>%
    summarise(rmse_median = median(rmse),
              rmse_max = max(rmse),
              rmse_90 = quantile(rmse,0.9))
  
  
  #Set criteria for choosing final model using individual sites
  
  #1. Determine minimum and maximum individual site "median_rmse" values
  #2. Narrow down sites to those within an individual site RSMPE deviance from min
  ind_dev_threshold <- 0.1
  final_models <- df_rmspe_priority_models %>%
    group_by(site_combo,response) %>%
    mutate(deviation_from_min = (rmse_median -min(rmse_median))/min(rmse_median)) %>%
    group_by(site_combo,response,model) %>%
    summarise(max_site_rmse = max(rmse_median),
              min_site_rmse = min(rmse_median)) %>%
    group_by(site_combo,response) %>%
    mutate(site_deviation = (max_site_rmse-min_site_rmse)/min_site_rmse) %>%
    # filter(site_deviation == min(site_deviation)) %>%
    mutate(rank = rank(max_site_rmse))###  CHANGE FROM max AS THE METRIC TO some combination of min and max
  
  return(final_models)
}
