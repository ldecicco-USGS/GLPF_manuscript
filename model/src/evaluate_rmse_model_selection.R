
library(dplyr)

#Read data

site_combos <- list()
site_combos[[1]] <- c("CL", "RO")
site_combos[[2]] <- c("PO", "MA", "RM")
site_combos[[3]] <- c("JI","OC")


names(site_combos) <- c("CL_RO","Agriculture","South_WI")

s <- 1
# filenm <- file.path("model","out",paste("rmse_Oct_3_",names(site_combos)[s],".rds",sep=""))
# df_rmse <- readRDS(file = filenm)

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_3_",names(site_combos)[s],".rds",sep=""))
df <- readRDS(file=filenm)


# num_models <- grep("_cv",names(df_rmse))[1]-2
# 
# df <- data.frame(site=character(),rmse=numeric(),model=character())
# for(i in 1:num_models) {
#   df_temp <- df_rmse[,c((num_models+1),i,(i+num_models+1))]
#   df_temp$model <- names(df_rmse)[i]
#   names(df_temp) <- c("response","rmse","site","model")
#   df <- rbind(df,df_temp)
# }  


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
ind_dev_threshold <- 0.03
final_models <- df_rmspe_priority_models %>%
  group_by(site_combo,response) %>%
  mutate(deviation_from_min = (rmse_median -min(rmse_median))/min(rmse_median)) %>%
  group_by(site_combo,response,model) %>%
  summarise(max_site_deviation = max(rmse_median)) %>%
  filter((max_site_deviation-min(max_site_deviation))/min(max_site_deviation) < ind_dev_threshold)

  
