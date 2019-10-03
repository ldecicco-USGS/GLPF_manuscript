
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
df_rmse <- readRDS(file=filenm)

df_rmse2 <- df_rmse
df_rmse2$response <- "response2"

df_rmse <- rbind(df_rmse,df_rmse2)

num_models <- grep("_cv",names(df_rmse))[1]-2

df <- data.frame(site=character(),rmse=numeric(),model=character())
for(i in 1:num_models) {
  df_temp <- df_rmse[,c((num_models+1),i,(i+num_models+1))]
  df_temp$model <- names(df_rmse)[i]
  names(df_temp) <- c("response","rmse","site","model")
  df <- rbind(df,df_temp)
}  


# 1. rank by median
# 2. From the top X models by overall median rank, eliminate models with large outliers
# 3. From the top X models by overall median rank, choose the model with the lowest maximum site median

num_priority_models <- 5

df_cv_medians <- df %>%
  group_by(model,response) %>%
  summarise(rmse_median = median(rmse),
            rmse_max = max(rmse),
            rmse_90 = quantile(rmse,0.9))

full_ranks <- df_cv_medians %>%
  group_by(response) %>%
  mutate(ranks = rank(rmse_median))


priority_models <- df_cv_medians[which(full_ranks$ranks <= num_priority_models),]




df_cv_stats <- df %>%
  filter(model %in% priority_models$model) %>%
  group_by(response,model,site) %>%
  summarise(rmse_median = median(rmse),
            rmse_max = max(rmse),
            rmse_90 = quantile(rmse,0.95)) %>%
  group_by(response,model) %>%
  summarise(max_median_site_rmse = max(rmse_median),
            max_max_site_rmse = max(rmse_max))

final_model <- df_cv_stats$model[which.min(df_cv_stats$max_max_site_rmse)]


