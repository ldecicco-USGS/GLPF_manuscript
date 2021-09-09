# Evaluate accuracy of models by determining percent of residuals within X log of the observed value for each model

accuracy_list <- list()

accuracy_list[[1]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_Agriculture.rds") %>%
  mutate(scale = "Watershed",model = "Agricultural sensors",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[2]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_Agriculture_no_corr.rds") %>%
  mutate(scale = "Watershed",model = "Agricultural alternative",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[3]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_CL_RO.rds") %>%
  mutate(scale = "Watershed",model = "Urban sensors",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[4]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_CL_RO_no_corr.rds") %>%
  mutate(scale = "Watershed",model = "Urban alternative",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[5]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_JI.rds") %>%
  mutate(scale = "Watershed",model = "Milwaukee River sensors",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[6]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_JI_no_corr.rds") %>%
  mutate(scale = "Watershed",model = "Milwaukee River alternative",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[7]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_2-sites.rds") %>%
  mutate(scale = "Subwatershed",model = "Suburban sensors",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[8]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_2-sites_no_corr.rds") %>%
  mutate(scale = "Subwatershed",model = "Suburban alternative",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[9]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_3-sites.rds") %>%
  mutate(scale = "Subwatershed",model = "Subwatershed Urban sensors",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

accuracy_list[[10]] <- readRDS("model/out/Model_archive_cv_results/rmse_cv_sites_2021-03-05_3-sites_no_corr.rds") %>%
  mutate(scale = "Subwatershed",model = "Subwatershed Urban alternative",residual = log_response - predictions) %>%
  group_by(model,response) %>%
  summarise(
    Percent_within_0.25_log = mean(abs(residual) <= 0.25),
    Percent_within_0.5_log = mean(abs(residual) <= 0.5),
    Percent_within_0.75_log = mean(abs(residual) <= 0.75),
    Percent_within_1_log = mean(abs(residual)<=1),
    Percent_within_1.5_log = mean(abs(residual)<=1.5),
    Cross_validation_predictions = length(residual))

# Standardize response names:
response <- c("BACHUM.cn.100mls", "bacHum", "Lachno.2.cn.100ml", "lachno2", "E..coli.CFUs.100ml", "eColi", "Entero.CFUs.100ml","ent", "ENTERO.cn.100mls")
response_name <- c("Human Bacteroides", "Human Bacteroides","Lachnospiraceae","Lachnospiraceae","E. coli (culture)","E. coli", "Enterococci (culture)", "Enterococci", "Enterococci")

accuracy <- accuracy_list %>% reduce(rbind)  %>%
  left_join(df_response_names)

accuracy_long <- accuracy %>%
  select(-Cross_validation_predictions,-response) %>%
  pivot_longer(!c(model,response_name),names_to = "log_error",values_to = "percent") %>%
  mutate(log_error = (sub(pattern = "Percent_within_","",log_error)),
         log_error = as.numeric(sub(pattern = "_log","",log_error)))




df_response_names <- data.frame(response,response_name)

accuracy_by_model <- accuracy %>% 
  group_by(model) %>%
  summarise(mean_0.25 = mean(Percent_within_0.25_log),
            mean_0.5 = mean(Percent_within_0.5_log),
            mean_0.75 = mean(Percent_within_0.75_log),
            mean_1 = mean(Percent_within_1_log),
            mean_1.5 = mean(Percent_within_1.5_log))

accuracy_by_response <- accuracy  %>% 
  group_by(response_name) %>%
  summarise(mean_0.25 = mean(Percent_within_0.25_log),
            mean_0.5 = mean(Percent_within_0.5_log),
            mean_0.75 = mean(Percent_within_0.75_log),
            mean_1 = mean(Percent_within_1_log),
            mean_1.5 = mean(Percent_within_1.5_log))


df_model <- pivot_longer(accuracy_by_model,!model,names_to = "log_error", values_to = "percent")
df_model$log_error <- as.numeric(sub(pattern = "mean_","",df_model$log_error))

accuracy_long$log_error <- (sub(pattern = "Percent_within_","",accuracy_long$log_error))

#accuracy_long$log_error <- as.numeric(sub(pattern = "mean_","",df_model$log_error))

df_response <- pivot_longer(accuracy_by_response,!response_name,names_to = "log_error", values_to = "percent")
df_response$log_error <- as.numeric(sub(pattern = "mean_","",df_response$log_error))

ggplot(df_model, aes(x = log_error, y = percent*100)) + 
  geom_line(aes(color = model, linetype = model))

ggplot(df_response, aes(x = log_error, y = percent*100)) + 
  geom_line(aes(color = response_name, linetype = response_name))

error_plot <- ggplot(accuracy_long, aes(x = log_error, y = percent*100, group = response_name)) + 
  geom_line(aes(color = response_name, linetype = response_name)) +
  facet_wrap(vars(model), labeller = label_wrap_gen()) +
  ylab("Percent of Observations Error Less than the Given Log-10 Error") +
  xlab("Log-10 Error")

png(error_plot, file = "plots/out/Figure_S2_model_error.png",units = "in",width = 9,height = 5,res = 300)
print(error_plot)
dev.off()

write.csv(accuracy,file = "output/model_accuracy_table.csv")
