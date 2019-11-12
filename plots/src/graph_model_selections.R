
## Visualize ##

graph_model_selections <- function(filenm) {
  
  GLRI_LMER <- readRDS(file.path("model","out","GLRI_LMER_model_rankings.rds"))
  GLRI_OLS <- readRDS(file.path("model","out","GLRI_OLS_model_rankings.rds"))
  MMSD <- readRDS(file.path("model","out","MMSD_LMER_model_rankings.rds"))
  
  model_plots <- list()
  #pdf(filenm)
  site_combo_choice <- "3-sites"
  df_plot <- filter(MMSD,site_combo == site_combo_choice) 
model_plots[[1]] <- ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
    #  scale_fill_brewer(palette="Paired")+
    ggtitle(site_combo_choice) +
    theme_minimal() +
    facet_wrap(~ response,nrow=3) +
    coord_flip()

  site_combo_choice <- "Agriculture"
  df_plot <- filter(GLRI_LMER,site_combo == site_combo_choice) 
  model_plots[[2]] <- ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
    #  scale_fill_brewer(palette="Paired")+
    ggtitle(site_combo_choice) +
    theme_minimal() +
    facet_wrap(~ response,nrow=3) +
    coord_flip()
  
  site_combo_choice <- "CL_RO"
  df_plot <- filter(GLRI_LMER,site_combo == site_combo_choice) 
  model_plots[[3]] <- ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
    #  scale_fill_brewer(palette="Paired")+
    ggtitle(site_combo_choice) +
    theme_minimal() +
    facet_wrap(~ response) +
    coord_flip()
  
  
  site_combo_choice <- "JI"
  df_plot <- filter(GLRI_OLS,site_combo == site_combo_choice) 
  model_plots[[3]] <- ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
    #  scale_fill_brewer(palette="Paired")+
    ggtitle(site_combo_choice) +
    theme_minimal() +
    facet_wrap(~ response) +
    coord_flip()
  
  # site_combo_choice <- "CL"
  # df_plot <- filter(GLRI_OLS,site_combo == site_combo_choice) 
  # ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  #   geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #   #  scale_fill_brewer(palette="Paired")+
  #   ggtitle(site_combo_choice) +
  #   theme_minimal() +
  #   facet_wrap(~ response) +
  #   coord_flip()
  # 
  # site_combo_choice <- "RO"
  # df_plot <- filter(GLRI_OLS,site_combo == site_combo_choice) 
  # ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  #   geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #   #  scale_fill_brewer(palette="Paired")+
  #   ggtitle(site_combo_choice) +
  #   theme_minimal() +
  #   facet_wrap(~ response) +
  #   coord_flip()
#  dev.off()
  

  #Final site combos = Agriculture, CL_RO, GLRI, 3-Site
return(model_plots)  
}
