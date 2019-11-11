
library(tidyverse)
source(file.path("model","src","rmse_model_selection.R"))


#### GLRI ###
#Evaluate GLRI LMER models:

site_combos <- list()
site_combos[[1]] <- c("CL", "RO")
site_combos[[2]] <- c("PO", "MA", "RM")
names(site_combos) <- c("CL_RO","Agriculture")
sites <- names(site_combos)[2]

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_9_",sites,".rds",sep=""))
df <- readRDS(file=filenm)
df <- df[-grep("M",df$model),]
                      
GLRI_LMER_selection <- rmse_model_selection(df)

#non-correlated extra variables
filenm <- file.path("model","out","rmse_and_sites_Oct_9_no_corrAgriculture.rds",sep="")
df <- readRDS(file=filenm)

GLRI_LMER_non_cor_selection <- rmse_model_selection(df)

GLRI_LMER_selection$model_run <- "sensors"
GLRI_LMER_non_cor_selection$model_run <- "non-cor"
GLRI_LMER <- full_join(GLRI_LMER_selection,GLRI_LMER_non_cor_selection)
GLRI_LMER$model <- factor(GLRI_LMER$model,levels = unique(c(GLRI_LMER_selection$model,GLRI_LMER_non_cor_selection$model)))


#Evaluate GLRI single site OLS models

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_10_","RO",".rds",sep=""))
df <- readRDS(file=filenm)
df <- df[-grep("M",df$model),]

GLRI_OLS_selection <- rmse_model_selection(df)


#non-correlated extra variables for single sites

filenm <- file.path("model","out",paste("rmse_and_sites_Oct_10_no_corr","RO",".rds",sep=""))
df <- readRDS(file=filenm)
df <- df[-grep("M",df$model),]

GLRI_OLS_non_cor_selection <- rmse_model_selection(df)

GLRI_OLS_selection$model_run <- "sensors"
GLRI_OLS_non_cor_selection$model_run <- "non-cor"
GLRI_OLS <- full_join(GLRI_OLS_selection,GLRI_OLS_non_cor_selection)
GLRI_OLS$model <- factor(GLRI_OLS$model,levels = unique(c(GLRI_OLS_selection$model,GLRI_OLS_non_cor_selection$model)))


### MMSD ###

#LMER

filenm <- file.path("model","out","rmse_and_sites_Oct_9_3-sites.rds",sep="")
df <- readRDS(file=filenm)
df <- df[-grep("M",df$model),]

MMSD_selection <- rmse_model_selection(df)


#non-correlated extra variables

filenm <- file.path("model","out","rmse_and_sites_Oct_11_no_corr3-sites.rds")
df <- readRDS(file=filenm)

MMSD_non_cor_selection <- rmse_model_selection(df)

MMSD_selection$model_run <- "sensors"
MMSD_non_cor_selection$model_run <- "non-cor"

MMSD <- full_join(MMSD_selection,MMSD_non_cor_selection)
MMSD$model <- factor(MMSD$model,levels = unique(c(MMSD_selection$model,MMSD_non_cor_selection$model)))


## Visualize ##

unique(MMSD$model)


p <- ggplot(data=MMSD, aes(x=response, y=rmse_median,fill=model_run)) +
  geom_bar(stat="min", color="black", position=position_dodge())+
  theme_minimal()
# Use brewer color palettes
p <- p + scale_fill_brewer(palette="Blues")
p



ggplot(data=MMSD, aes(x=response, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  scale_fill_brewer(palette="Paired")+
  ggtitle("MMSD")) +
  theme_minimal()

site_combo_choice <- "3-sites"
df_plot <- filter(MMSD,site_combo == site_combo_choice) 
ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #  scale_fill_brewer(palette="Paired")+
  ggtitle(site_combo_choice) +
  theme_minimal() +
  facet_wrap(~ response)

site_combo_choice <- "Agriculture"
df_plot <- filter(GLRI_LMER,site_combo == site_combo_choice) 
ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
#  scale_fill_brewer(palette="Paired")+
  ggtitle(site_combo_choice) +
  theme_minimal() +
  facet_wrap(~ response)

site_combo_choice <- "CL_RO"
df_plot <- filter(GLRI_LMER,site_combo == site_combo_choice) 
ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #  scale_fill_brewer(palette="Paired")+
  ggtitle(site_combo_choice) +
  theme_minimal() +
  facet_wrap(~ response)


site_combo_choice <- "JI"
df_plot <- filter(GLRI_OLS,site_combo == site_combo_choice) 
ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #  scale_fill_brewer(palette="Paired")+
  ggtitle(site_combo_choice) +
  theme_minimal() +
  facet_wrap(~ response)

site_combo_choice <- "CL"
df_plot <- filter(GLRI_OLS,site_combo == site_combo_choice) 
ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #  scale_fill_brewer(palette="Paired")+
  ggtitle(site_combo_choice) +
  theme_minimal() +
  facet_wrap(~ response)

site_combo_choice <- "RO"
df_plot <- filter(GLRI_OLS,site_combo == site_combo_choice) 
ggplot(data=df_plot, aes(x=model, y=rmse_median, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+
  #  scale_fill_brewer(palette="Paired")+
  ggtitle(site_combo_choice) +
  theme_minimal() +
  facet_wrap(~ response)

# Rind min RMSE for "sensors" options and "non-cor" options and construct table

#Final site combos = Agriculture, CL_RO, GLRI, 3-Site

GLRI_LMER -> group_by(site_combo, response) %>%
  
