###
#Outline of workflow for the three different spatial scales
#
#1. Data processing: 
#  a. MRL determination for optical data
#      -Question: can we determine a common MRL for all data sets or separate?
#    i.   Locate blanks for each data set
#    ii.  Compute MRLs
#    iii. Apply MRLs to each raw data set
#   
#  b. Compute summary optical data parameters
#    i.   Develop common set of parameters needed
#       - determine frequency of censored values for each parameter for each data set
#       - decide which parameters to keep 
#       - modify Optical summary definitions
#    ii.  Use HydroOpt routines to compute 
#    iii. Result: summary optical data sets
#  c. Combine summary optical data sets with bacteria data
#    i.   Use GR numbers form CA lab and FT numbers from UWM lab. These are 
#         all joined already from previous data tasks
#    ii.  Use optical parameters from the final GLPF data set to define
#         optical parameters for all three spatial scales
#
#2. Data description
#  a. Generate plot (figure 2) with concentration and occurrence of HB
#    i. Done: script = Figure 2.R, Results.Rmd
#  b. Determine numbers of samples and such for adding to text
#    ii. Began this task: script = Results.Rmd
#
#3. Modeling
#  a. Large watersheds
#    i.   Begin with LME modeling of common parameters
#    ii.  Choose groups of sites where this works
#    iii. Explore additional parameters for watersheds where that doesn't work
#    iv.  Develop summary table of models
#  b. Subwatersheds
#    i.  LME modeling with common parameters
#    ii. Include in modeling table with Large watersheds
#  c. Small 
#



##########################################
# Project Setup
##########################################

# Packages in all scripts included here

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(rmarkdown)
library(servr)
library(USGSHydroOpt)
library(RColorBrewer)
library(scales)
library(USGSHydroTools)

dir.create("process", showWarnings = FALSE)
dir.create(file.path("process","out"), showWarnings = FALSE)
dir.create("probably_junk", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)
dir.create(file.path("plots","out"), showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)
dir.create(file.path("report","individual_reports","out"), showWarnings = FALSE)
dir.create("model", showWarnings = FALSE)
dir.create(file.path("model","out"), showWarnings = FALSE)
dir.create("cache", showWarnings = FALSE)


##########################################
# Fetch
##########################################


##########################################
# Process
##########################################

#Combine MMSD P3 and P4 vectorized abs and fl data
# source(file = file.path("process","src","merge_fl_abs_mmsd_p3_p4_vectorized.R"))
# 
# 
#Generate a combined dataframe that has human markers and a few basic optical signals
#for all three spatial scales
source(file=file.path("process","src","GenerateComboHMPlusBasicOptical.R"))
comboHMPlusBasicOptical(filename = "combined_human_markers.rds") #File OUT name

# # QA functions 
# # 1. Set Minimum Reporting Levels (MRLs): 
# #    A. Functions for identifying blanks
# 
source(file = file.path("process","src","get_MMSD_blank_GRnums.R"))
source(file = file.path("process","src","get_GLRI_blank_GRnums.R"))
source(file = file.path("process","src","get_GLPF_blank_GRnums.R"))

#    B. Functions for Defining MRLs with the collective set of blanks from all scales
source(file = file.path("process","src","optMRL.R"))
source(file = file.path("process","src","optMRLAdjust.R"))
source(file = file.path("process","src","defineMRLs.R"))

#    C. Functions for adjusting raw data to include MRLs
source(file = file.path("process","src","applyMRLs.R"))

# Set MRLs
#   Define blank samples
get_MMSD_blank_GRnums()
get_GLRI_blank_GRnums()
get_GLPF_blank_GRnums()

#   Define MRLs
define_MRLs()

#   Apply MRLs
apply_MRLs()

# Start with EEMS and abs data + human marker data, and add in optical summary variables.
# Use list of optical summary variables for GLPF consistently for data sets from all three scales.

# GLPF: Add summary variables
# Begin with previously merged data set, strip out the current optical summary variables
# and add in all of the ones selected for GLPF

#Retrieve Turbidity data from NWIS for GLRI
source(file=file.path("process","src","get_GLRI_turbidity_data.R"))
#get_GLRI_turbidity_data() #Only run this once. It is a slow process.

#Populate the GLRI data set with turbidity parameters for each sampling period
source(file=file.path("process","src","PopulateTurbidity.R"))
PopulateTurbidity()

#Populate the MMSD data set with turbidity parameters for each sampling period
source(file=file.path("process","src","PopulateTurbidity_mmsd.R"))
Populate_turbidity_mmsd()

# Remove optical variables from initial summary files so we can add consistent
# variables for all three scales.

source(file.path("process", "src","remove_old_optical_signals.R"))
remove_old_optical_signals()

#Add summary variables
source(file.path("process", "src","get_summaries.R"))
get_summaries()

#Generate dataframe with bacteria markers and human virus data to feed into Figure 3
source("Process/src/GenerateComboHM_Virus.R")
comboHM_HV("HM_HV.rds")

##########################################
# Model
##########################################
# Source any functions:
# DO NOT RUN this again. It takes hours: scripts used = 
# DO NOT RUN again: MMSD lmer cv multi-org_rmse_by_site.R
# DO NOT RUN again: MMSD lmer cv multi-org_rmse_by_site_no_corr.R
# DO NOT RUN again: MMSD lmer cv multi-org_rmse_by_site_CG_BK.R
# DO NOT RUN again: MMSD lmer cv multi-org_rmse_by_site_CG_BK_no_corr.R
# DO NOT RUN again: GLRI Jones Island cv multi-org_rmse.R
# DO NOT RUN again: GLRI Jones Island cv multi-org_rmse_no_corr.R
# DO NOT RUN again: GLRI lmer cv multi-org_rmse_by_site.R
# DO NOT RUN again: GLRI lmer cv multi-org_rmse_by_site_no_corr.R

# Output is used in the evaluation scripts below

# Evaluate model results
source(file.path("model","src","rmse_model_selection.R"))
source(file.path("model","src","evaluate_rmse_model_selection.R"))
evaluate_rmse_model_selection()

#Develop final modeling table for large and sub-watersheds
source(file.path("model","src","modeling_summary_table.R"))
model_summary <- modeling_summary_table()
saveRDS(model_summary,file.path("model","out","modeling_summary_table.rds"))

#Save model objects from final models
  #MMSD


source(file.path("model","src","MMSD Suburban save final model objects.R"))
mmsd_models <- save_mmsd_model_objects()

  #GLRI
source(file.path("model","src","GLRI save final model objects.R"))
glri_models <- save_glri_model_objects()

#Run the modeling routines and generate HIB predictions
source(file.path("model","src","Generate_final_model_objects.R"))
mmsd_model_objects <- generate_final_model_objects("mmsd_summary.rds","MMSD",mmsd_models)
glri_model_objects <- generate_final_model_objects("glri_summary.rds","GLRI",glri_models)

final_model_list <- list(mmsd_model_objects,glri_model_objects)

save(final_model_list, file = file.path("model","out","final_model_objects.RData"))


##########################################
# Visualize
##########################################


source(file = file.path("plots","src","graph_model_selections.R"))
source(file = file.path("plots","src","Figure_2.R"))
source(file = file.path("plots","src","Figure_3.R"))
source(file = file.path("plots","src","Figure_4.R"))

model_plots <- graph_model_selections()
pdf(file.path("plots","out","model_selection_bar_charts.pdf"))
for(i in 1:length(model_plots)) print(model_plots[[i]])
dev.off()

fig_2 <- plot_fig_2()
ggsave(fig_2, filename = file.path("plots","out","Figure_2_bar_box.pdf"), width = 3.5, height = 5)
ggsave(fig_2, filename = file.path("plots","out","Figure_2_bar_box.png"), width = 3.5, height = 5)

fig_3 <- plot_fig_3()
ggsave(fig_3, filename = file.path("plots","out","Figure_3_virus_bacteria_relations.pdf"), width = 7, height = 2.5)

fig_4 <- plot_fig_4()
ggsave(fig_4, filename = file.path("plots","out","Figure_4_virus_occurrence_from_bacteria_predictions.pdf"), width = 3, height = 5)


##########################################
# Report
##########################################
# Source the functions:
source(file = file.path("report","src","create_report.R"))

#Results section
render(input = file.path("report","individual_reports","Results.Rmd"),
       output_dir = file.path("report","individual_reports","out"))
       
# Within the "report" folder, create .Rmd files to
# generate "chapters".

       

# You can create individual html files like this:
render(input = file.path("report","individual_reports","EEMs.Rmd"),
       output_dir = file.path("report","individual_reports","out"))

# Or bind them all together like this. 
summaryDF <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
EEMs <- readRDS(file.path("raw","GLPF","Optics", "EEMs3D_noQA.rds"))

create_report(EEMplot = EEMplot,
              summaryDF = summaryDF)

# See the action:
servr::httd("report/full_report/final_report")
