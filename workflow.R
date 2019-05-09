###
#Outline of workflow for the three different spatial scales
#
#1. Data processing: 
#  a. MRL determination for optical data
#      -Question: can we determine a common MRL for all data sets or separate?
#    i.   Locate blanks for each data set
#    ii.  Compute MRLs
#    iii. Apply MRLs to each raw data set
#  b. Compute summary optical data parameters
#    i.   Develop common set of parameters needed
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
#    i.   Begin with OLS modeling of common parameters
#    ii.  Explore additional parameters for watersheds where that doesn't work
#    iii. Develop summary table of models
#  b. Subwatersheds
#    i.  OLS modeling with common parameters: this works
#    ii. Include in modeling table with Large watersheds
#  c. Small 
#



##########################################
# Project Setup
##########################################

# When you add a new package to your scripts, add 
# it here too. This will just help make sure we each
# have the right packages installed.

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(rmarkdown)
library(bookdown)
library(servr)
library(USGSHydroOpt)
library(RColorBrewer)
library(scales)

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

# This is an example of how to fetch the raw data.
# I would start from these lines in each of the sourced
# scripts further down:

# ## GLPF:
# glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
#
# ## GLRI:
# load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
# load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
#
# ## MMSD:
# 
# 

##########################################
# Process
##########################################
# Source any functions:
# Anything here would take the data from the "raw"
# folder, and convert it to something more useful. 
# Ideally, when this step is done, we'd have stuff populated
# in a folder "process/out".
# I would also say once this step is done, it can be commented
# out. Collaborators would need to un-comment these lines
# once. 
#
# source(file = file.path("process","src","functions.R"))

#Combine MMSD P3 and P4 vectorized abs and fl data
# source(file = file.path("process","src","merge_fl_abs_mmsd_p3_p4_vectorized.R"))
# 
# 
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



source(file=file.path("process","src","GenerateComboHMPlusBasicOptical.R"))
comboHMPlusBasicOptical("combined_human_markers.rds")

##########################################
# Model
##########################################
# Source any functions:
# source(file = file.path("model","src","model_script.R"))

# This is where `remake` would come in handy!

##########################################
# Visualize
##########################################
# Source the functions:
source(file = file.path("plots","src","plot_EEM.R"))
source(file = file.path("plots","src","plot_base.R"))

# My vote would be that here we have a source file
# that creates functions to call, then here we call
# those functions. This makes what is happening in the 
# source file a little more transparent, and allows
# some fiddling here. 

source(file = file.path("plots","src","Figure_2.R"))
fig_2 <- plot_fig_2()
ggsave(fig_2, filename = file.path("plots","out","Figure_2_bar_box.png"), width = 7, height = 5)

########################
# Plot an EEM heatmap:
########################

# # Call the functions:
# summaryDF <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
# EEMs <- readRDS(file.path("raw","GLPF","Optics", "EEMs3D_noQA.rds"))
# EEMplot <- plot_single_EEM(EEMs, summaryDF$CAGRnumber[1])
# # Save the plot:
# ggsave(EEMplot, filename = file.path("plots","out","EEM.png"), width = 5, height = 5)
########################

########################
# Plot an base-R thing:
########################

png(file.path("plots","out","base_r_example.png"))
base_plot(summaryDF)
dev.off()

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
