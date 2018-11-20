##########################################
# Project Setup
##########################################

# When you add a new package to your scripts, add 
# it here too. This will just help make sure we each
# have the right packages installed.

library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(bookdown)
library(servr)
library(USGSHydroOpt)
library(party)
library(partykit)
library(rpartScore)
library(RColorBrewer)

dir.create("process", showWarnings = FALSE)
dir.create(file.path("process","out"), showWarnings = FALSE)
dir.create("probably_junk", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)
dir.create(file.path("plots","out"), showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)
dir.create(file.path("report","individual_reports","out"), showWarnings = FALSE)
dir.create("model", showWarnings = FALSE)
dir.create(file.path("model","out"), showWarnings = FALSE)

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
source(file = file.path("process","src","functions.R"))

# Anything here would take the data from the "raw"
# folder, and convert it to something more useful. 
# Ideally, when this step is done, we'd have stuff populated
# in a folder "process/out".
# I would also say once this step is done, it can be commented
# out. Collaborators would need to un-comment these lines
# once. 
#
#This is where `remake` would come in handy!

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

# My vote would be that here we have a source file
# that creates functions to call, then here we call
# those functions. This makes what is happening in the 
# source file a little more transparent, and allows
# some fiddling here. 

########################
# Plot an EEM heatmap:
########################

# Call the functions:
summaryDF <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
EEMs <- readRDS(file.path("raw","GLPF","Optics", "EEMs3D_noQA.rds"))
EEMplot <- plot_single_EEM(EEMs, summaryDF$CAGRnumber[1])
# Save the plot:
ggsave(EEMplot, filename = file.path("plots","out","EEM.png"), width = 5, height = 5)
########################


##########################################
# Report
##########################################
# Source the functions:
source(file = file.path("report","src","create_report.R"))

# Within the "report" folder, create .Rmd files to
# generate "chapters".

# You can create individual html files like this:
render(input = file.path("report","individual_reports","EEMs.Rmd"),
       output_dir = file.path("report","individual_reports","out"))

# Or bind them all together like this. 
summaryDF <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
EEMs <- readRDS(file.path("raw","GLPF","Optics", "EEMs3D_noQA.rds"))

create_report(EEMplot = EEMplot)

# See the action:
servr::httd("report/full_report/final_report")
