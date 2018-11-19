library(dplyr)
library(tidyr)
library(ggplot2)

dir.create("process", showWarnings = FALSE)
dir.create("probably_junk", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)

##########################################
# Fetch
##########################################
## GLPF:
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
## GLRI:
load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
## MMSD:

##########################################
# Process
##########################################

##########################################
# Visualize
##########################################

##########################################
# Report
##########################################

