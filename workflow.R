# When you add a new package to your scripts, add 
# it here too. This will just help make sure we each
# have the right packages installed.

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

# This is an example of how to fetch the raw data.
# I would start from these lines in each of the sourced
# scripts further down:

# ## GLPF:
# glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
# ## GLRI:
# load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
# load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
# ## MMSD:

##########################################
# Process
##########################################

# Anything here would take the data from the "raw"
# folder, and convert it to something more useful. 
# Ideally, when this step is done, we'd have stuff populated
# in a folder "process/out".
# I would also say once this step is done, it can be commented
# out. Collaborators would need to un-comment these lines
# once. 
#
#This is where `remake` would come in handy!

# source(file = file.path("process","src","functions.R"))

##########################################
# Visualize
##########################################

# My vote would be that here we have a source file
# that creates functions to call, then here we call
# those functions. This makes what is happening in the 
# source file a little more transparent, and allows
# some fiddling here. 

# So, source the functions:
source(file = file.path("process","src","samplePlot.R"))
# Then call the functions:

# I'd say usually the "visualize" functions wouldn't take
# too long, so if you leave these lines un-commented,
# that is fine (so, comment out the process code, leave 
# the visualize code alone).


##########################################
# Report
##########################################

# Within the "report" folder, create .Rmd files to
# generate "chapters".

# You can create individual html files like this:

# Or bind them all together like this: