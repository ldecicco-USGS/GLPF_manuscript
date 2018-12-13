# Process GLPF optical data  to add summary variables

#Read in current summary file, vectorized fl data, and vectorized abs data
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
dffl <- readRDS(file.path("raw","GLPF", "optics", "dffl_noQA.rds"))
dfabs <- readRDS(file.path("raw","GLPF", "optics", "dfabs_noQA.rds"))


# Strip out previous optical summary variables
# names(glpf)

glpf <- glpf[,-c(55:641)]

# Determine MDLs