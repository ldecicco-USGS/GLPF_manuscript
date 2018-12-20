# Use combined data set from three spatial scales and determine MDLs. 
# Adjust data with MDLs.

hm <- readRDS(file.path("process","out","combined_human_markers.rds"))[1:7]
              