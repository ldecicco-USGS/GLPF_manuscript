# GLPf manuscript, Figure 2
# Boxplots of human markers at three spatial scales

library(dplyr)

#Load data
# ## GLPF:
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
#
# ## GLRI:
load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
glri <- dfAll
# load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
#
# ## MMSD:

# 
# 



# Extract human markers and make one dataframe with all three scales

names(glpf)[21:40]

glpfHM <- glpf[,c("CAGRnumber","pdate","hydroCondition","bacHum", "bacHum_rk" , "lachno", "lachno_rk")]
glriHM <- glri[,c(