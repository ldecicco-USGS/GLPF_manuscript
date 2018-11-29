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

# Consider all values < 225 as censored to make censored values consistent across studies
# Set all values < 225 to 225

glri[which(glri$BACHUM.cn.100mls <226),"BACHUM.cn.100mls"] <- 225 '


# load(file = file.path("raw","GLRI","GLRIdfOptSummary2016-03-03.RData"))
#
# ## MMSD:

# 
# 



# Extract human markers and make one dataframe with all three scales

names(glpf)[21:40]

glpf$bacHum
glpfHM <- glpf[,c("CAGRnumber","State","pdate","hydroCondition","bacHum", "bacHum_rk" , "lachno", "lachno_rk")]
glpfHM$scale <- "small"

names(glri)[1:20]
glriHM <- glri[,c("GRnumber","Site","psdate","FinalHydrologicConditionDecision","BACHUM.cn.100mls", "Lachno.2.cn.100ml")]




