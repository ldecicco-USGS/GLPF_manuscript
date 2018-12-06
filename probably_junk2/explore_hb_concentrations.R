# Look for site names from GLPF

library(dataRetrieval)

#Load data
# ## GLPF:
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))


siteIDs <- unique(glpf$SiteID)
siteIDs <- siteIDs[which(!siteIDs %in% c("PROCESSING","00000000"))]

siteNames <- readNWISsite(siteIDs)


mmsd <- readRDS("raw/MMSD/dfOptP3P4Combined.rds")
unique(mmsd$ProjectSiteID)
load(file = file.path("raw","GLRI","FINAL8GLRIVirusSFSDec152015.RData"))
glri <- dfAll


df <- readRDS(file=file.path("process","out","combined_human_markers.rds"))
df$hb <- df$bacHum + df$lachno2

eventRatios <- df %>%
  group_by(scale,site,hydro_condition) %>%
  summarize(meanConc = mean(hb),
            medianConc = median(hb),)

mean_by_hydro <- spread(eventRatios[,-5],hydro_condition,meanConc)
mean_by_hydro$ratio = mean_by_hydro$Event/mean_by_hydro$`Low Flow`

plot(mean_by_hydro$site,mean_by_hydro$ratio)

median_by_hydro <- spread(eventRatios[,-4],hydro_condition,medianConc)
median_by_hydro$ratio = median_by_hydro$Event/mean_by_hydro$`Low Flow`

plot(mean_by_hydro$site,mean_by_hydro$ratio,las=2)

## Test sig diff by hydro event
sitesSmall <- c("WI","NY","MI")
names(sitesSmall) <- c("Kinnickinnic","Red","Middle Clinton")
sitesWatershed <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
names(sitesWatershed) <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
sitesSubWatershed <- c("HW","UW","MC","MW","MF","LD","CG","BK")
names(sitesSubWatershed) <- c("Honey","Underwood","Menomonee DS","Menomonee Mid","Menomonee US","Little Menomonee","Milwaukee US","Bark")
sites <- c(sitesSmall,sitesSubWatershed,sitesWatershed)

GT25 <- c(NA,NA,NA,1,1,1,1,1,0,0,0,1,1,1,0,0,0,0,0)

names(GT25) <- sites

pValues <- numeric()
for(i in which(!is.na(GT25))) {
  which(df$site==names(GT25)[i])
  subdfEvent <- filter(df,site==names(GT25)[i] & hydro_condition == "Event")
  subdfLow <- filter(df,site==names(GT25)[i] & hydro_condition != "Event")
  wilcox.result <- wilcox.test(subdfEvent$hb,subdfLow$hb)
  pValues <- c(pValues,wilcox.result$p.value)
}
names(pValues) <- names(GT25)[-(1:3)]
  