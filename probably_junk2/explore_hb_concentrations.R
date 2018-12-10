# Look for site names from GLPF

library(dataRetrieval)
library(tidyr)
library(ggplot2)
library(scales)

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

plot(mean_by_hydro$site,mean_by_hydro$ratio,las=2)

median_by_hydro <- spread(eventRatios[,-4],hydro_condition,medianConc)
median_by_hydro$ratio = median_by_hydro$Event/mean_by_hydro$`Low Flow`

plot(mean_by_hydro$site,mean_by_hydro$ratio,las=2)

## Test sig diff by hydro event
sitesSmall <- c("WI","NY","MI")
names(sitesSmall) <- c("Kinnickinnic","Red","Middle Clinton")
sitesWatershed <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
names(sitesWatershed) <- c("Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
sitesSubWatershed <- c("UW","MC","MW","CG","BK")
names(sitesSubWatershed) <- c"Underwood","Menomonee DS","Menomonee US","Milwaukee US","Bark")
sites <- c(sitesSmall,sitesSubWatershed,sitesWatershed)

#GT25 <- c(NA,NA,NA,1,1,1,1,1,0,0,0,1,1,1,0,0,0,0,0)

#names(GT25) <- sites

pValues <- numeric()
for(i in levels(df$site)) {
  subdfEvent <- filter(df,site==i & hydro_condition == "Event")
  subdfLow <- filter(df,site==i & hydro_condition != "Event")
  wilcox.result <- wilcox.test(subdfEvent$hb,subdfLow$hb)
  pValues <- c(pValues,wilcox.result$p.value)
}
names(pValues) <- levels(df$site)
pValues <=0.05

urban_GT_25 <- c("WI","MI","NY","UW","MC","MW","Rouge","Clinton","Milwaukee") #Define sites with > 25% urban

df$LU_cat <- ifelse(df$site %in% urban_GT_25,"GT_25_%_Urban","LT_25_%_Urban")

mediumUrbanEvent <- df[which(df$scale == "subwatershed" & df$LU_cat == "GT_25_%_Urban" & df$hydro_condition=="Event"),"hb"]
mediumUrbanLowFlow <- df[which(df$scale == "subwatershed" & df$LU_cat == "GT_25_%_Urban" & df$hydro_condition!="Event"),"hb"]
wilcox.test(mediumUrbanEvent,mediumUrbanLowFlow)

mediumNonUrbanEvent <- df[which(df$scale == "subwatershed" & df$LU_cat != "GT_25_%_Urban" & df$hydro_condition=="Event"),"hb"]
mediumNonUrbanLowFlow <- df[which(df$scale == "subwatershed" & df$LU_cat != "GT_25_%_Urban" & df$hydro_condition!="Event"),"hb"]
wilcox.test(mediumNonUrbanEvent,mediumNonUrbanLowFlow)

largeUrbanEvent <- df[which(df$scale == "watershed" & df$LU_cat == "GT_25_%_Urban" & df$hydro_condition=="Event"),"hb"]
largeUrbanLowFlow <- df[which(df$scale == "watershed" & df$LU_cat == "GT_25_%_Urban" & df$hydro_condition!="Event"),"hb"]
wilcox.test(largeUrbanEvent,largeUrbanLowFlow)

largeNonUrbanEvent <- df[which(df$scale == "watershed" & df$LU_cat != "GT_25_%_Urban" & df$hydro_condition=="Event"),"hb"]
largeNonUrbanLowFlow <- df[which(df$scale == "watershed" & df$LU_cat != "GT_25_%_Urban" & df$hydro_condition!="Event"),"hb"]
wilcox.test(largeNonUrbanEvent,largeNonUrbanLowFlow)

ggplot(df,aes(y=hb,x=LU_cat)) +
  geom_boxplot(aes(fill=hydro_condition)) +
  scale_y_continuous(trans="log10",breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(~scale, scales='free_x', space = "free_x")
  

boxplot(df$hb~df$scale + df$LU_cat,log="y",las=2)
