# Figure 2: Occurrence and boxplots by study


dfHM <- readRDS(file.path("process","out","combined_human_markers.rds"))

#Sum of human markers
dfHM$hm <- dfHM$bacHum + dfHM$lachno2



boxplot(hm~site+hydro_condition,data=dfHM,log="y",las=2)
boxplot(hm~site,data=dfHM,log="y",las=2)

names(dfHM)
