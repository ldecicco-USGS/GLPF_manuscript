# Figure 2: Occurrence and boxplots by study

plot_fig_2 <- function() {
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(gridExtra)
  library(cowplot)
  
  dfHM <- readRDS(file.path("process","out","combined_human_markers.rds"))
  
  #Add short names for MMSD sites
  abbrevs <- c("Underwood", "Wauwatosa", "16th", "Cedarburg","Bark")
  abbrev_short <- c("UW","MW","MC","CG","BK")
  siteID <- c("04087088", "04087120", "04087142", "04086600", "05426060")
  sites <- data.frame(abbrev = abbrev_short,Abbreviation = abbrevs,Site_ID = siteID)
  
  i <- 1
  dfHM$shortname <- as.character(dfHM$site)
  for (i in 1:length(abbrevs)) {
  dfHM$shortname[which(dfHM$shortname == abbrev_short[i])] <- abbrevs[i]
  }
  
  site_levels <- c("WI","NY","MI","Underwood","16th","Wauwatosa","Cedarburg","Bark","Rouge","Clinton","Milwaukee","Raisin","Maumee","Portage","Manitowoc","Menominee")
  dfHM$shortname <- factor(dfHM$shortname,levels = site_levels)
  dfHM$site <- dfHM$shortname
  
  #Sum of human markers
  dfHM$hm <- dfHM$bacHum + dfHM$lachno2
  
  # boxplot(hm~site+hydro_condition,data=dfHM,log="y",las=2)
  # boxplot(hm~site,data=dfHM,log="y",las=2)
  
  boxp <- ggplot(dfHM,aes(y=hm,x=site)) +
    geom_boxplot(aes(fill=hydro_condition)) +
    facet_grid(~scale, scales='free_x', space = "free_x") +
    #  geom_hline(yintercept = 225) +
    #  xTickLabelFont=c(14,"bold", "#993333") +
    scale_y_continuous(trans="log10",breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(text = element_text(size=10),
          #axis.text.x = element_text(angle=90, hjust=1)
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = c(0.8, 0.8),
          legend.text = element_text(size = 8),               #legend text size
          legend.title = element_text(size = 8)) +            #legend title size
    guides(fill=guide_legend(title=element_blank())) +  #Legend title
    labs(x="",y="Human Markers (cn/100 ml)")
  
  #Occurrence
  HMoccurrence <- group_by(dfHM, scale, site, hydro_condition) %>% 
    summarise(occur = mean(hm > 500),
              count = n())
  
  barp <- ggplot(HMoccurrence,aes(y=occur,x=site,group=hydro_condition,fill=hydro_condition)) +
    geom_bar(stat = "identity", position = 'dodge') +
    geom_text(aes(y=1.1, label =  count), size = 1.5, angle = 90,nudge_x = rep(c(-0.25,0.25),16)) +
    ylim(0.0,1.19) +
    facet_grid(~scale, scales='free_x', space = "free_x") +
    #  geom_hline(yintercept = 225) +
    #    xTickLabelFont=c(14,"bold", "#993333") +
    theme_bw() +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle=90, hjust=1.0,vjust = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position='none') +
    labs(x="",y="Occurrence ")+
    theme(axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"))
  
 # barp
  
  #png(filenm)
  fig_2 <- plot_grid(boxp,barp,  ncol=1, align="v")
  #dev.off()
  return(fig_2)
}
