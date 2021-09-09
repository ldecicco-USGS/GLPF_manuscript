# Figure 2: Occurrence and boxplots by study

plot_fig_2 <- function() {
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(grid)
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
  
  #Occurrence
  HMoccurrence <- group_by(dfHM, scale, site, hydro_condition) %>% 
    summarise(occur = mean(hm > 500),
              count = n())
  
  # boxplot(hm~site+hydro_condition,data=dfHM,log="y",las=2)
  # boxplot(hm~site,data=dfHM,log="y",las=2)
  
  panel <- data.frame(label = c("","","A"), site = c(1,1,7))
  
  boxp <- ggplot(dfHM,aes(y=hm,x=site)) +
    geom_boxplot(aes(fill=hydro_condition))  +
    geom_text(data = HMoccurrence, aes(y=10^8.5, label =  count), size = 3, angle = 90,nudge_x = rep(c(-0.25,0.25),16))+
    coord_cartesian(#xlim = c(10, 14), # This focuses the x-axis on the range of interest
                    clip = 'off') +
    facet_grid(~scale, scales='free_x', space = "free_x") +
    #  geom_hline(yintercept = 225) +
    #  xTickLabelFont=c(14,"bold", "#993333") +
    scale_y_continuous(trans="log10",breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(text = element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1.0,vjust = 0.5),
          legend.position = c(0.8, 0.8),
          legend.text = element_text(size = 8),               #legend text size
          legend.title = element_text(size = 8)) +            #legend title size
    labs(x="",y="sHM (cn/100mL)")+
    guides(fill=guide_legend(title=element_blank())) +  #Legend title
    ggtitle("Human Markers (cn/100 ml)",)+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(boxp, filename = file.path("plots","out","Figure_2_bar_box_presentation.png"), width = 10, height = 5)
  
  # 
  # barp <- ggplot(HMoccurrence,aes(y=occur,x=site,group=hydro_condition,fill=hydro_condition)) +
  #   geom_bar(stat = "identity", position = 'dodge') +
  #   geom_text(aes(y=1.1, label =  count), size = 1.5, angle = 90,nudge_x = rep(c(-0.25,0.25),16))+
  #   #geom_text(aes(y=1.15, x = 1,label =  "n = "), size = 2, angle = 0) +
  #   ylim(0.0,1.19) +
  #   facet_grid(~scale, scales='free_x', space = "free_x") +
  #   #  geom_hline(yintercept = 225) +
  #   #    xTickLabelFont=c(14,"bold", "#993333") +
  #   labs(x="",y="Occurrence ")+
  #   theme_bw() +
  #   theme(text = element_text(size=10),
  #         axis.text.x = element_text(angle=90, hjust=1.0,vjust = 0.5),
  #         strip.background = element_blank(),
  #         strip.text.x = element_blank(),
  #         legend.position='none') +
  #    # theme(axis.text.x = element_text(colour = "black"),
  #    #      axis.text.y = element_text(colour = "black")) +
  #   labs(tag = "n = ")+
  #   theme(plot.tag.position = c(-0.10,0.93),plot.tag = element_text(size=8),plot.margin=unit(c(0,1,0.,0),"cm")) 
  #   # labs(tag = "B")+
  #   # theme(plot.margin=unit(c(1,1.2,1.5,1.2),"cm")) + theme(plot.tag.position = c(1.05, 1))
  #   
  # 
  # barp
  # 
  #png(filenm)
  #fig_2 <- grid.arrange(boxp,barp, left = textGrob("n =", vjust = 2.5,hjust = -2.4,gp=gpar(fontsize = 10)))
  
   # fig_2 <- plot_grid(boxp,NULL,barp,NULL, ncol=2, align="v",labels = c("","A","","B"),
   #                    label_size = 10)
   fig_2 <- boxp
   fig_2
   #dev.off()
  return(fig_2)
}
