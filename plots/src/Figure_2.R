# Figure 2: Occurrence and boxplots by study

plot_fig_2 <- function() {
  dfHM <- readRDS(file.path("process","out","combined_human_markers.rds"))
  
  #Sum of human markers
  dfHM$hm <- dfHM$bacHum + dfHM$lachno2
  
  #Remove sites with < 10 observations
  site_count <- as.data.frame(table(dfHM$site),stringsAsFactors = FALSE)
  low_count_sites <- site_count$Var1[which(site_count$Freq < 10)]
  dfHM <- dfHM[-which(dfHM$site %in% low_count_sites),]
  
  
  # boxplot(hm~site+hydro_condition,data=dfHM,log="y",las=2)
  # boxplot(hm~site,data=dfHM,log="y",las=2)
  
  boxp <- ggplot(dfHM,aes(y=hm,x=site)) +
    geom_boxplot(aes(fill=hydro_condition)) +
    scale_y_continuous(trans='log10') +
    facet_grid(~scale, scales='free_x', space = "free_x") +
    #  geom_hline(yintercept = 225) +
    #  xTickLabelFont=c(14,"bold", "#993333") +
    theme_bw() +
    theme(text = element_text(size=14),
          #axis.text.x = element_text(angle=90, hjust=1)
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(x="",y="Human Markers (cn/100 ml)")
  
  #Occurrence
  HMoccurrence <- group_by(dfHM, scale, site, hydro_condition) %>% 
    summarize(occur = mean(hm > 500),
              count = n())
  
  barp <- ggplot(HMoccurrence,aes(y=occur,x=site,group=hydro_condition,fill=hydro_condition)) +
    geom_bar(stat = "identity", position = 'dodge') +
    geom_text(aes(y=1.1, label =  count), size = 2.5, angle = 90,nudge_x = rep(c(-0.25,0.25),16)) +
    ylim(0.0,1.2) +
    facet_grid(~scale, scales='free_x', space = "free_x") +
    #  geom_hline(yintercept = 225) +
    #    xTickLabelFont=c(14,"bold", "#993333") +
    theme_bw() +
    theme(text = element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    labs(x="",y="Occurrence ")
  
 # barp
  
  #png(filenm)
  fig_2 <- plot_grid(boxp,barp,  ncol=1, align="v")
  #dev.off()
  return(fig_2)
}
