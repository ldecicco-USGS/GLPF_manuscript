plot_fig_3 <- function(){
  
  library(tidyverse)
  
  ## GGPLOT version: difficult to customize x-axis labels for publication...
  ## Occcurrence for HIB and FIB bins
  # source("Process/src/GenerateComboHM_Virus.R")
  # comboHM_HV("HM_HV.rds")
  
  dfHV <- readRDS(file=file.path("process","out","HM_HV.rds"))
  dfHV_bkcg <- dfHV %>% filter(!is.na(HumanVirus)) %>%
    filter((site %in% c("BK","CG")))
  dfHV <- dfHV %>% filter(!is.na(HumanVirus)) %>%
    filter(!(site %in% c("BK","CG")))
  
  
  #Sum of human markers
  dfHV$hm <- dfHV$bacHum + dfHV$lachno2
  
  response <- c("hm","eColi","ent")
  response_names <- c("sHM (Copy Number/100mL)","E. coli (CFU/100mL)", "Enterococci (Copy Number/100mL)")
  cutpoints <- list()
  cutpoints[[1]] <- c(0,450,1000,10000,100000,3000000)
  cutpoints[[2]] <- c(0,100,235,1000,10000,260000)
  cutpoints[[3]] <- c(0,100,1000,10000,100000,2000000)
  
  dfHV$HumanVirus_all <- dfHV$HumanVirus
#  dfHV$HumanVirus <- dfHV$Adenovirus.C.D.F + dfHV$Adenovirus.A + dfHV$Enterovirus
#  dfHV <- filter(dfHV,!is.na(G2.Norovirus))
  dfHV$VirusOccur <- ifelse(dfHV$HumanVirus > 0.011,1,0)
  
  #Sum of human markers
  dfHV$hm <- dfHV$bacHum + dfHV$lachno2
  
  HVOccur <- data.frame()
  for (i in 1:length(response)) {
    df <- subset(dfHV,!is.na(dfHV[,response[i]]))
    df$category <- cut(df[,response[i]],cutpoints[[i]],cutpoints[[i]][-1])
    Cats <- table(df$category)
    
    Occur <- df %>%
      group_by(category) %>%
      summarize(Occurrence = mean(VirusOccur),
                n = length(VirusOccur))
    Occur$response <- response[i]
    Occur$category <- as.character(Occur$category)
    Occur[dim(Occur)[1],"category"] <- paste0("<",Occur[dim(Occur)[1],"category"])
    Occur$category <- factor(Occur$category,levels = Occur$category)
    
    HVOccur <- rbind(HVOccur,Occur)
  }
  
  df_response <- data.frame(response=response,full_name=response_names)
  HVOccur <- left_join(HVOccur,df_response)
  HVOccur$full_name <- factor(HVOccur$full_name, levels = response_names)
  
  # Set up ordered factor for graphing
  category_text <- c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5","0-10^2","10^2-235","235-10^3","10^3-10^4",
                     " 10^4","0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5")
  
  conc_10_5 <- which(category_text %in% " 10^5")
  conc_10_4 <- which(category_text %in% " 10^4")
  
  category_text[c(conc_10_5,conc_10_4)] <- c(rep(expression(""> 10^5),length(conc_10_5)),rep(expression(""> 10^4),length(conc_10_4)))
  category_levels <- c("0-10^2","0-450","10^2-235","10^2-10^3","235-10^3","450-10^3","10^3-10^4",expression(""> 10^4),"10^4-10^5",expression(""> 10^5))
  HVOccur$category_text <- factor(category_text,levels = category_levels)
  
  bacteria_vs_virus <- ggplot(HVOccur,aes(x=category_text,y=Occurrence)) +
    geom_bar(stat = "identity") +
    facet_wrap(. ~ full_name, scales = "free_x",nrow = 3,ncol = 1) +
#    geom_text(aes(label = c("A","B","C")), stat = "identity",y = 0.6,x=5.5) +
    ylim(0,0.55) +
    geom_text(
      aes(label=n),
      stat='identity',
      y=0.53,size = 2
    )+
    theme(axis.text.x=element_text(angle = 0, hjust = 0.5,vjust=0.0,size = 6, )) +
    xlab("Indicator Bacteria Concentration") +
    ylab("Human Virus Occurrence Proportion") +
    scale_x_discrete(labels = function(l) parse(text=l))+
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 7,colour = "black"),
          axis.text.y = element_text(size = 7,colour = "black")) +
  annotate("text",label = "n = ",x = 0.6,y = 0.53,size = 2)
  return(bacteria_vs_virus)
}

# pdf("plots/out/Figure_3.pdf",height = 6, width = 7/2.54)
# print(bacteria_vs_virus)
# dev.off()
#shell.exec("test.pdf")


# 
# #############Base graphics version#####################
# 
# source("Process/src/GenerateComboHM_Virus.R")
# comboHM_HV("HM_HV.rds")
# 
# dfHV <- readRDS(file=file.path("process","out","HM_HV.rds"))
# dfHV <- dfHV %>% filter(!is.na(HumanVirus))
# 
# # 3-panel graph, one panel for each indicator bacteria vs HV occurrence
# 
# dfHV$VirusOccur <- ifelse(dfHV$HumanVirus > 0.011,1,0)
# 
# #Sum of human markers
# dfHV$hm <- dfHV$bacHum + dfHV$lachno2
# 
# response <- c("hm","eColi","ent")
# response_names <- c("sHM (Copy Number/100mL)","E. coli (CFU/100mL)", "Enterococci (Copy Number/100mL)")
# cutpoints <- list()
# cutpoints[[1]] <- c(0,450,1000,10000,100000,3000000)
# cutpoints[[2]] <- c(0,100,235,1000,10000,260000)
# cutpoints[[3]] <- c(0,100,1000,10000,100000,2000000)
# 
# axis_text <- list()
# axis_text[[1]] <- parse(text=c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5"))
# axis_text[[2]] <- parse(text=c("0-100","100-235","235-10^3","10^3-10^4"," 10^4"))
# axis_text[[3]] <- parse(text=c("0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5"))
# 
# filenm <- "fig_3.pdf"
# pdf(filenm)
# par(mfcol = c(5,2),mar = c(1.5,0,1,3),oma = c(5,4,2,1))
# 
# for (i in 1:length(response)) {
#   df <- subset(dfHV,!is.na(dfHV[,response[i]]))
#   df$category <- cut(df[,response[i]],cutpoints[[i]],cutpoints[[i]][-1])
#   Cats <- table(df$category)
#   
#   Occur <- df %>%
#     group_by(category) %>%
#     summarize(Occurrence = mean(VirusOccur),
#               n = length(VirusOccur))
#   Occur$response <- response[i]
#   Occur$category <- as.character(Occur$category)
#   Occur[dim(Occur)[1],"category"] <- paste0("<",Occur[dim(Occur)[1],"category"])
#   Occur$category <- factor(Occur$category,levels = Occur$category)
#   
#   barplot(Occur$Occurrence,col ="grey",ylim = c(0.0,0.5),las=2)
#   
#   axis(side = 1,at = c(0.7,1.9,3.1,4.3,5.5),labels = axis_text[[i]],cex=0.5)
#   axis(side = 1,at = (5.3),labels = ">",tick = FALSE,cex=0.7)
#   if(i == 1) {
#     text(Occur$n[5],x = c(5.5), y = (Occur$Occurrence[5] - 0.075))
#     text(Occur$n[-5],x = c(0.7,1.9,3.1,4.3), y = (Occur$Occurrence[-5] + 0.075))
#     }
#   else{
#     text(Occur$n,x = c(0.7,1.9,3.1,4.3,5.5), y = (Occur$Occurrence + 0.075))
#   }
#   text("n =", x = 0.25, y = Occur$Occurrence[1] + 0.075)
#   mtext(side = 3,response_names[i],cex = 0.6,line = -1,font = 2)
#   # mtext(Occur$n, at = c(0.7,1.9,3.1,4.3,5.5), line = -2, side = 3,cex = 0.5)
#   # mtext("n=",line = -2, adj=0,side = 3,cex = 0.5)
#   
#   
# }
# 
# mtext(side = 1,text = "Bacteria Concentration",line = 3,cex=0.7)
# mtext(side = 2,text = "Human Virus Occurrence Proportion",line = 2.5,outer = TRUE,adj = 1,cex = 0.7)
# 
# dev.off()
# shell.exec(filenm)
