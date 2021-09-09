plot_abstract_art <- function(){
  
  library(tidyverse)
  
  ## GGPLOT version: difficult to customize x-axis labels for publication...
  ## Occcurrence for HIB and FIB bins
  # source("Process/src/GenerateComboHM_Virus.R")
  # comboHM_HV("HM_HV.rds")
  
  # PANEL 1: ACTUAL sHM vs viruses
  
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
      summarise(Occurrence = mean(VirusOccur),
                n = length(VirusOccur))
    Occur$response <- response[i]
    Occur$category <- as.character(Occur$category)
    Occur[dim(Occur)[1],"category"] <- paste0("<",Occur[dim(Occur)[1],"category"])
    Occur$category <- factor(Occur$category,levels = Occur$category)
    
    HVOccur <- rbind(HVOccur,Occur)
  }
  
  df_response <- data.frame(response=response,full_name=response_names)
  HVOccur <- left_join(HVOccur,df_response)
#  HVOccur$full_name <- factor(HVOccur$full_name, levels = response_names)
  
  # Set up ordered factor for graphing
  category_text <- c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5","0-10^2","10^2-235","235-10^3","10^3-10^4",
                     " 10^4","0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5")
  
  conc_10_5 <- which(category_text %in% " 10^5")
  conc_10_4 <- which(category_text %in% " 10^4")
  
  category_text[c(conc_10_5,conc_10_4)] <- c(rep(expression(""> 10^5),length(conc_10_5)),rep(expression(""> 10^4),length(conc_10_4)))
  category_levels <- c("0-10^2","0-450","10^2-235","10^2-10^3","235-10^3","450-10^3","10^3-10^4",expression(""> 10^4),"10^4-10^5",expression(""> 10^5))
  HVOccur$category_text <- factor(category_text,levels = category_levels)
  
  HVoccur_obs <- HVOccur
  
  
  
  #PANEL 2, modeled sHM vs viruses
  
  load(file = file.path("model","out","final_model_objects.RData"))
  mmsd_model_objects <- final_model_list[[1]]
  glri_model_objects <- final_model_list[[2]]
  
  # #Start with MMSD
  # df_predictions <- full_join(mmsd_model_objects[[1]][[2]],mmsd_model_objects[[2]][[2]])
  
  for(i in 1:length(mmsd_model_objects)){
    df_add <- mmsd_model_objects[[i]][[2]]
    # df_add <- rename(df_add,lachno2 = Lachno.2.cn.100ml, bacHum = BACHUM.cn.100mls)
    # df_add$response <- ifelse(df_add$response == "Lachno.2.cn.100ml", "lachno2",df_add$response)
    # df_add$response <- ifelse(df_add$response == "BACHUM.cn.100mls", "bacHum",df_add$response)
    if(i == 1) df_predictions <- df_add
    if(i != 1) df_predictions <- full_join(df_predictions,df_add)
  }
  
  #Add in GLRI
  for(i in 1:length(glri_model_objects)){
    df_add <- glri_model_objects[[i]][[2]]
    df_add <- rename(df_add,lachno2 = Lachno.2.cn.100ml, bacHum = BACHUM.cn.100mls)
    df_add$response <- ifelse(df_add$response == "Lachno.2.cn.100ml", "lachno2",df_add$response)
    df_add$response <- ifelse(df_add$response == "BACHUM.cn.100mls", "bacHum",df_add$response)
    df_predictions <- full_join(df_predictions,df_add)
  }
  
  names(glri_model_objects[[i]][[2]])
  names(df_predictions)
  
  ## Virus Occcurrence for HIB prediction bins
  
  dfHV <- df_predictions %>% filter(!is.na(virus_occur))
  df_lachno <- df_predictions %>% filter(response == "lachno2")
  df_lachno <- rename(df_lachno,lachno_prediction = predictions,log_lachno = log_response) %>%
    select(-response)
  df_bacHum <- df_predictions %>% filter(response == "bacHum")
  df_bacHum <- rename(df_bacHum,bacHum_prediction = predictions,log_bacHum = log_response) %>%
    select(-response)
  
  dfsHM <- left_join(df_lachno,df_bacHum)
  dfsHM$sHM <- 10^dfsHM$lachno_prediction + 10^dfsHM$bacHum_prediction
  
  
  responses <- c("sHM")
  response_names <- c("sHM (Copy Number/100mL)")
  cutpoints <- list()
  
  cutpoints[[1]] <- c(0,450,1000,10000,100000,3000000)
  
  
  
  # Sum of lachno and bachum
  
  
  HVOccur <- data.frame()
  i <- 1
  df <- dfsHM
  df$category <- cut(df[,"sHM"],cutpoints[[i]],cutpoints[[i]][-1])
  Cats <- table(df$category)
  
  Occur <- df %>%
    group_by(category) %>%
    summarise(Occurrence = mean(virus_occur),
              n = length(virus_occur))
  Occur$response <- responses[i]
  Occur$category <- as.character(Occur$category)
  Occur[dim(Occur)[1],"category"] <- paste0("<",Occur[dim(Occur)[1],"category"])
  Occur$category <- factor(Occur$category,levels = Occur$category)
  
  HVOccur <- rbind(HVOccur,Occur)
  
  # Set up ordered factor for graphing
  category_text <- c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5")
  category_text[category_text == " 10^5"] <- expression(""> 10^5)
  category_levels <- c("0-450","450-10^3","10^3-10^4","10^4-10^5",expression(""> 10^5))
  HVOccur$category_text <- factor(category_text,levels = category_levels)
  HVOccur$full_name <- "Modeled sHM (Copy Number/100mL)"
  
  # HVOccur$category
  # m_bacteria_vs_virus <- ggplot(HVOccur,aes(x=category_text,y=Occurrence)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(
  #     aes(label=n),
  #     stat='identity',
  #     y=0.49,size = 2
  #   )+
  #   theme(axis.text.x=element_text(angle = 0, hjust = 0.5,vjust=0.0,size = 6, )) +
  #   xlab("Fitted Human Indicator Bacteria Concentration") +
  #   ylab("Human Virus Occurrence Proportion") +
  #   scale_x_discrete(labels = function(l) parse(text=l)) +
  #   theme(axis.title.x = element_text(size = 7),
  #         axis.title.y = element_text(size = 7),
  #         axis.text.x = element_text(size = 7,colour = "black"),
  #         axis.text.y = element_text(size = 7,colour = "black")) +
  #   annotate("text",label = "n = ",x = 0.6,y = 0.49, size = 2)
  # 
  
  HVOccur_plot <- full_join(HVOccur,HVoccur_obs)
  response_names <- c("sHM (Copy Number/100mL)","E. coli (CFU/100mL)", "Enterococci (Copy Number/100mL)","Modeled sHM (Copy Number/100mL)")
  
  HVOccur_plot$full_name <- factor(HVOccur_plot$full_name, levels = response_names)
  
  HVOccur_plot$category_text <- factor(HVOccur_plot$category_text,levels = HVOccur_plot$category_text[c(1,2,11,12,17,13,3,4,15,20)])
  
  response_names %in% unique(HVOccur_plot$full_name)
  
  ####################################
  
  HVOccur_plot_abstract_art <- HVOccur_plot %>%
    filter(grepl("sHM",full_name)) %>%
    mutate(facet_name = ifelse(full_name == "Modeled sHM (Copy Number/100mL)","Modeled Human Bacteria","Observed Human Bacteria"))
      
  bar_color <- colors()[239]
  
  bacteria_vs_virus <- ggplot(HVOccur_plot_abstract_art,aes(x=category_text,y=Occurrence)) +
    geom_bar(stat = "identity", color = "black",fill = "transparent", size = 1.) +
    facet_wrap(. ~ full_name, scales = "free_x",nrow = 1,ncol = 4, labeller = label_wrap_gen()) +
    #    geom_text(aes(label = c("A","B","C")), stat = "identity",y = 0.6,x=5.5) +
#    ylim(0,0.55)+
    scale_y_continuous(expand = c(0, 0),limits = c(0,0.55)) +
    theme(axis.text.x=element_text(angle = 0, hjust = 0.5,vjust=0.0,size = 5, )) +
    xlab("") +
    #xlab("Sum of Human Bacteroides and \nLachnospiraceae (count number/100 mL)") +
    #theme(axis.title.x = ggtext::element_markdown()) +
    ylab("Human Virus Occurrence") +
    scale_x_discrete(labels = function(l) parse(text=l))+
    theme(axis.title.x = element_text(size = 18, colour = "black", vjust = -1, face = "bold"),
          axis.title.y = element_text(size = 18, colour = "black", vjust = 3, face = "bold"),
          axis.text.x = element_text(size = 18,colour = "black", angle = 45, vjust = 1.,hjust = 1, face = "bold"),
          axis.text.y = element_text(size = 16,colour = "black", face = "bold"),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    theme(strip.background =element_rect(fill="transparent"))+
    theme(strip.text = element_text(colour = 'black'))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA)) +
    theme(plot.margin=unit(c(0,0,1,1),"cm")) +
    theme(panel.spacing = unit(4, "lines")) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1.5))
  
 
    bacteria_vs_virus
  
  ggsave(bacteria_vs_virus,filename = "test.png",bg = "transparent")
  
  ####################################
  
  
  # fig_3 <- plot_grid(bacteria_vs_virus, ncol=1, align="v",labels = c("A","B"), label_x = 0.94, label_y = 1,
  #                    label_size = 10)
  return(bacteria_vs_virus)
}

