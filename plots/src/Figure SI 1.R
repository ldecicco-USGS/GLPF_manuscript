
plot_ww <- function() {
  library(tidyverse)
  library(dataRetrieval)
  
  # Load GLPF + GLRI small scale data
  glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))
  
  #Load MMSD Phase IV data (doesn't have WWTP data for bacteria)
  df_MMSD <- readRDS(file.path("process","out","mmsd_summary.rds"))
  
  
  #Load WWTP data from the spatial indicator study
  #WWTP_JI <- readNWISqw(c("430125087540400","425308087504900"), parameterCd = "All", startDate = "2007-01-01","2017-01-01")
  #saveRDS(WWTP_JI,file = "raw/MMSD/WW_data.rds")
  WWTP_JI <- readRDS(file = "raw/MMSD/WW_data.rds")
  parms <- unique(WWTP_JI$parm_cd)
  parm_info <- readNWISpCode(parms)
  
  parms_HIB <- c(31742,31743,31745)
  names(parms_HIB) <- c("bacHum","lachno","ent")
  HIB_WW_P3_long <- filter(WWTP_JI,parm_cd %in% parms_HIB) %>%
    select(site_no,startDateTime,endDateTime,parm_cd,remark_cd,result_va,val_qual_tx)
  HIB_WW_P3 <- HIB_WW_P3_long %>% pivot_wider(names_from = parm_cd,values_from = result_va)
  names(HIB_WW_P3)[c(6,7,8)] <- c("bacHum","lachno","ent")
  HIB_WW_P3$sHM <- HIB_WW_P3$bacHum + HIB_WW_P3$lachno
  
  HIB_WW_P3$Sewer_type <- "WWTP"
  HIB_WW_P3$Study <- "P3"
  
  sewer_codes <- c("WW Influent","WWTP","Sanitary","Sanitary Grab")
  
  WW <- glpf %>% filter(VirusAutosampleorSewerGrab %in% sewer_codes)
  
  WW$VirusAutosampleorSewerGrab
  WWSites <- readNWISsite( WW$SiteID)
  
  WW$Sewer_type <- "WWTP"
  WW$Sewer_type[grep("Sanitary",WW$VirusAutosampleorSewerGrab)] <- "Sanitary_Grab"
  
  WW$sHM <- WW$bacHum + WW$lachno
  WW_init <- WW %>%
    select(bacHum,lachno,sHM,ent,Sewer_type,DOCResult,T,F)
  WW_init$Study <- "GLPF"
  
  WW_all <- full_join(WW_init,HIB_WW_P3) %>%
    rename(Signal_T = T, Signal_F = F)
  
  WW_all_long <- WW_all[c(1,2,3,4:9)] %>% pivot_longer(-c(Sewer_type,Study), names_to = c("parameter"), values_to = "value") %>%
    filter(!is.na(value)) %>% 
    arrange(parameter,Sewer_type)
  #WW_all_long <- WW_all_long %>%filter(Study == "GLPF")
  WW_all_long$parameter <- factor(WW_all_long$parameter,levels = c("bacHum","lachno","sHM","ent","DOCResult","Signal_F","Signal_T"))
  # ggplot(WW_all_long, aes(x=Sewer_type,y=value)) +
  #   geom_boxplot() +
  #   geom_jitter(width = 0.1) +
  #   facet_wrap(vars(parameter),scales = "free_y") +
  #   scale_y_log10()
  
  WW_all_long_sHB <- WW_all_long %>%
    filter(!(parameter %in% c("bacHum","lachno")))
  WW_all_long_sHB$Sewer_type <- sub(pattern = "Sanitary_Grab",replacement = "Local",x=  WW_all_long_sHB$Sewer_type)
  WW_all_long_sHB$Sewer_type <- sub(pattern = "WWTP",replacement = "Regional",x=  WW_all_long_sHB$Sewer_type)
  WW_all_long_sHB$parameter <- sub(pattern = "DOCResult",replacement = "DOC",x=  WW_all_long_sHB$parameter)
  WW_all_long_sHB$parameter <- sub(pattern = "Signal_F",replacement = "F",x=  WW_all_long_sHB$parameter)
  WW_all_long_sHB$parameter <- sub(pattern = "Signal_T",replacement = "T",x=  WW_all_long_sHB$parameter)
  WW_all_long_sHB$parameter <- sub(pattern = "ent",replacement = "Enterococci",x=  WW_all_long_sHB$parameter)
  
  
  
  ww_plot <- ggplot(WW_all_long_sHB, aes(x=Sewer_type,y=value)) +
    geom_boxplot() +
    geom_jitter(width = 0.1) +
    facet_wrap(vars(parameter),scales = "free_y") +
    scale_y_log10() +
    xlab("Sanitary Sewer Source") +
    ylab("Concentration")
  
  png(file.path("plots","out","Figure_SI_1.png"))
  print(x = ww_plot)
  dev.off()
  
  ww_plot
}
