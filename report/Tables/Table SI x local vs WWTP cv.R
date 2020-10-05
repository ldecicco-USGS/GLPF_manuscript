library(tidyverse)
library(dataRetrieval)

# Load GLPF + GLRI small scale data
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))


sewer_codes <- c("WW Influent","WWTP","Sanitary","Sanitary Grab")

WW <- glpf %>% filter(VirusAutosampleorSewerGrab %in% sewer_codes)

WW$VirusAutosampleorSewerGrab
WWSites <- readNWISsite( WW$SiteID)

WW$Sewer_type <- "WWTP"
WW$Sewer_type[grep("Sanitary",WW$VirusAutosampleorSewerGrab)] <- "Sanitary_Grab"

WW$sHM <- WW$bacHum + WW$lachno
WW_init <- WW %>%
  select(bacHum,lachno,sHM,ent,Sewer_type,DOCResult,T,F)

WW_all <- WW_init

WW_all_long <- WW_all %>% pivot_longer(-c(Sewer_type), names_to = c("parameter"), values_to = "value") %>%
  filter(!is.na(value))
WW_all_long$parameter <- factor(WW_all_long$parameter,levels = c("bacHum","lachno","sHM","ent","DOCResult","F","T"))
ggplot(WW_all_long, aes(x=Sewer_type,y=value)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  facet_wrap(vars(parameter),scales = "free_y") +
  scale_y_log10()

WW_all_long_sHB <- WW_all_long %>%
  filter(!(parameter %in% c("bacHum","lachno")))

ggplot(WW_all_long_sHB, aes(x=Sewer_type,y=value)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  facet_wrap(vars(parameter),scales = "free_y") +
  scale_y_log10()

  
## Compute coefficient of variation for local sewage and influent 
#

coef_var <- WW_all_long %>%
  group_by(Sewer_type,parameter) %>%
  summarize(cv = sd(value,na.rm=TRUE)/mean(value,na.rm=TRUE),
            mean = mean(value,na.rm=TRUE),
            stdev = sd(value,na.rm=TRUE),
            max = max(value,na.rm=TRUE),
            min = min(value,na.rm=TRUE))


coef_var_wide <- pivot_wider(data = coef_var,names_from = c(parameter), values_from = c(cv,mean,stdev))

grab <- coef_var[1:7,]
names(grab) <- paste0("Local_",names(grab))
WWTP <- coef_var[8:14,]
names(WWTP) <- paste0("WWTP_",names(WWTP))
coef_var_wide <- cbind(grab,WWTP)
cv <- coef_var_wide %>%
  ungroup() %>%
  select(Parameter = Local_parameter,Local = Local_cv,WWTP=WWTP_cv)

cv$ratio <- cv$Local/cv$WWTP
range(cv$ratio[-c(1:2)])

write.csv(cv,"./report/tables/CV_local_vs_WWTP.csv")
  

#Compare DOC to natural waters
coef_var_wide$min[5] #range of local sanitary = 2.56 - 139

glri <- readRDS("./process/out/glri_summary.rds")
range(glri$DOCResult,na.rm=TRUE) # range = 2.11 - 35
summary(glri$DOCResult,na.rm=TRUE) # range = 2.11 - 35
quantile(glri$DOCResult,0.02,na.rm=TRUE)

glpf <- readRDS("./process/out/glpf_summary.rds")
glpf_env <- glpf %>% 
  filter(!(VirusAutosampleorSewerGrab %in% c("WW Influent", "Sanitary Grab","Sanitary","WWTP")))
range(glpf_env$DOCResult,na.rm=TRUE) # range = 1.68-148.75
quantile(glpf_env$DOCResult,0.02,na.rm=TRUE)

glpf_ww_WWTP <- glpf %>% 
  filter((VirusAutosampleorSewerGrab %in% c("WW Influent","WWTP")))

range(glpf_ww_WWTP$DOCResult,na.rm=TRUE) # range = 20 - 91
quantile(glpf_ww_WWTP$DOCResult,0.02,na.rm=TRUE)

glpf_ww_WWTP <- glpf %>% 
  filter(!(VirusAutosampleorSewerGrab %in% c("WW Influent","WWTP")))

range(glpf_ww_WWTP$DOCResult,na.rm=TRUE) # range = 20 - 91
quantile(glpf_ww_WWTP$DOCResult,0.02,na.rm=TRUE)
