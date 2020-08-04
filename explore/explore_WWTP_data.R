library(tidyverse)
library(dataRetrieval)

# Load GLPF + GLRI small scale data
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))

#Load MMSD Phase IV data (doesn't have WWTP data)
load(file.path("Raw","MMSD","PhaseIV","VirusPhaseIVData.Rdata"))

WWTP_JI <- readNWISqw("430125087540400", parameterCd = "All", startDate = "2007-01-01","2017-01-01")
parms <- unique(WWTP_JI$parm_cd)
parm_info <- readNWISpCode(parms)

parms_HIB <- c(31742,31743)

HIB_WW_P3_long <- filter(WWTP_JI,parm_cd %in% parms_HIB) %>%
  select(site_no,startDateTime,endDateTime,parm_cd,remark_cd,result_va,val_qual_tx)
HIB_WW_P3 <- HIB_WW_P3_long %>% pivot_wider(names_from = parm_cd,values_from = result_va)
names(HIB_WW_P3)[c(6,7)] <- c("bacHum","lachno")
HIB_WW_P3$sHM <- HIB_WW_P3$bacHum + HIB_WW_P3$lachno

HIB_WW_P3$Sewer_type <- "WWTP"

plot(HIB_WW_P3$bacHum,HIB_WW_P3$lachno,log="xy")
abline(0,1)

unique(glpf$VirusAutosampleorSewerGrab)

test <- glpf[grep("Sewer Grab",glpf$VirusAutosampleorSewerGrab),]

sewer_codes <- c("WW Influent","WWTP","Sanitary","Sanitary Grab")

WW <- glpf %>% filter(VirusAutosampleorSewerGrab %in% sewer_codes)

WW$VirusAutosampleorSewerGrab
WWSites <- readNWISsite( WW$SiteID)

WW$Sewer_type <- "WWTP"
WW$Sewer_type[grep("Sanitary",WW$VirusAutosampleorSewerGrab)] <- "Sanitary_Grab"

WW$sHM <- WW$bacHum + WW$lachno
WW_init <- WW %>%
  select(bacHum,lachno,sHM,Sewer_type)

ggplot(WW, aes(x=Sewer_type,y=sHM)) +
  geom_boxplot() + geom_jitter(width = 0.1)

WW_all <- full_join(WW_init,HIB_WW_P3)

ggplot(WW_all, aes(x=Sewer_type,y = sHM)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1) +
  scale_y_log10()

WW_all$sewer_type

