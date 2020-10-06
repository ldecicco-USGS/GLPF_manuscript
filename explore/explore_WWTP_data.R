library(tidyverse)
library(dataRetrieval)

# Load GLPF + GLRI small scale data
glpf <- readRDS(file.path("raw","GLPF", "summary_noQA.rds"))

#Load MMSD Phase IV data (doesn't have WWTP data for bacteria)
df_MMSD <- readRDS(file.path("process","out","mmsd_summary.rds"))


#Load WWTP data from the spatial indicator study
WWTP_JI <- readNWISqw("430125087540400", parameterCd = "All", startDate = "2007-01-01","2017-01-01")
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
  select(bacHum,lachno,sHM,ent,Sewer_type,DOCResult,T,F)
WW_init$Study <- "GLPF"

ggplot(WW, aes(x=Sewer_type,y=sHM)) +
  geom_boxplot() + geom_jitter(width = 0.1)
summary(WW$sHM)

WW_all <- full_join(WW_init,HIB_WW_P3) %>%
  rename(Signal_T = T, Signal_F = F)

ggplot(WW_all, aes(x=Sewer_type,y = sHM)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1) +
  scale_y_log10()
summary(WW_all$sHM)

WW_all$sHM <- WW_all$bacHum + WW_all$lachno
WW_all_long <- WW_all[c(1,2,3,4:9)] %>% pivot_longer(-c(Sewer_type,Study), names_to = c("parameter"), values_to = "value") %>%
  filter(!is.na(value))
WW_all_long <- WW_all_long %>%filter(Study == "GLPF")
WW_all_long$parameter <- factor(WW_all_long$parameter,levels = c("bacHum","lachno","sHM","ent","DOCResult","Signal_F","Signal_T"))
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

  
WW_local <- filter(WW_all, Sewer_type == "Sanitary_Grab")

modeldf <- na.exclude(WW_local[,c("sHM","Signal_F","Signal_T")])

modeldf <- as.data.frame(modeldf)
m <- lm(log(sHM) ~ Signal_F  + Signal_T,data == modeldf)
m <- lm(formula(log(modeldf$sHM) ~ modeldf$Signal_F  + modeldf$Signal_T))
summary(m)

sHM_vect <- modeldf$sHM
F_vect <- modeldf$Signal_F
T_vect <- modeldf$Signal_T

m <- lm(log10(sHM_vect) ~ F_vect + T_vect)

summary (m)


#### need to check this once Phase IV data is incorporated   ####
#### Currently there is only 4 observations                  ####

WWTP <- filter(WW_all, Sewer_type == "WWTP")

modeldf <- na.exclude(WWTP[,c("sHM","Signal_F","Signal_T")])

modeldf <- as.data.frame(modeldf)
m <- lm(formula(sHM ~ signal_F + signal_T),data == modeldf)


sHM_vect <- modeldf$sHM
F_vect <- modeldf$Signal_F
T_vect <- modeldf$Signal_T

m <- lm(log10(sHM_vect) ~ F_vect + T_vect)

summary (m)


## Compute coefficient of variation for local sewage and influent 
#

coef_var <- WW_all_long %>%
  group_by(Sewer_type,parameter) %>%
  summarize(cv = sd(value,na.rm=TRUE)/mean(value,na.rm=TRUE),
            mean = mean(value,na.rm=TRUE),
            stdev = sd(value,na.rm=TRUE))


coef_var_wide <- pivot_wider(data = coef_var,names_from = c(parameter), values_from = c(cv,mean,stdev))

grab <- coef_var[1:7,]
names(grab) <- paste0("Local_",names(grab))
WWTP <- coef_var[8:14,]
names(WWTP) <- paste0("WWTP_",names(WWTP))
coef_var_wide <- cbind(grab,WWTP)
cv <- coef_var_wide %>%
  ungroup() %>%
  select(Parameter = Local_parameter,Local = Local_cv,WWTP=WWTP_cv)

write.csv(cv,"CV_local_vs_WWTP.csv)
  
