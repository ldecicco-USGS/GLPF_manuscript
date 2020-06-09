library(tidyverse)


source("Process/src/GenerateComboHM_Virus.R")
comboHM_HV("HM_HV.rds")

dfHV <- readRDS(file=file.path("process","out","HM_HV.rds"))
dfHV <- dfHV %>% filter(!is.na(HumanVirus))

#Sum of human markers
dfHV$hm <- dfHV$bacHum + dfHV$lachno2

cutpoints <- c(0,450,1000,10000,100000,3000000)

#$cutpoints <- c(0,451*(2^(1:13)));cutpoints
dfHV$category <- cut(dfHV$hm,cutpoints,cutpoints[-1])
hmCats <- table(dfHV$category)

dfHV$VirusOccur <- ifelse(dfHV$HumanVirus > 0.011,1,0)
HVOccur <- dfHV %>%
  group_by(category) %>%
  summarize(Occurrence = mean(VirusOccur),
            n = length(VirusOccur))


barplot(HVOccur$Occurrence,col ="blue3",ylim = c(0.0,0.5))

axis_text <- parse(text=c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5"))
axis(side = 1,at = c(0.7,1.9,3.1,4.3,5.5),labels = axis_text)
axis(side = 1,at = (5.3),labels = ">",tick = FALSE)
mtext(side = 2,text = "Human Virus Occurrence Proportion",line = 2.5)
mtext(side = 1,text = "Sum of Human Indicator Bacteria (Copy Number/100mL)",line = 2.5)



#E. coli bins
response <- "eColi"
df <- filter(dfHV,!is.na(eColi))
cutpoints <- c(0,100,235,1000,10000,260000)
df$category <- cut(df[,response],cutpoints,cutpoints[-1])
Cats <- table(df$category)

Occur <- df %>%
  group_by(category) %>%
  summarize(Occurrence = mean(VirusOccur),
            n = length(VirusOccur))

barplot(Occur$Occurrence,col ="blue3",ylim = c(0.0,0.5))

axis_text <- parse(text=c("0-100","100-235","235-10^3","10^3-10^4"," 10^4"))
axis(side = 1,at = c(0.7,1.9,3.1,4.3,5.5),labels = axis_text)
axis(side = 1,at = (5.3),labels = ">",tick = FALSE)
mtext(side = 2,text = "Human Virus Occurrence Proportion",line = 2.5)
mtext(side = 1,text = "E. Coli(CFU/100mL)",line = 2.5)



#ent bins
response <- "ent"
df <- filter(dfHV,!is.na(ent))
cutpoints <- c(0,100,1000,10000,100000,2000000)
df$category <- cut(df[,response],cutpoints,cutpoints[-1])
Cats <- table(df$category)

Occur <- df %>%
  group_by(category) %>%
  summarize(Occurrence = mean(VirusOccur),
            n = length(VirusOccur))

barplot(Occur$Occurrence,col ="blue3",ylim = c(0.0,0.5))
axis_text <- parse(text=c("0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5"))
axis(side = 1,at = c(0.7,1.9,3.1,4.3,5.5),labels = axis_text)
axis(side = 1,at = (5.3),labels = ">",tick = FALSE)
mtext(side = 2,text = "Human Virus Occurrence Proportion",line = 2.5)
mtext(side = 1,text = "E. Coli(CFU/100mL)",line = 2.5)





# 
# 
# # Cumulative percent occurrence
# dfHV <- dfHV %>%
#   arrange(hm,VirusOccur)
# 
# dfHV$CumOccur <- 0
# for(i in 1:dim(dfHV)[1]) {
#   dfHV[i,"CumOccur"] <- mean(dfHV[1:i,"VirusOccur"])
# }
# 
# plot(dfHV$hm,dfHV$CumOccur,log="x",type="l")
