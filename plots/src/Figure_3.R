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
mtext(side = 1,text = "Enterococci (Copy Number/100mL)",line = 2.5)





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


## GGPLOT version: not ideal and difficult to customize for publication...
## Occcurrence for HIB and FIB bins


response <- c("hm","eColi","ent")
response_names <- c("sHM (Copy Number/100mL)","E. coli (CFU/100mL)", "Enterococci (Copy Number/100mL)")
cutpoints <- list()
cutpoints[[1]] <- c(0,450,1000,10000,100000,3000000)
cutpoints[[2]] <- c(0,100,235,1000,10000,260000)
cutpoints[[3]] <- c(0,100,1000,10000,100000,2000000)

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
category_text <- c("0-450","450-10^3","10^3-10^4","10^4-10^5",">10^5","0-10^2","10^2-235","235-10^3","10^3-10^4",
                   ">10^4","0-10^2","10^2-10^3","10^3-10^4","10^4-10^5",">10^5")
category_levels <- c("0-10^2","0-450","10^2-235","10^2-10^3","235-10^3","450-10^3","10^3-10^4","<10^4","10^4-10^5","<10^5")
HVOccur$category_text <- factor(category_text,levels = category_levels)

ggplot(HVOccur,aes(x=category_text,y=Occurrence)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ full_name, scales = "free_x",nrow = 3,ncol = 1) +
  geom_text(
    aes(label=n),
    stat='identity',
    nudge_y=0.05,
  )+
  xlab("Indicator Bacteria Concentration") +
  ylab("Human Virus Occurrence Proportion")


#############Base graphics version#####################

source("Process/src/GenerateComboHM_Virus.R")
comboHM_HV("HM_HV.rds")

dfHV <- readRDS(file=file.path("process","out","HM_HV.rds"))
dfHV <- dfHV %>% filter(!is.na(HumanVirus))

# 3-panel graph, one panel for each indicator bacteria vs HV occurrence

dfHV$VirusOccur <- ifelse(dfHV$HumanVirus > 0.011,1,0)

#Sum of human markers
dfHV$hm <- dfHV$bacHum + dfHV$lachno2

response <- c("hm","eColi","ent")
response_names <- c("sHM (Copy Number/100mL)","E. coli (CFU/100mL)", "Enterococci (Copy Number/100mL)")
cutpoints <- list()
cutpoints[[1]] <- c(0,450,1000,10000,100000,3000000)
cutpoints[[2]] <- c(0,100,235,1000,10000,260000)
cutpoints[[3]] <- c(0,100,1000,10000,100000,2000000)

axis_text[[1]] <- parse(text=c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5"))
axis_text[[2]] <- parse(text=c("0-100","100-235","235-10^3","10^3-10^4"," 10^4"))
axis_text[[3]] <- parse(text=c("0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5"))

filenm <- "fig_3.pdf"
pdf(filenm)
par(mfcol = c(5,2),mar = c(1.5,0,0,3),oma = c(5,4,2,1))

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
  
  barplot(Occur$Occurrence,col ="grey",ylim = c(0.0,0.5))
  
  axis(side = 1,at = c(0.7,1.9,3.1,4.3,5.5),labels = axis_text[[i]])
  axis(side = 1,at = (5.3),labels = ">",tick = FALSE)
  mtext(side = 3,response_names[i],cex = 0.6,line = -3,font = 2)
  mtext(paste("n=", Occur$n, , sep = ""), at = c(0.7,1.9,3.1,4.3,5.5), line = 0, side = 3)
  
  
}

mtext(side = 1,text = "Bacteria Concentration",line = 3,cex=0.7)
mtext(side = 2,text = "Human Virus Occurrence Proportion",line = 2.5,outer = TRUE,adj = 1,cex = 0.7)

dev.off()
shell.exec(filenm)
