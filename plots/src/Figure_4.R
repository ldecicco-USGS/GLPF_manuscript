#Graph virus occurrence with bacteria prediction bins


library(tidyverse)
#Use model objects and extract predictions and virus occurrence from associated dataframes

#Start with MMSD
df_predictions <- full_join(mmsd_model_objects[[1]][[2]],mmsd_model_objects[[2]][[2]])

#Add in GLRI
for(i in 1:length(glri_model_objects)){
  df_add <- glri_model_objects[[i]][[2]]
  df_add <- rename(df_add,lachno2 = Lachno.2.cn.100ml, bacHum = BACHUM.cn.100mls)
  df_predictions <- full_join(df_predictions,glri_model_objects[[i]][[2]])
}

names(glri_model_objects[[i]][[2]])
names(df_predictions)

## Virus Occcurrence for HIB prediction bins

dfHV <- df_predictions %>% filter(!is.na(virus_occur))

responses <- c("lachno2","bacHum")
response_names <- c("Lachnospiraceae (Copy Number/100mL)","Human Bacteroides (CFU/100mL)")
cutpoints <- list()
cutpoints[[1]] <- c(0,450,1000,10000,100000,3000000)/2
cutpoints[[2]] <- c(0,450,1000,10000,100000,3000000)/2

cutpoints[[1]] <- c(0,1000,2500,10000,25000,1500000)
cutpoints[[2]] <- c(0,1000,2500,10000,25000,1500000)


dfHV$predictions_conc <- 10^dfHV$predictions

HVOccur <- data.frame()
for (i in 1:length(responses)) {
  df <- subset(dfHV,response == responses[i])
  df$category <- cut(df[,"predictions_conc"],cutpoints[[i]],cutpoints[[i]][-1])
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
}

df_response <- data.frame(response=response,full_name=response_names)
HVOccur <- left_join(HVOccur,df_response)
HVOccur$full_name <- factor(HVOccur$full_name, levels = response_names)

# Set up ordered factor for graphing
category_text <- c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5","0-10^2","10^2-235","235-10^3","10^3-10^4",
                   " 10^4","0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5")
category_text[category_text == " 10^5"] <- expression(""> 10^5)
category_levels <- c("0-10^2","0-450","10^2-235","10^2-10^3","235-10^3","450-10^3","10^3-10^4"," 10^4","10^4-10^5",expression(""> 10^5))
HVOccur$category_text <- factor(category_text,levels = category_levels)

HVOccur$category
bacteria_vs_virus <- ggplot(HVOccur,aes(x=category,y=Occurrence)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ full_name, scales = "free_x",nrow = 3,ncol = 1) +
  geom_text(
    aes(label=n),
    stat='identity',
    nudge_y=0.03,size = 3
  )+
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5,vjust=0.0,size = 6, )) +
  xlab("Predicted Indicator Bacteria Concentration") +
  ylab("Human Virus Occurrence Proportion") +
  scale_x_discrete(labels = function(l) parse(text=l))

bacteria_vs_virus

return(bacteria_vs_virus)
