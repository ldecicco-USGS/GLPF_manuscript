#Graph virus occurrence with bacteria prediction bins


library(tidyverse)
#Use model objects and extract predictions and virus occurrence from associated dataframes

load(file = file.path("model","out","final_model_objects.RData"))
mmsd_model_objects <- final_model_list[[1]]
glri_model_objects <- final_model_list[[2]]

#Start with MMSD
df_predictions <- full_join(mmsd_model_objects[[1]][[2]],mmsd_model_objects[[2]][[2]])

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
cutpoints[[1]] <- c(0,1000,2500,10000,25000,1500000)*2
cutpoints[[1]] <- c(0,1000,2000,4000,6000,8000,10000,15000,20000,35000,50000,3000000)
cutpoints[[1]] <- c(0,1000,5000,7500,10000,15000,20000,35000,50000,3000000)

cutpoints[[1]] <- c(0,1000,5000,7500,15000,20000,35000,50000,3000000)
cutpoints[[1]] <- c(0,1000,5000,10000,20000,35000,50000,3000000)
cutpoints[[1]] <- c(0,1500,6000,12000,20000,35000,50000,3000000)
cutpoints[[1]] <- c(0,1700,5000,10000,20000,35000,50000,3000000)



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

#df_response <- data.frame(response=response,full_name=response_names)
# HVOccur <- left_join(HVOccur,df_response)
# HVOccur$full_name <- factor(HVOccur$full_name, levels = response_names)

# Set up ordered factor for graphing
category_text <- c("0-1000","450-10^3","10^3-10^4","10^4-10^5"," 10^5","0-10^2","10^2-235","235-10^3","10^3-10^4",
                   " 10^4","0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5")
category_text[category_text == " 10^5"] <- expression(""> 10^5)
category_levels <- c("0-10^2","0-450","10^2-235","10^2-10^3","235-10^3","450-10^3","10^3-10^4"," 10^4","10^4-10^5",expression(""> 10^5))
HVOccur$category_text <- factor(category_text,levels = category_levels)

HVOccur$category
bacteria_vs_virus <- ggplot(HVOccur,aes(x=category,y=Occurrence)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label=n),
    stat='identity',
    nudge_y=0.03,size = 3
  )+
  theme(axis.text.x=element_text(angle = 0, hjust = 0.5,vjust=0.0,size = 6, )) +
  xlab("Predicted Indicator Bacteria Concentration") +
  ylab("Human Virus Occurrence Proportion") 
#  scale_x_discrete(labels = function(l) parse(text=l))

bacteria_vs_virus


# CDF

dfsHM <- arrange(dfsHM,sHM)
cumulative_Occur <- numeric()
for(i in 1:dim(dfsHM)[1]) {
  cumulative_Occur <- c(cumulative_Occur,mean(dfsHM[(1:i),"virus_occur"]))
}

dfsHM$sHM_obs <- dfsHM$lachno2 + dfsHM$bacHum
dfsHM$sHM_obs <- ifelse(dfsHM$sHM_obs < 450,450,dfsHM$sHM_obs)
plot(dfsHM$sHM,cumulative_Occur,log="x",pch = "",cex=0.7)

l <- loess(cumulative_Occur ~ dfsHM$sHM,span = 0.4)
summary(l)
lines(dfsHM$sHM,predict(l),col="green")

lines(lowess(x=dfsHM$sHM,y=cumulative_Occur,f = 0.2))


dfsHM <- arrange(dfsHM,sHM_obs)
cumulative_Occur <- numeric()
for(i in 1:dim(dfsHM)[1]) {
  cumulative_Occur <- c(cumulative_Occur,mean(dfsHM[(1:i),"virus_occur"]))
}
#points(dfsHM$sHM_obs,cumulative_Occur,pch=20,cex=0.7,col="blue")
#lines(lowess(x=dfsHM$sHM_obs,y=cumulative_Occur,f = 0.2))
l <- loess(cumulative_Occur ~ dfsHM$sHM_obs,span = 0.6)
summary(l)
lines(dfsHM$sHM_obs,predict(l),col="blue")


plot(dfsHM$sHM_obs,dfsHM$sHM,log="xy")

plot(dfsHM$sHM_obs,cumulative_Occur,pch=20,cex=0.7,col="blue",log="x")


# X-point average distribution curve
ave_window <- 40/2
begin <- ave_window
end <- dim(dfsHM)[1] - ave_window

#Predicted values: compute means
mean_prediction <- numeric()
mean_occur_pred <- numeric()
dfsHM <- arrange(dfsHM,sHM)
for(i in begin:end) {
  mean_prediction <- c(mean_prediction,mean(dfsHM[((i- ave_window):(i+ ave_window)),"sHM"]))
  mean_occur_pred <- c(mean_occur_pred,mean(dfsHM[((i- ave_window):(i+ ave_window)),"virus_occur"]))
}

plot(mean_prediction,mean_occur_pred,type = "l",log="x",col = "blue")


#Observed values: compute means
mean_obs <- numeric()
mean_occur_obs <- numeric()
dfsHM <- arrange(dfsHM,sHM_obs)
for(i in begin:end) {
  mean_obs <- c(mean_obs,mean(dfsHM[((i- ave_window):(i+ ave_window)),"sHM_obs"]))
  mean_occur_obs <- c(mean_occur_obs,mean(dfsHM[((i- ave_window):(i+ ave_window)),"virus_occur"]))
}

lines(mean_obs,mean_occur_obs,type = "l",col="orange")



### Individual HIB
# 
# responses <- c("lachno2","bacHum")
# response_names <- c("Lachnospiraceae (Copy Number/100mL)","Human Bacteroides (Copy Number/100mL)")
# cutpoints <- list()
# cutpoints[[1]] <- c(0,1000,2500,10000,25000,1500000)
# cutpoints[[2]] <- c(0,1000,2500,10000,25000,1500000)

# dfHV$predictions_conc <- 10^dfHV$predictions
# 
# HVOccur <- data.frame()
# for (i in 1:length(responses)) {
#   df <- subset(dfHV,response == responses[i])
#   df$category <- cut(df[,"predictions_conc"],cutpoints[[i]],cutpoints[[i]][-1])
#   Cats <- table(df$category)
#   
#   Occur <- df %>%
#     group_by(category) %>%
#     summarise(Occurrence = mean(virus_occur),
#               n = length(virus_occur))
#   Occur$response <- responses[i]
#   Occur$category <- as.character(Occur$category)
#   Occur[dim(Occur)[1],"category"] <- paste0("<",Occur[dim(Occur)[1],"category"])
#   Occur$category <- factor(Occur$category,levels = Occur$category)
#   
#   HVOccur <- rbind(HVOccur,Occur)
# }
# 
# df_response <- data.frame(response=response,full_name=response_names)
# HVOccur <- left_join(HVOccur,df_response)
# HVOccur$full_name <- factor(HVOccur$full_name, levels = response_names)
# 
# # Set up ordered factor for graphing
# category_text <- c("0-450","450-10^3","10^3-10^4","10^4-10^5"," 10^5","0-10^2","10^2-235","235-10^3","10^3-10^4",
#                    " 10^4","0-10^2","10^2-10^3","10^3-10^4","10^4-10^5"," 10^5")
# category_text[category_text == " 10^5"] <- expression(""> 10^5)
# category_levels <- c("0-10^2","0-450","10^2-235","10^2-10^3","235-10^3","450-10^3","10^3-10^4"," 10^4","10^4-10^5",expression(""> 10^5))
# HVOccur$category_text <- factor(category_text,levels = category_levels)
# 
# HVOccur$category
# bacteria_vs_virus <- ggplot(HVOccur,aes(x=category,y=Occurrence)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(. ~ full_name, scales = "free_x",nrow = 3,ncol = 1) +
#   geom_text(
#     aes(label=n),
#     stat='identity',
#     nudge_y=0.03,size = 3
#   )+
#   theme(axis.text.x=element_text(angle = 0, hjust = 0.5,vjust=0.0,size = 6, )) +
#   xlab("Predicted Indicator Bacteria Concentration") +
#   ylab("Human Virus Occurrence Proportion") +
#   scale_x_discrete(labels = function(l) parse(text=l))
# 
# bacteria_vs_virus


return(bacteria_vs_virus)
