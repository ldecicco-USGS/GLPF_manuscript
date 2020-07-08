#Graph virus occurrence with bacteria prediction bins


#Use model objects and extract predictions and virus occurrence from associated dataframes

#MMSD


df_predictions <- full_join(mmsd_model_objects[[1]][[2]],mmsd_model_objects[[2]][[2]])

for(i in 1:length(glri_model_objects)){
  df_add <- glri_model_objects[[i]][[2]]
  df_add <- rename(df_add,lachno2 = Lachno.2.cn.100ml, bacHum = BACHUM.cn.100mls)
  df_predictions <- full_join(df_predictions,glri_model_objects[[i]][[2]])
}

names(glri_model_objects[[i]][[2]])
names(df_predictions)

