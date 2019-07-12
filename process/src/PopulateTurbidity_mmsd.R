# Add MMSD continuous water quality data to summary dataframe

library(dplyr)

Populate_turbidity_mmsd <- function(){
  df_MMSD <- readRDS(file.path("raw","MMSD","dfOptP3P4Combined.rds"))
  
  df_MMSD_CQW <- readRDS(file.path("raw","mmsd","dfOptP3P4CombinedContWQ.rds"))
  
  df_cqw <- df_MMSD_CQW[,c("GRnumber","Turbidity_mean")]
  
  df_mmsd <- full_join(df_MMSD,df_cqw,by="GRnumber")
  
  sum(!is.na(df_mmsd$Turbidity_mean))
  
  saveRDS(df_mmsd,file=file.path("process","out","dfOptP3P4Combined_w_turb.rds"))
}