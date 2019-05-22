# remove_old_optical_signals.R
# Remove the optical signals previously added to the bacteria and other data
# so that all optical variables can be added consistently for the three spatial scales
# including consideration of censored values.

remove_old_optical_signals <- function() {
  
  #GLRI
  df_glri <- readRDS(file.path("process","out","GLRISummaryWithTurbidity.rds"))
  begin_remove <- which(names(df_glri) == "OB1")
  end_remove <- which(names(df_glri) == "TimeZoneAdjustment")
  df_glri <- df_glri[,-c(begin_remove:end_remove)]
  
  #MMSD 
  df_mmsd <- readRDS(file.path("raw","MMSD","dfOptP3P4Combined.rds"))
  begin_remove <- which(names(df_mmsd) == "OB1")
  end_remove <- which(names(df_mmsd) == "logA488")
  df_mmsd <- df_mmsd[,-c(begin_remove:end_remove)]
  
  
  
  # GLPF
  df_glpf <- readRDS(file.path("raw","GLPF","summary_noQA.rds"))
  df_glpf <- df_glpf[,1:(which(names(df_glpf) == "OB1")-1)]
  df_glpf <- df_glpf %>% 
    rename(GRnumber=CAGRnumber) %>%
    filter(GRnumber %in% names(glpf_fl)[-1])
  
  saveRDS(df_glri,file = file.path("process","out","glri_summary_input.rds"))
  saveRDS(df_mmsd,file = file.path("process","out","mmsd_summary_input.rds"))
  saveRDS(df_glpf,file = file.path("process","out","glpf_summary_input.rds"))
  
}