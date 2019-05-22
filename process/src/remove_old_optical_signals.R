# remove_old_optical_signals.R
# Remove the optical signals previously added to the bacteria and other data
# so that all optical variables can be added consistently for the three spatial scales
# including consideration of censored values.

remove_old_optical_signals <- function() {
  library(dplyr)
  
  #GLRI
  df_glri <- readRDS(file.path("process","out","GLRISummaryWithTurbidity.rds"))
  begin_remove <- which(names(df_glri) == "OB1")
  end_remove <- which(names(df_glri) == "TimeZoneAdjustment")
  df_glri <- df_glri[,-c(begin_remove:end_remove)]
  df_glri <- dplyr::filter(df_glri, !is.na(FT.))
  
  glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
  glri_GRnumbers <- dplyr::filter(glri_GRnumbers, GRnumbers != "gr13755")
  df_glri <- dplyr::filter(df_glri, !(GRnumber %in% glri_GRnumbers$GRnumbers))
  
  #MMSD 
  df_mmsd <- readRDS(file.path("raw","MMSD","dfOptP3P4Combined.rds"))
  begin_remove <- which(names(df_mmsd) == "OB1")
  end_remove <- which(names(df_mmsd) == "logA488")
  df_mmsd <- df_mmsd[,-c(begin_remove:end_remove)]
  mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
  df_mmsd <- dplyr::filter(df_mmsd, !(GRnumber %in% mmsd_GRnumbers$GRnumbers))
  
  # GLPF
  glpf <- readRDS(file.path("process","out","glpf_fl_MRL_adjusted.rds"))
  glpf_fl <- glpf$df2
  
  glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))
  glpf_GRnumbers$GRnumbers <- as.character(glpf_GRnumbers$GRnumbers)
  
  df_glpf <- readRDS(file.path("raw","GLPF","summary_noQA.rds"))
  df_glpf <- df_glpf[,1:(which(names(df_glpf) == "OB1")-1)]
  df_glpf <- df_glpf %>% 
    rename(GRnumber=CAGRnumber) %>%
    filter(GRnumber %in% names(glpf_fl)[-1])
  
  saveRDS(df_glri,file = file.path("process","out","glri_summary_input.rds"))
  saveRDS(df_mmsd,file = file.path("process","out","mmsd_summary_input.rds"))
  saveRDS(df_glpf,file = file.path("process","out","glpf_summary_input.rds"))
  
}