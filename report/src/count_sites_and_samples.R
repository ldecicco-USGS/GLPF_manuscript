#Count sites and samples

count_sites_and_samples <- function() {
  df <- readRDS(file.path("process","out","glpf_summary.rds"))
  
  df$site_type <- df$IfTributarySewerOutfallManholeorDitch
  unique(df$site_type)
  
  df[grep("Autosample",df$VirusAutosampleorSewerGrab),"site_type"] <- "Stream"
  df[grep("WW Influent",df$VirusAutosampleorSewerGrab),"site_type"] <- "WWTP"
  unique(df$site_type)
  
  unique(df$VirusAutosampleorSewerGrab)
  ww_rows <- which(df$VirusAutosampleorSewerGrab %in% c("Sanitary Grab", "WWTP", "WW Influent", "Sanitary"))
  
  df_ww <- df[ww_rows,]
  df <- df[-ww_rows,]
  
  autosample_rows <- grep("Autosample",df$VirusAutosampleorSewerGrab)
  
  df_auto <- df[-autosample_rows,]
  df_small <- df[-autosample_rows,]  
  
  unique(df_small$site_type)
  
  test <- df_small[grep("Tributary",df_small$site_type),]
  
  
  df %>%
    group_by(State) %>%
    summarize(count = length(unique(SiteID)))
  
  site_count <- df_small %>%
    group_by(State) %>%
    summarize(sites = length(unique(SiteID)))
  
  site_count <- rbind(site_count,c("All",length(unique(df_small$SiteID))       ))
  
  sample_count <- df_small %>%
    group_by(State) %>%
    summarize(samples = length(GRnumber))
  
  sample_count <- rbind(sample_count,c("All",length(df_small$GRnumber)       ))
  
  sites_and_samples <- left_join(site_count,sample_count)
  
  return (sites_and_samples)
}
