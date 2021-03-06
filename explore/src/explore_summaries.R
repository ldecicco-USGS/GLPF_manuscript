library(dplyr)
library(tidyr)

abs_signals <- function(){
  abs_signals <- data.table::fread(file.path("raw","Optical summary definitions","abs_wavs.csv"))
  abs_signals_glpf <- data.table::fread(file.path("raw","Optical summary definitions","abs_wavsCA.csv"))
  abs_signals_all <- bind_rows(mutate(abs_signals, study = "MMSD"),
                               mutate(abs_signals, study = "GLRI"),
                               mutate(abs_signals_glpf, study = "GLPF"))
  return(abs_signals_all)
  
}

fl_signals <- function(){
  
  fl_signals <- data.table::fread(file.path("raw","Optical summary definitions","ex_ems_means.csv"), na.strings = "")
  fl_signals_glpf <- data.table::fread(file.path("raw","Optical summary definitions","ex_ems_meansCA.csv"), na.strings = "")
  
  fl_signals_all <- bind_rows(mutate(fl_signals, study="MMSD"),
                              mutate(fl_signals, study="GLRI"),
                              mutate(fl_signals_glpf, study="GLPF"))
  
  fl_signals_all$Em2[which(fl_signals_all$Em1 == fl_signals_all$Em2)] <- NA
  
  return(fl_signals_all)
}

em_peaks <- function(fl_signals_all, fl_sum){
  
  fl_peaks <- fl_signals_all %>%
    filter(is.na(Ex2) & is.na(Em2)) %>%
    select(Peak, x=Ex1, y=Em1, Source, study)
  
  fl_peaks_long <- data.frame()
  
  nearest <- data.frame()

  for(i in unique(fl_peaks$study)){
    signals <- filter(fl_peaks, study == i)
    
    sig_vals <- filter(fl_sum, study == i)
    vals_geo <- sf::st_multipoint(as.matrix(sig_vals[,c("x","y")]))
    
    study_coords <- data.frame()
    for(j in 1:nrow(signals)){
      point_j <- sf::st_point(as.matrix(signals[j,c("x","y")]))
      nearest_1 <- sf::st_nearest_points(point_j, vals_geo)
      
      binder <- data.frame(sf::st_coordinates(sf::st_cast(nearest_1, "POINT")[2]))
      binder <- bind_cols(binder, signals[j,!(names(signals) %in% c("x","y"))])
      study_coords <- bind_rows(study_coords, binder)
    }
    
    study_coords <- study_coords %>%
      rename(x=X,y=Y) %>%
      left_join(sig_vals, by=c("x","y","study"))
    
    nearest <- bind_rows(nearest, study_coords)
  }
  
  return(nearest)

}

em_long <- function(fl_signals_all, fl_sum){
  
  em_bands <- fl_signals_all %>%
    filter(is.na(Ex2) & !is.na(Em2)) %>%
    select(-Ex2) %>%
    rename(x = Ex1, ymin = Em1, ymax = Em2)
  
  em_bands_long <- data.frame()
  
  for(i in 1:nrow(em_bands)){
    
    study_data <- fl_sum %>%
      filter(study == em_bands$study[i])
    
    # find closest x:
    if(em_bands$x[i] %in% unique(study_data$x)){
      study_data <- study_data %>%
        filter(x == em_bands$x[i])
    } else {
      # this just gets the closest x...
      # it ignores if there's a further away x with more matching y's....
      # might consider using sf again with line segments
      study_data <- study_data %>%
        filter(abs(x - em_bands$x[i]) == min(abs(x - em_bands$x[i])))
    }
    
    stuff = study_data %>%
        filter(y >= em_bands$ymin[i],
               y <= em_bands$ymax[i]) %>%
      mutate(Peak = em_bands$Peak[i],
             Source = em_bands$Source[i])
    
    em_bands_long <- bind_rows(em_bands_long, stuff)
    
  }
  return(em_bands_long)
}

eems_boxes <- function(fl_signals_all, fl_sum){
  
  eems_boxes <- fl_signals_all %>%
    filter(!is.na(Em2) & !is.na(Ex2)) 
  
  eems_boxes_long <- data.frame()
  
  for(i in 1:nrow(eems_boxes)){
    from_y <- eems_boxes$Em1[i]
    to_y <- eems_boxes$Em2[i]
    from_x <- eems_boxes$Ex1[i]
    to_x <- eems_boxes$Ex2[i]
    
    y = fl_sum %>%
      filter(study == eems_boxes$study[i], 
             x >= from_x & x <= to_x,
             y >= from_y & y <= to_y,) %>%
      mutate(Peak = eems_boxes$Peak[i],
             Source = eems_boxes$Source[i])
    
    eems_boxes_long <- bind_rows(eems_boxes_long, y)
  }
  return(eems_boxes_long)
} 

combine_remarks <- function(glri, mmsd, glpf, wavelength = "exem"){
  
  rmks_glri <- glri$dfRemarks %>%
    tidyr::gather("sample","censored",-!!wavelength) %>%
    mutate(censored = substr(censored, 1, 1) == "<") %>%
    filter(!is.na(censored)) %>%
    mutate(study = "GLRI")
  
  rmks_mmsd <- mmsd$dfRemarks %>%
    tidyr::gather("sample","censored",-!!wavelength) %>%
    mutate(censored = substr(censored, 1, 1) == "<") %>%
    filter(!is.na(censored)) %>%
    mutate(study = "MMSD")
  
  rmks_glpf <- glpf$dfRemarks %>%
    tidyr::gather("sample","censored",-!!wavelength) %>%
    mutate(censored = substr(censored, 1, 1) == "<") %>%
    filter(!is.na(censored)) %>%
    mutate(study = "GLPF")
  
  rmks <- bind_rows(rmks_glri,
                       rmks_mmsd,
                       rmks_glpf) %>%
    group_by(!!sym(wavelength), study) %>%
    summarize(total = n(),
              censored = sum(censored),
              freq = (total-censored)/total)
  
  return(rmks)
  
}

combine_values <- function(glri, mmsd, glpf, wavelength = "exem"){
  df_glri <- glri$df2 %>%
    tidyr::gather("sample","value",-!!wavelength) %>%
    filter(!is.na(value)) %>%
    mutate(study = "GLRI")
  
  df_mmsd <- mmsd$df2 %>%
    tidyr::gather("sample","value",-!!wavelength) %>%
    filter(!is.na(value)) %>%
    mutate(study = "MMSD")
  
  df_glpf <- glpf$df2 %>%
    tidyr::gather("sample","value",-!!wavelength) %>%
    filter(!is.na(value)) %>%
    mutate(study = "GLPF")
  
  df_fl <- bind_rows(df_glri,
                     df_mmsd,
                     df_glpf) %>%
    group_by(!!sym(wavelength), study) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              min = min(value, na.rm = TRUE))
  
  return(df_fl)
}

abs_peaks <- function(abs_signals_all, abs_sum){

  abs_signals_all <- rename(abs_signals_all, Wavelength=abs_wav)

  abs_peaks_long <- abs_signals_all %>%
    left_join(abs_sum, by = c("Wavelength","study"))
  
  return(abs_peaks_long)
  
}

print_fl_pages <- function(filename, fl_df, 
                           wavelength = "exem",
                           color_lim = c(-0.2,0.2),
                           npages = NA,
                           filter_min = NA,
                           filter_max = NA){
  
  fl_long <- long_df(fl_df, study = "") %>%
    select(-study)
  
  if(!is.na(filter_max) & !is.na(filter_min)){
    fl_long <- filter(fl_long, value > filter_min & value < filter_max)
  }
  
  if(is.na(npages)){
    npages <- ceiling(length(unique(fl_long$sample))/9)
  }
  
  pdf(file.path("explore","out",filename))
  for(i in 1:npages){
    print(
      ggplot(fl_long) +
        geom_point(aes(x = x, y = y, color = value)) +
        theme_bw() +
        scale_colour_gradient2(limits = color_lim) +
        ggforce::facet_wrap_paginate(~ sample, ncol = 3, nrow = 3, page = i)
    )
  }
  dev.off()
  
}


long_df <- function(df, study, wavelength = "exem"){
  
  df_long <- split_exem(df, wavelength = wavelength) %>%
    gather("sample","value",-x,-y) %>%
    filter(!is.na(value)) %>%
    mutate(study = !!study)
  
  return(df_long)
}

split_exem <- function(df, wavelength = "exem"){
  
  df_split <- df %>%
    tidyr::separate(!!wavelength, into = c("x","y"),sep = "/") %>%
    mutate(x = as.numeric(x),
           y = as.numeric(y))
  return(df_split)
}
