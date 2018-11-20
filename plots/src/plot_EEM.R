plot_single_EEM <- function(EEMs, id, intensity.limits = c(0,5)){

  mat <- as.data.frame(as.table(EEMs[,,id])) %>%
    mutate(Ex = as.numeric(as.character(Var1)),
           Em = as.numeric(as.character(Var2)),
           ID = !! id) %>%
    rename(Intensity = Freq) %>%
    filter(!is.na(Intensity)) %>%
    filter(Intensity > 0) %>%
    select(-Var1, -Var2)
  
  Peaks <- USGSHydroOpt::ex_ems
  
  colPal <- c(colors()[30], colors()[143],colors()[554])
  
  heatPage <- ggplot(data = mat, aes(x=Ex, y=Em)) +
    geom_tile(aes(fill = Intensity)) +
    theme_bw() + 
    scale_fill_gradientn(colours=colPal,
                         na.value = "transparent",
                         limits= intensity.limits) +
    xlim(c(240,500)) +
    ylim(c(210, 600)) +
    coord_fixed(ratio=0.75) +
    geom_text(data = Peaks, aes(x = ExCA, y = EmCA, label = Peak))
  
  return(heatPage)
  
}