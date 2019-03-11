

merge_p3_p4_vectorized <- function() {
  #MMSD
  load(file.path("raw","MMSD","PhaseIV","MMSDOpticalData.RData"))
  abs_pIV <- dfabs
  fl_pIV <- dffl
  names(abs_pIV)[1] <- "Wavelength"
  row.names(abs_pIV) <- abs_pIV$Wavelength
  names(fl_pIV) <- substr(names(fl_pIV),1,7)
  names(fl_pIV)[1] <- "exem"
  
  
  #MMSD
  load(file.path("raw","MMSD","PhaseIII","MMSDabsEEMs.RData"))
  abs_pIII <- dfabs
  fl_pIII <- dfFluor
  names(abs_pIII)[1] <- "Wavelength"
  row.names(abs_pIII) <- abs_pIV$Wavelength
  names(fl_pIII) <- substr(names(fl_pIII),1,7)
  names(fl_pIII)[1] <- "exem"
  
  
  abs_pIII_unique <- abs_pIII[,-(which(names(abs_pIII)[-1] %in% names(abs_pIV))+1)]
  fl_pIII_unique <- fl_pIII[,-(which(names(fl_pIII)[-1] %in% names(fl_pIV))+1)]
  
  mmsd_abs <- merge(abs_pIII_unique,abs_pIV,by="Wavelength")
  mmsd_fl <- merge(fl_pIII_unique,fl_pIV,by="exem")
  
  save(mmsd_abs,mmsd_fl,file=file.path("process","out","mmsd_optical_Dec_2018_no_MRLs.RData"))
  
}