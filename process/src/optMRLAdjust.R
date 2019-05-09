#' optMRLAdjust
#'
#' Define function to adjust absorbance data for minimum reporting levels (MRLs).
#'
#' @param df dataframe with wavelength as column 1, and 
#' absorbance coefficients in the remaining columns
#' @param dfMRLs dataframe with one column to define wavelength and one column 
#' to define MRLs
#' @param Wavelength character string representing the name of the wavelength
#' column in dataframes df and optMRL.
#' @param sampleGRnums vector of lab ID numbers that represent the environmental
#' samples
#' @param multiplier The value to multiply the MRL by to set abs values (typically
#' 1.0, 0.5, or 0). The default is 1.0 to set values below the MRL to the MRL.
#' @examples 
#' @export 
#' @return 

optMRLAdjust <- function(df,dfMRLs,Wavelength,sampleGRnums,multiplier=1.0) {  

  df2 <- df[,c(Wavelength,sampleGRnums)]
  dfRemarks <- df[,c(Wavelength,sampleGRnums)]
  
  MRL <- dfMRLs[,"MRL"]
  
  for(i in 2:ncol(df2)){
    df2[which(df2[i] < MRL), i] <- MRL[which(df2[i] < MRL)]*multiplier
    dfRemarks[which(dfRemarks[i] < MRL), i] <- paste("<",MRL[which(dfRemarks[i] < MRL)])
  }
  
  return(list(df2=df2,dfRemarks=dfRemarks))
}

