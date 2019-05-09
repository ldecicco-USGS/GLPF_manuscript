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

  join_by <- "Wavelength"
  names(join_by) <- Wavelength
  MRL <- dfMRLs[,c("Wavelength","MRL")]
  
  df <- dplyr::left_join(df, MRL, by = join_by)
  
  df2 <- df[,c(Wavelength,sampleGRnums,"MRL")]
  dfRemarks <- df[,c(Wavelength,sampleGRnums,"MRL")]
  
  for(i in 2:ncol(df2)){
    df2[which(df2[[i]] < df2$MRL), i] <- df2$MRL[which(df2[[i]] < df2$MRL)]*multiplier
    dfRemarks[which(dfRemarks[[i]] < dfRemarks$MRL), i] <- paste("<",dfRemarks$MRL[which(dfRemarks[[i]] < dfRemarks$MRL)])
  }
  
  return(list(df2=df2,dfRemarks=dfRemarks))
}

