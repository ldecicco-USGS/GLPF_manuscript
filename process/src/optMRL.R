#' Define function to compute MRL
#' optMRL
#'
#' Determines minimum reporting levels (MRLs) for absorbance data using results 
#' of blanks samples: MRL = mean(blank results) + 3 * sd(blank results).
#'
#' @param df dataframe with wavelength as column 1, and 
#' absorbance coefficients in the remaining columns
#' @param Wavelength character string representing the name of the wavelength
#' column in dataframe df.
#' @param blankGRnums vector of lab ID numbers that represent the blank samples
#' to be used in determination of the MRLs
#' @examples 
#' @export 
#' @return 

optMRL <- function(df,Wavelength,blankGRnums) {

  dfBlankSummary <- df %>%
    tidyr::gather("sample","value",-!!Wavelength) %>%
    filter(sample %in% !!blankGRnums) %>%
    rename(Wavelength = !!Wavelength) %>% 
    filter(!is.na(value)) %>%
    group_by(Wavelength) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              min = min(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>%
    mutate(MRL = mean +3*sd,
           MRL = ifelse(mean<0, 3*sd, MRL))
  
  return(dfBlankSummary)
}


