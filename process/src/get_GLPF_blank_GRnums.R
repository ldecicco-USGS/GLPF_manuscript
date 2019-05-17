
# GLPF: identify blank GR numbers. Much of the work was done when separating
# QA data in the original GLPF munging on the repo that Laura developed for GLPF modeling.
#
library(dplyr)

get_GLPF_blank_GRnums <- function(){
  
  #Use GLPF summary file for defining blanks
  df <- readRDS(file.path("raw","GLPF","summary_QA.rds"))
  
  df$FieldExpID <- gsub(" ","",df$FieldID)
  #Define GRnumbers for blank samples in original summary file from CA
  blankRows1 <- grep("blank",df$Comments, ignore.case = TRUE)
  blankRows2 <- grep("2",df$SampleType9regular2blank7replicate, ignore.case = TRUE)

  blanks1 <- df[blankRows1,]
  blanks2 <- df[blankRows2,] #RM015 is not a blank
  
  sum(!(blankRows2 %in% blankRows1)) #
  sum(!(blankRows1 %in% blankRows2)) #

  blankRows <- unique(c(blankRows1,blankRows2))
  
  df_blanks <- df[blankRows,]
  
  blankGRnums <- df_blanks[,"CAGRnumber"]
  saveRDS(data.frame(GRnumbers = blankGRnums),file=file.path("process","out","GLPF_blank_GRnumbers.rds"))
  
}
