
# GLRI
#
library(dplyr)

get_GLRI_blank_GRnums <- function(){
  
  #Look at tracking file
  tracking <- read.csv(file.path("raw","GLRI","MasterSampleLogCombinedOct102014.csv"),stringsAsFactors = FALSE)
  tracking$psdate <- as.POSIXct(tracking$Start.date.time..mm.dd.yy.hh.mm.,format="%m/%d/%Y %H:%M")
  tracking_QA <- filter(tracking,QA.sample > 0) %>%
    filter(UW.M.Bacteria > 0 & USGS.CA.Optics.DOC > 0)
  
  tracking_QA$QA.sample
 tracking$USGS.CA.Optics.DOC
  
  tracking_QA$Comments
  
  blank_rows <- grep("blank",tracking_QA$Comments,ignore.case = TRUE)
  blank_rows <- unique(blank_rows,which(tracking_QA$Blank>0))
  rep_rows <- grep("plicate",tracking_QA$Comments,ignore.case = TRUE)
  rep_rows <- unique(rep_rows,which(tracking_QA$Replicate>0))
  auto_rows <- grep("auto",tracking_QA$Comments,ignore.case = TRUE)
  RC_rows <- grep("control",tracking_QA$Comments,ignore.case = TRUE)
  test <- tracking_QA[which(tracking_QA$QA.sample>0),]
  test2 <- tracking_QA[unique(c(blank_rows,rep_rows,auto_rows,RC_rows)),]
  test2$Sample.ID %in% test$Sample.ID
  check <- test[!(test$Sample.ID %in% test2$Sample.ID),]
  
  
  #look at GLRI optical summary file
  
  df <- read.csv(file.path("raw","GLRI","GLRI_102714 data dump_vectorized_plus ABS vect Summary.csv"),stringsAsFactors = FALSE)
  df <- filter(df,!is.na(df$Tex275em340_result))
  
  sum(is.na(df$DOCResult)) #125 NA results for DOC are present when optical is reported
  sum(is.na(df$Tex275em340_result))
  sum(is.na(df$Aex260em450_result))
  sum(is.na(df$Mex300em390_result))
  sum(is.na(df$HI_Ohno2008))
  

  df$FieldExpID <- gsub(" ","",df$FieldExpID)
  #Define GRnumbers for blank samples in original summary file from CA
  blankRows1 <- grep("blank",df$QA, ignore.case = TRUE)
  blankRows2 <- grep("blank",df$SampleNotes, ignore.case = TRUE)
  blankRows3 <- grep("blank",df$FieldExpID,ignore.case = TRUE)
  
  blanks1 <- df[blankRows1,]
  blanks2 <- df[blankRows2,] #RM015 is not a blank
  blanks2 <- filter(blanks2,FieldExpID!="RM015")
  blanks3 <- df[blankRows3,]
  
  sum(!(blankRows2 %in% blankRows1)) #
  sum(!(blankRows1 %in% blankRows2)) #
  sum(!(blankRows3 %in% blankRows)) #
  sum(!(blankRows3 %in% blankRows2)) #
  
  blankRows <- c(blankRows1,blankRows2,blankRows3)
  
  df_blanks <- df[blankRows,]
  
  blankGRnums <- df[blankRows,"GRnumber"]
  blanks_from_MMSDOptSummary <- df[blankRows,]
  
  blank_sample_IDs_optSum <- gsub(" ","",blanks_from_MMSDOptSummary$FieldExpID)
  
  #Note: 2 blanks did not get reported from CA lab: CG 100 and MC 100. We will move ahead withou
  #these samples to determine MRLs. Three process blanks were not included in the tracking form.
  #We will not use these in the final MRL determination as they do not represent the entire
  #sampling process. Final blanks used are all determined from the CA summary file here:
  # file.path("raw","MMSD","PhaseIV","MMSDOptSummary.csv")
  
  #blank_sample_IDs[!(blank_sample_IDs %in% blank_sample_IDs_optSum)]
  process_blank_field_ids <- blank_sample_IDs_optSum[!(blank_sample_IDs_optSum %in% blank_sample_IDs)]
  which(!(blank_sample_IDs_optSum %in% blank_sample_IDs))
  
  remove_blanks <- which(blanks_from_MMSDOptSummary$FieldExpID %in% process_blank_field_ids)
  blankGRnums <- blanks_from_MMSDOptSummary[-remove_blanks,"GRnumber"]
  saveRDS(data.frame(GRnumbers = blankGRnums),file=file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
  
}
