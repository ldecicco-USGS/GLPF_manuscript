
# First compare blanks recorded in Sample tracking.csv with blanks reported from 
# CA lab in VirusPhaseIVData.Rdata (df). Ultimately the CA lab reported results were used, but the
# process blanks from this file were not used to determine MRLs because they do not represent
# the entire sampling process.
#
get_MMSD_blank_GRnums <- function(){
  
  #Look at tracking file
  tracking <- read.csv(file.path("raw","MMSD","PhaseIV","Sample tracking.csv"),stringsAsFactors = FALSE)
  
  blank_rows <- grep("blank",tracking$Comments,ignore.case = TRUE)                        
  rep_rows <- grep("plicate",tracking$Comments,ignore.case = TRUE)
  
  tracking$qa_type <- NA
  tracking[blank_rows,"qa_type"] <- "blank"
  tracking[rep_rows,"qa_type"] <- "replicate"
  blanks <- tracking[blank_rows,]
  qa <- tracking[c(blank_rows,rep_rows),]
  
  blank_sample_IDs <- gsub(" ","",blanks$Sample.ID)
  
  #Look at CA summary reporting file
  load(file.path("raw","MMSD","PhaseIV","VirusPhaseIVData.Rdata"))
  
  Opt_sum_previous <- df
  
  df <- read.csv(file.path("raw","MMSD","PhaseIV","MMSDOptSummary.csv"),skip=1,stringsAsFactors = FALSE)
  df$FieldExpID <- gsub(" ","",df$FieldExpID)
  #Define GRnumbers for blank samples in original summary file from CA
  blankRows <- grep("blank",df$QA, ignore.case = TRUE)
  blankRows2 <- grep("blank",df$SampleNotes, ignore.case = TRUE)
  sum(!(blankRows2 %in% blankRows)) #confirmed that all blanks are represented in blankRows
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