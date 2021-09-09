# Gather data for SI spreadsheet
# One tab per spatial scale

library(tidyverse)

# GLRI data

filenm <- "glri_summary.rds"
df <- readRDS(file.path("process","out",filenm))

abbrev_short <- c("EE", "OC", "JI", "CL", "RO", "RM", "MA", "PO")
abbrevs <- c("Menominee","Manitowoc","Milwaukee","Clinton","Rouge","Raisin","Maumee","Portage")
siteID <- c("04067500", "04085427", "04087170", "04165500", "04166500", "04176500", "04193490", "04195500")
sites <- data.frame(abbrev = abbrev_short,Abbreviation = abbrevs,Site_ID = siteID)



df <- full_join(sites,df)
unique(df$abbrev)

keep_glri <- read.csv(file.path("process","in","glri_variables_for_SI.csv"))
cols_to_keep <- keep_glri$column_names[which(keep_glri$keep==1)]
cols_to_keep <- c("Site_ID","Abbreviation",cols_to_keep)
glri_df_for_SI <- df[,cols_to_keep]
write.csv(df[,cols_to_keep],file.path("process","out","glri_data_for_SI.csv"),row.names = FALSE)
openxlsx::write.xlsx(df[,cols_to_keep],
                     file.path("process","out","glri_data_for_SI.xlsx"))


# MMSD data
filenm <- "mmsd_summary.rds"
df <- readRDS(file.path("process","out",filenm))
abbrevs <- c("Underwood", "Wauwatosa", "16th", "Cedarburg","Bark")
abbrev_short <- c("UW","MW","MC","CG","BK")
siteID <- c("04087088", "04087120", "04087142", "04086600", "05426060")
sites <- data.frame(abbrev = abbrev_short,Abbreviation = abbrevs,Site_ID = siteID)
df <- full_join(sites,df)
#write.csv(data.frame(column_names = names(df)),file.path("process","out","mmsd_variables_for_SI_orig.csv"), row.names = FALSE)

keep <- read.csv(file.path("process","in","mmsd_variables_for_SI.csv"))
cols_to_keep <- keep$column_names[which(keep$keep==1)]
cols_to_keep <- c("Site_ID","Abbreviation",cols_to_keep)

df_for_SI <- df[,cols_to_keep]

#remove Honey, Donges, and Menomonee Falls. They have only 7, 6, and 6 observations respectively 
remove_sites <- c("HW","LD","MF")
df <- filter(df,!(abbrev %in% remove_sites))

write.csv(df[,cols_to_keep],file.path("process","out","mmsd_data_for_SI.csv"),row.names = FALSE)
openxlsx::write.xlsx(df[,cols_to_keep],
                     file.path("process","out","mmsd_data_for_SI.xlsx"))




#GLPF data
filenm <- "GLPF_summary.rds"
df <- readRDS(file.path("process","out","GLPF_summary.rds"))
#write.csv(data.frame(column_names = names(df)),file.path("process","out","glpf_variables_for_SI.csv"), row.names = FALSE)

keep <- read.csv(file.path("process","in","glpf_variables_for_SI.csv"))
cols_to_keep <- keep$column_names[which(keep$keep==1)]
cols_to_keep <- c("SiteID",cols_to_keep)
df_for_SI <- df[,cols_to_keep]

write.csv(df[,cols_to_keep],file.path("process","out","glpf_data_for_SI.csv"),row.names = FALSE)
<<<<<<< HEAD
=======

>>>>>>> 1a3a71200f24e2616aa4ccc4a9308843ffb0cbfb
openxlsx::write.xlsx(df[,cols_to_keep],
                     file.path("process","out","glpf_data_for_SI.xlsx"))
