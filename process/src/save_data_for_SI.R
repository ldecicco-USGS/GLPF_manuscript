# Gather data for SI spreadsheet
# One tab per spatial scale

library(tidyverse)

# GLRI data

filenm <- "glri_summary.rds"
df <- readRDS(file.path("process","out",filenm))
keep_glri <- read.csv(file.path("process","in","glri_variables_for_SI.csv"))
glri_cols_to_keep <- keep_glri$column_names[which(keep_glri$keep==1)]
glri_df_for_SI <- df[,glri_cols_to_keep]
write.csv(df[,glri_cols_to_keep],file.path("process","out","glri_data_for_SI.csv"))



# MMSD data
filenm <- "mmsd_summary.rds"
df <- readRDS(file.path("process","out",filenm))

#write.csv(data.frame(col_names = names(df)),file.path("process","out","mmsd_variables_for_SI_orig.csv"), row.names = FALSE)

keep <- read.csv(file.path("process","in","mmsd_variables_for_SI.csv"))
cols_to_keep <- keep$column_names[which(keep$keep==1)]
df_for_SI <- df[,cols_to_keep]

#remove Honey, Donges, and Menomonee Falls. They have only 7, 6, and 6 observations respectively 
remove_sites <- c("HW","LD","MF")
df <- filter(df,!(abbrev %in% remove_sites))

write.csv(df[,cols_to_keep],file.path("process","out","mmsd_data_for_SI.csv"))

#GLPF data
filenm <- "GLPF_summary.rds"
df <- readRDS(file.path("process","in","GLPF_summary.rds"))
#write.csv(data.frame(col_names = names(df)),file.path("process","out","glpf_variables_for_SI.csv"), row.names = FALSE)

keep <- read.csv(file.path("process","out","glpf_variables_for_SI.csv"))
cols_to_keep <- keep$column_names[which(keep$keep==1)]
df_for_SI <- df[,cols_to_keep]

write.csv(df[,cols_to_keep],file.path("process","out","glpf_data_for_SI.csv"))
