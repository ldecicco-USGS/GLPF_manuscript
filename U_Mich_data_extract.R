# Data for U Mich
library(dplyr)

df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df_GLPF <- readRDS(file.path("process","out","glpf_summary.rds"))

df_Cl <- filter(df_GLPF,State == "MI")
write.csv(file="names_for_UMICH_.csv",names(df_Cl),row.names = FALSE)
df_vars <- read.csv(file="variables_for_UMICH_to_keep.csv",stringsAsFactors = FALSE)
df_vars <- df_vars[which(df_vars$keep==1),]

names(df_Cl)

df_Cl <- df_Cl[,df_vars$variable]

test <- df_Cl[which(df_Cl$SiteID == "00000000"),]
test_all <- df_GLPF[which(df_GLPF$SiteID == "00000000"),]
table(test_all$State)
table(df_GLPF$State)

fl_signals <- read.csv(file.path("raw","Optical summary definitions","ex_ems_means.csv"),stringsAsFactors = FALSE)
fl_signals_UMich <- fl_signals[which(fl_signals$Peak %in% names(df_Cl)),]
write.csv(fl_signals_UMich,file="fluorescence signal wavelengths.csv",row.names = FALSE)
