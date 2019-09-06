library(tidyr)
library(dplyr)

#GLRI:
glri <- readRDS(file.path("process","out","glri_fl_MRL_adjusted.rds"))
glri_fl <- glri$df2
glri_abs <- readRDS(file.path("process","out","glri_abs_MRL_adjusted.rds"))
glri_abs <- glri_abs$df2
rm(glri)

# Let's pull all site info out, then have a similar "GRnumber", "NWISSiteID", date, then all the data
# Then site time could just be keyed off NWISSiteID

abs_long_glri <- gather(glri_abs, "GRnumber", "abs", -Wavelength)
fl_long_glri <- gather(glri_fl, "GRnumber", "fl", -exem)

rm(glri_abs, glri_fl)

dir.create("data_release", showWarnings = FALSE)
# data.table::fwrite(fl_long_glri, file = file.path("data_release","GLRI_EEMs.csv"))
# data.table::fwrite(abs_long_glri, file = file.path("data_release","GLRI_abs.csv"))
# data.table::fwrite(df_glri, file = file.path("data_release","GLRI_summary.csv"))

################################################

# MMSD:
mmsd <- readRDS(file.path("process","out","mmsd_fl_MRL_adjusted.rds"))
mmsd_fl <- mmsd$df2

mmsd_abs <- readRDS(file.path("process","out","mmsd_abs_MRL_adjusted.rds"))
mmsd_abs <- mmsd_abs$df2
rm(mmsd)

load(file.path("raw","MMSD","PhaseIII","MMSDabsEEMs.RData"))
rm(MMSD3DEEMs)

phaseIII <- names(dfabs)[-1][!(names(dfabs)[-1] %in% names(mmsd_abs)[-1])]
phaseIII <- phaseIII[phaseIII != "X"]

dfabs <- dfabs[,c("Wavelength", phaseIII)]
dfFluor <- dfFluor[,c("Wavelength.Pairs",phaseIII)]

phaseIV <- names(mmsd_abs)[-1][!(names(mmsd_abs)[-1] %in% names(dfabs)[-1])]

mmsd_abs <- mmsd_abs[,c("Wavelength", phaseIV)]
mmsd_fl <- mmsd_fl[,c("exem",phaseIV)]

mmsd_abs_total <- mmsd_abs %>%
  left_join(dfabs, by="Wavelength", )

mmsd_fl_total <- mmsd_fl %>%
  left_join(dfFluor, by=c("exem"="Wavelength.Pairs"))

abs_long_mmsd <- gather(mmsd_abs_total, "GRnumber", "abs", -Wavelength)
fl_long_mmsd <- gather(mmsd_fl_total, "GRnumber", "fl", -exem)

rm(mmsd_abs_total, mmsd_fl_total, phaseIV, mmsd_abs, mmsd_fl, dfFluor, dfabs, phaseIII)

################################################

# GLPF:
glpf <- readRDS(file.path("process","out","glpf_fl_MRL_adjusted.rds"))
glpf_fl <- glpf$df2

rm(glpf)

glpf_abs <- readRDS(file.path("process","out","glpf_abs_MRL_adjusted.rds"))
glpf_abs <- glpf_abs$df2

abs_long_glpf <- gather(glpf_abs, "GRnumber", "abs", -Wavelength)
fl_long_glpf <- gather(glpf_fl, "GRnumber", "fl", -exem)

rm(glpf_abs, glpf_fl)
gc()

abs_tots <- bind_rows(abs_long_glpf, abs_long_glri, abs_long_mmsd)

data.table::fwrite(abs_tots, file = file.path("data_release","absorption.csv"))
rm(abs_long_glpf, abs_long_glri, abs_long_mmsd, abs_tots)

fl_tots <- bind_rows(fl_long_glpf, fl_long_glri, fl_long_mmsd)
data.table::fwrite(fl_tots, file = file.path("data_release","EEMs.csv"))
rm(fl_long_glpf, fl_long_glri, fl_long_mmsd, fl_tots)

#######################################
# Summaries:
df_glri <- readRDS(file.path("process","out","glri_summary.rds"))
df_glpf <- readRDS(file.path("process","out","glpf_summary.rds"))
df_mmsd <- readRDS(file.path("process","out","mmsd_summary.rds"))
# df_glpf_QA <- readRDS(file.path("raw","GLPF","summary_QA.rds"))

# Remove ratio and logs:
df_glri <- df_glri[,1:which(names(df_glri) == "Aresid267" )]
df_glpf <- df_glpf[,1:which(names(df_glpf) == "Aresid267" )]
df_mmsd <- df_mmsd[,1:which(names(df_mmsd) == "Aresid267" )]

# Remove straight abs (we're including the full spectrum):
df_glri <- df_glri[,!grepl("A\\d{3}", names(df_glri))]
df_glpf <- df_glpf[,!grepl("A\\d{3}", names(df_glpf))]
df_mmsd <- df_mmsd[,!grepl("A\\d{3}", names(df_mmsd))]

df_glpf_opt_sigs <- df_glpf[,c(1,55:length(names(df_glpf)))]
df_glri_opt_sigs <- df_glri[,c(1,262:length(names(df_glri)))]
df_mmsd_opt_sigs <- df_mmsd[,c(1,28:length(names(df_mmsd)))]

df_tots <- bind_rows(df_glpf_opt_sigs, df_glri_opt_sigs, df_mmsd_opt_sigs)

data.table::fwrite(df_tots, file = file.path("data_release","summary_signals.csv"))


df_glpf_samples <- df_glpf[,c(1:54)]
df_glri_samples <- df_glri[,c(1:261)]
df_mmsd_samples <- df_mmsd[,c(1:27)]

glpf_samples <- df_glpf_samples %>%
  select(GRnumber, pdate, pedate, SiteID)

glri_samples <- df_glri_samples %>%
  select(GRnumber, pdate=psdate, pedate, SiteID=NWIS) %>%
  mutate(SiteID = dataRetrieval::zeroPad(SiteID, 8))

mmsd_samples <- df_mmsd_samples %>%
  select(GRnumber, pdate=psdate, pedate, SiteID=ProjectSiteID, abbrev)

mmsd_samples$SiteID[mmsd_samples$abbrev == "MC"] <- "04087142"
mmsd_samples$SiteID[mmsd_samples$abbrev == "BK"] <- "05426060"
mmsd_samples$SiteID[mmsd_samples$abbrev == "HW"] <- "04087119"
mmsd_samples$SiteID[mmsd_samples$abbrev == "CG"] <- "04086600"
mmsd_samples$SiteID[mmsd_samples$abbrev == "UW"] <- "04087088"
mmsd_samples$SiteID[mmsd_samples$abbrev == "MW"] <- "04087120"
mmsd_samples$SiteID[mmsd_samples$abbrev == "MF"] <- "04087030"
mmsd_samples$SiteID[mmsd_samples$abbrev == "LD"] <- "04087050"
mmsd_samples <- select(mmsd_samples, -abbrev)


glpf_samples$SiteID[glpf_samples$SiteID == "40871464"] <- "040871464"
glpf_samples$SiteID[glpf_samples$SiteID == "4087146"] <- "04087146"
glpf_samples$SiteID[glpf_samples$SiteID == "40871472"] <- "040871472"
glpf_samples$SiteID[glpf_samples$SiteID == "40871488"] <- "040871488"

samples_df <- bind_rows(mmsd_samples, 
                        glri_samples, 
                        glpf_samples)

sites_NWIS <- dataRetrieval::readNWISsite(unique(samples_df$SiteID))

data.table::fwrite(samples_df, file = file.path("data_release","summary_df.csv"))


