library(ggplot2)
library(ggforce)
library(readxl)

source(file.path("explore","src","explore_summaries.R"))

#FL:

#MMSD
load(file.path("raw","MMSD","PhaseIV","MMSDOpticalData.RData"))
mmsd_fl <- data.frame(dffl)
names(mmsd_fl) <- substr(names(mmsd_fl),1,7)
names(mmsd_fl)[1] <- "exem"
rm(MMSDP43DEEMs, dfabs, dffl)

#GLRI
glri_fl <- read_xlsx(file.path("raw","GLRI","GLRI_CompleteData_091913.xlsx"),sheet = "Vectorized Flourescence_Corr", skip = 1)
names(glri_fl)[1] <- "exem"

#GLPF
glpf_fl <- readRDS(file.path("raw","GLPF","optics","dffl_QA.rds"))

mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
mmsd_GRnumbers$GRnumbers <- as.character(mmsd_GRnumbers$GRnumbers)
glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
glri_GRnumbers$GRnumbers <- as.character(glri_GRnumbers$GRnumbers)
glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))
glpf_GRnumbers$GRnumbers <- as.character(glpf_GRnumbers$GRnumbers)

mmsd_blank_cols <- which(names(mmsd_fl) %in% mmsd_GRnumbers$GRnumbers)
glri_blank_cols <- which(names(glri_fl) %in% glri_GRnumbers$GRnumbers)
glpf_blank_cols <- which(names(glpf_fl) %in% glpf_GRnumbers$GRnumbers)

mmsd_blanks_fl <- mmsd_fl[,c(1,mmsd_blank_cols)]
glri_blanks_fl <- glri_fl[,c(1,glri_blank_cols)]
glpf_blanks_fl <- glpf_fl[,c(1,glpf_blank_cols)]

rm(mmsd_fl, glri_fl, glpf_fl, 
   mmsd_blank_cols, glri_blank_cols, glpf_blank_cols,
   glpf_GRnumbers, glri_GRnumbers, mmsd_GRnumbers)

############################################

# Fluorescence:
print_fl_pages("GLPF_blankEEMs.pdf", glpf_blanks_fl)
print_fl_pages("MMSD_blankEEMs.pdf", mmsd_blanks_fl)
print_fl_pages("GLRI_blankEEMs.pdf", glri_blanks_fl)

glri_long <- long_df(glri_blanks_fl, "GLRI")
mmsd_long <- long_df(mmsd_blanks_fl, "MMSD")
glpf_long <- long_df(glpf_blanks_fl, "GLPF")

fl_df <- bind_rows(glri_long,
                   mmsd_long,
                   glpf_long)

fl_df_summary <- fl_df %>%
  group_by(x, y, study) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

mean_blanks <- ggplot(fl_df_summary) +
  geom_point(aes(x = x, y = y, color = mean)) +
  theme_bw() +
  scale_colour_gradient2(limits = c(-0.2,0.2)) +
  facet_grid(. ~ study)

ggsave(mean_blanks, filename = file.path("explore","out","mean_blanks.pdf"))

fl_signals_all <- fl_signals()

peaks_blanks <- em_peaks(fl_signals_all, fl_df_summary)
lines_blanks <- em_long(fl_signals_all, fl_df_summary)
boxes_blanks <- eems_boxes(fl_signals_all, fl_df_summary)

blank_signals <- bind_rows(peaks_blanks, lines_blanks, boxes_blanks)

summary_blank_signals <- blank_signals %>%
  group_by(Peak, study ) %>%
  summarize(mean = mean(mean, na.rm = TRUE),
            min = min(min, na.rm = TRUE),
            max = max(max, na.rm = TRUE))

data.table::fwrite(summary_blank_signals, file = file.path("explore","out","summary_blanks_fl.csv"))

ggplot(summary_blank_signals) +
  geom_point(aes(x=study, y=mean)) +
  facet_wrap(. ~ Peak)

####################
#Absorbance:
source(file.path("process","src","explore_summaries.R"))

abs_signals_all <- abs_signals()

load(file.path("raw","MMSD","PhaseIV","MMSDOpticalData.RData"))
mmsd_abs <- data.frame(dfabs)
names(mmsd_abs)[1] <- "Wavelength"
rm(dffl, mmsd_fl, dfabs, 
   MMSDP43DEEMs)

glri_abs <- read.csv(file.path("raw","GLRI","compiled_absorbance_Corr_May2014.csv"),stringsAsFactors = FALSE)
names(glri_abs)[1] <- "Wavelength"
row.names(glri_abs) <- glri_abs$Wavelength

glpf_abs <- readRDS(file.path("raw","GLPF","optics","dfabs_QA.rds"))
names(glpf_abs)[1] <- "Wavelength"
row.names(glpf_abs) <- glpf_abs$Wavelength

mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
mmsd_GRnumbers$GRnumbers <- as.character(mmsd_GRnumbers$GRnumbers)
glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
glri_GRnumbers$GRnumbers <- as.character(glri_GRnumbers$GRnumbers)
glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))
glpf_GRnumbers$GRnumbers <- as.character(glpf_GRnumbers$GRnumbers)

mmsd_blank_cols <- which(names(mmsd_abs) %in% mmsd_GRnumbers$GRnumbers)
glri_blank_cols <- which(names(glri_abs) %in% glri_GRnumbers$GRnumbers)
glpf_blank_cols <- which(names(glpf_abs) %in% glpf_GRnumbers$GRnumbers)

mmsd_blanks_abs <- mmsd_abs[,c(1,mmsd_blank_cols)]
glri_blanks_abs <- glri_abs[,c(1,glri_blank_cols)]
glpf_blanks_abs <- glpf_abs[,c(1,glpf_blank_cols)]

rm(glpf_abs, glri_abs, mmsd_abs)
rm(mmsd_blank_cols, glri_blank_cols, glpf_blank_cols,
   glpf_GRnumbers, glri_GRnumbers, mmsd_GRnumbers)

mmsd_abs_long <- long_df(mmsd_blanks_abs, study = "MMSD", wavelength = "Wavelength") %>%
  select(-y)%>%
  rename(Wavelength=x)

glri_abs_long <- long_df(glri_blanks_abs, study = "GLRI", wavelength = "Wavelength") %>%
  select(-y)%>%
  rename(Wavelength=x)

glpf_abs_long <- long_df(glpf_blanks_abs, study = "GLPF", wavelength = "Wavelength") %>%
  select(-y) %>%
  rename(Wavelength=x)

blanks_abs <- bind_rows(mmsd_abs_long, glri_abs_long, glpf_abs_long)

peaks_blank <- abs_peaks(abs_signals_all, blanks_abs)

summary_abs_blank <- peaks_blank %>%
  group_by(Wavelength, study) %>%
  summarize(mean = mean(value, na.rm = TRUE))

data.table::fwrite(summary_abs_blank, file = file.path("explore","out","summary_abs_blanks.csv"))

ggplot(summary_abs_blank) +
  geom_point(aes(x=Wavelength, y=mean)) +
  facet_grid(. ~ study) +
  theme_bw()
