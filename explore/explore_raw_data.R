library(ggplot2)
library(ggforce)
library(dplyr)
library(tidyr)
library(sf)

source(file.path("explore","src","explore_summaries.R"))

load(file = file.path("process","out","glri_optical_Dec_2018_no_MRLs.RData"))
load(file = file.path("process","out","mmsd_optical_Dec_2018_no_MRLs.RData"))

glpf_fl <- readRDS(file.path("raw","GLPF","optics","dffl_noQA.rds"))

mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
mmsd_GRnumbers$GRnumbers <- as.character(mmsd_GRnumbers$GRnumbers)
glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
glri_GRnumbers$GRnumbers <- as.character(glri_GRnumbers$GRnumbers)
glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))
glpf_GRnumbers$GRnumbers <- as.character(glpf_GRnumbers$GRnumbers)

glri_GRnumbers <- filter(glri_GRnumbers, GRnumbers != "gr13755")

mmsd_blank_cols <- which(names(mmsd_fl) %in% mmsd_GRnumbers$GRnumbers)
glri_blank_cols <- which(names(glri_fl) %in% glri_GRnumbers$GRnumbers)

mmsd_fl <- data.frame(mmsd_fl)
mmsd_fl <- mmsd_fl[,-mmsd_blank_cols]
glri_fl <- glri_fl[,-glri_blank_cols]

print_fl_pages("GLPF_EEMs.pdf", glpf_fl, 
               color_lim = c(0,3), npages = 3, 
               filter_min = 0, filter_max = 10)
print_fl_pages("MMSD_EEMs.pdf", mmsd_fl, 
               color_lim = c(0,3), npages = 3, 
               filter_min = 0, filter_max = 10)
print_fl_pages("GLRI_EEMs.pdf", glri_fl, 
               color_lim = c(0,3), npages = 3, 
               filter_min = 0, filter_max = 10)

glri_long <- long_df(glri_fl, "GLRI")
mmsd_long <- long_df(mmsd_fl, "MMSD")
glpf_long <- long_df(glpf_fl, "GLPF")

fl_df <- bind_rows(glri_long,
                   mmsd_long,
                   glpf_long)

fl_df_summary <- fl_df %>%
  group_by(x, y, study) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

mean_raw <- ggplot(fl_df_summary) +
  geom_point(aes(x = x, y = y, color = mean)) +
  theme_bw() +
  xlim(c(250,450)) +
  scale_colour_gradient2(limits = c(0,3)) +
  facet_grid(. ~ study)

ggsave(mean_raw, filename = file.path("explore","out","mean_raw_data.pdf"))

fl_signals_all <- fl_signals()

peaks_raw <- em_peaks(fl_signals_all, fl_df_summary)
lines_raw <- em_long(fl_signals_all, fl_df_summary)
boxes_raw <- eems_boxes(fl_signals_all, fl_df_summary)

raw_signals <- bind_rows(peaks_raw, lines_raw, boxes_raw)

summary_raw_signals <- raw_signals %>%
  group_by(Peak, study ) %>%
  summarize(mean = mean(mean, na.rm = TRUE),
            min = min(min, na.rm = TRUE),
            max = max(max, na.rm = TRUE))

data.table::fwrite(summary_raw_signals, file = file.path("explore","out","summary_raw_signals_fl.csv"))

ggplot(summary_raw_signals) +
  geom_point(aes(x=study, y=mean)) +
  facet_wrap(. ~ Peak)

####################
# Abs:
source(file.path("process","src","explore_summaries.R"))
abs_signals_all <- abs_signals()

load(file = file.path("process","out","glri_optical_Dec_2018_no_MRLs.RData"))
load(file = file.path("process","out","mmsd_optical_Dec_2018_no_MRLs.RData"))

glpf_abs <- readRDS(file.path("raw","GLPF","optics","dfabs_noQA.rds"))
names(glpf_abs)[1] <- "Wavelength"
rm(glri_fl, mmsd_fl)

mmsd_GRnumbers <- readRDS(file.path("process","out","MMSD_PhaseIV_blank_GRnumbers.rds"))
mmsd_GRnumbers$GRnumbers <- as.character(mmsd_GRnumbers$GRnumbers)
glri_GRnumbers <- readRDS(file.path("process","out","GLRI_blank_GRnumbers.rds"))
glri_GRnumbers$GRnumbers <- as.character(glri_GRnumbers$GRnumbers)
glpf_GRnumbers <- readRDS(file.path("process","out","GLPF_blank_GRnumbers.rds"))
glpf_GRnumbers$GRnumbers <- as.character(glpf_GRnumbers$GRnumbers)

mmsd_blank_cols <- which(names(mmsd_abs) %in% mmsd_GRnumbers$GRnumbers)
glri_blank_cols <- which(names(glri_abs) %in% glri_GRnumbers$GRnumbers)

mmsd_abs <- data.frame(mmsd_abs)
mmsd_abs <- mmsd_abs[,-mmsd_blank_cols]
glri_abs <- glri_abs[,-glri_blank_cols]
rm(glri_GRnumbers, mmsd_GRnumbers, glpf_GRnumbers, glri_blank_cols, mmsd_blank_cols)

mmsd_abs_long <- long_df(mmsd_abs, study = "MMSD", wavelength = "Wavelength") %>%
  select(-y)%>%
  rename(Wavelength=x)

glri_abs_long <- long_df(glri_abs, study = "GLRI", wavelength = "Wavelength") %>%
  select(-y)%>%
  rename(Wavelength=x)

glpf_abs_long <- long_df(glpf_abs, study = "GLPF", wavelength = "Wavelength") %>%
  select(-y) %>%
  rename(Wavelength=x)

raw_abs <- bind_rows(mmsd_abs_long, glri_abs_long, glpf_abs_long)

peaks_raw <- abs_peaks(abs_signals_all, raw_abs)

summary_abs_raw <- peaks_raw %>%
  group_by(Wavelength, study) %>%
  summarize(mean = mean(value, na.rm = TRUE))

data.table::fwrite(summary_abs_raw, file = file.path("explore","out","summary_abs_raw.csv"))

ggplot(summary_abs_raw) +
  geom_point(aes(x=Wavelength, y=mean)) +
  facet_grid(. ~ study) +
  theme_bw()
