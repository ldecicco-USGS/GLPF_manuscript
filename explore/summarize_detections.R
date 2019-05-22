library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

source(file.path("explore","src","explore_summaries.R"))

# Fluorescence:
fl_signals_all <- fl_signals()

glri <- readRDS(file.path("process","out","glri_fl_MRL_adjusted.rds"))
mmsd <- readRDS(file.path("process","out","mmsd_fl_MRL_adjusted.rds"))
glpf <- readRDS(file.path("process","out","glpf_fl_MRL_adjusted.rds"))

rmks_fl <- combine_remarks(glri, mmsd, glpf)
df_fl <- combine_values(glri, mmsd, glpf)
rm(glpf, glri, mmsd)

fl_MRLs <- readRDS(file.path("process","out","fl_MRLs.rds"))

fl_MRL_clean <- split_exem(fl_MRLs, wavelength = "Wavelength") %>%
  select(x, y, MRL)

fl_remark_summary <- split_exem(rmks_fl)  %>%
  left_join(fl_MRL_clean, by=c("x","y"))

peaks <- em_peaks(fl_signals_all, fl_remark_summary)
lines <- em_long(fl_signals_all, fl_remark_summary)
boxes <- eems_boxes(fl_signals_all, fl_remark_summary)

ggplot() +
  geom_point(data = peaks, aes(x=x, y=y, color = freq),
             size = 5, shape=16, alpha = 0.3) +
  geom_point(data = lines, aes(x=x, y=y, color = freq),
             size = 5, shape=16, alpha = 0.3) +
  geom_point(data = boxes, aes(x=x, y=y, color = freq),
             size = 5, shape=16, alpha = 0.3) +
  geom_text(data = peaks,
            aes(x=x,y=y,label=Peak), size=3) +
  facet_grid(. ~ study) +
  theme_bw() + 
  scale_color_gradient(
    trans = "log",
    low = "blue", high = "white")

all_signals <- bind_rows(peaks, lines, boxes) 

summary_signals <- all_signals %>%
  group_by(Peak, study ) %>%
  summarize(mean_freq = mean(freq, na.rm = TRUE),
            total = mean(total, na.rm = TRUE),
            mean_cen = mean(censored, na.rm = TRUE),
            mean_MRL = mean(MRL, na.rm = TRUE))

fl_sum_values <- split_exem(df_fl)  %>%
  tidyr::gather("stat","value", -x, -y, -study)

peaks_vals <- em_peaks(fl_signals_all, fl_sum_values)
lines_vals <- em_long(fl_signals_all, fl_sum_values)
boxes_vals <- eems_boxes(fl_signals_all, fl_sum_values)

ggplot() +
  geom_point(data = peaks_vals, aes(x=x, y=y, color = value),
             size = 5, shape=16, alpha = 0.3) +
  geom_point(data = lines_vals, aes(x=x, y=y, color = value),
             size = 5, shape=16, alpha = 0.3) +
  geom_point(data = boxes_vals, aes(x=x, y=y, color = value),
             size = 5, shape=16, alpha = 0.3) +
  geom_text(data = peaks_vals,
            aes(x=x,y=y,label=Peak), size=3) +
  facet_grid(stat ~ study, scales = "free") +
  theme_bw() + 
  scale_color_gradient(trans = "log", 
                       low = "red", high = "white")

all_signals_stats <- bind_rows(peaks_vals, 
                               lines_vals, 
                               boxes_vals) 

summary_signals_stats <- all_signals_stats %>%
  filter(stat == "mean") %>%
  group_by(Peak, study) %>%
  summarize(mean = mean(value, na.rm = TRUE))

summary_fl <- summary_signals_stats %>%
  left_join(summary_signals, by=c("Peak","study"))

data.table::fwrite(summary_fl, file = file.path("explore","out","summary_fl.csv"))

######################################
# Absorbance

abs_signals_all <- abs_signals()
abs_MRLs <- readRDS(file.path("process","out","abs_MRLs.rds"))

# Abs Data:
glri_abs <- readRDS(file.path("process","out","glri_abs_MRL_adjusted.rds"))
mmsd_abs <- readRDS(file.path("process","out","mmsd_abs_MRL_adjusted.rds"))
glpf_abs <- readRDS(file.path("process","out","glpf_abs_MRL_adjusted.rds"))

rmks_abs <- combine_remarks(glri_abs, mmsd_abs, glpf_abs, wavelength = "Wavelength")
df_abs <- combine_values(glri_abs, mmsd_abs, glpf_abs, wavelength = "Wavelength")
rm(glpf_abs, glri_abs, mmsd_abs)

rmks_abs <- rmks_abs %>%
  left_join(select(abs_MRLs, Wavelength, MRL), by="Wavelength")

peaks_censor <- abs_peaks(abs_signals_all, rmks_abs)

ggplot() +
  geom_line(data = peaks_censor,
            aes(x = Wavelength, y = freq)) +
  facet_grid(. ~ study) +
  theme_bw() 

summary_abs_det <- peaks_censor %>%
  group_by(Wavelength, study) %>%
  summarize(mean_freq = mean(freq, na.rm = TRUE),
            mean_censored = mean(censored, na.rm = TRUE),
            total = mean(total, na.rm = TRUE),
            mean_MRL = mean(MRL, na.rm = TRUE))

peaks_values <- abs_peaks(abs_signals_all, df_abs)

peaks_long <- peaks_values %>%
  tidyr::gather("stat","value",-Wavelength, -study)

ggplot() +
  geom_line(data = peaks_long,
            aes(x = Wavelength, y = value)) +
  geom_point(data = peaks_long,
            aes(x = Wavelength, y = value)) +
  facet_grid(stat ~ study, scales = "free") +
  theme_bw() 

summary_abs_stats <- peaks_long %>%
  filter(stat == "mean") %>%
  group_by(Wavelength, study) %>%
  summarize(mean = mean(value, na.rm = TRUE))

summary_abs <- summary_abs_stats %>%
  left_join(summary_abs_det, by=c("Wavelength","study"))

data.table::fwrite(summary_abs, file = file.path("explore","out","summary_abs.csv"))

