# Run SI plot and generate data files for SI



# Figure SI-2



# Figure SI-3
source(file.path("plots","src","Figure SI 3.R"))
Figure_SI_3 <- plot_ww()

#Data tables for SI
source(file.path("process","src","save_data_for_SI.R"))
  # need to paste these files into the SI workbook
