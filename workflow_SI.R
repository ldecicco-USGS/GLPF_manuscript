# Run SI plot and generate data files for SI


# Figure SI-1
source(file.path("plots","src","Figure SI 1.R"))
Figure_SI_1 <- plot_ww()


#Data tables for SI
source(file.path("process","src","save_data_for_SI.R"))
  # need to paste these files into the SI workbook
