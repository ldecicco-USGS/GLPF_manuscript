# Plot frequencies
library(ggplot2)

dfAbs <- read.csv(file.path("process","out","summary_abs.csv"))

study_colors <- c("blue","forestgreen","red")
names(study_colors) <- c("GLPF","GLRI","MMSD")
dfAbs$plotcolors <- study_colors[dfAbs$study]

plot(dfAbs$Wavelength,dfAbs$mean_freq,col=dfAbs$plotcolors,pch=20)



dfFl <- read.csv(file.path("process","out","summary_fl.csv"))

dfFl$plotcolors <- study_colors[dfFl$study]
dfFl$Peak <- factor(dfFl$Peak,levels = dfFl$Peak)

boxplot(dfFl$mean_freq~dfFl$Peak | df$study,col=dfFl$plotcolors,pch=20,las=2)

ggplot(dfFl, aes(x = dfFl$study,y=dfFl$mean_freq),xlab = "Study") +
  geom_point() +
  facet_wrap(facets = dfFl$Peak,nrow = 7) + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Study") + ylab("Frequency") +
  ggtitle("Fluorescence frequency of detection") +
  theme(plot.title = element_text(hjust = 0.5))

