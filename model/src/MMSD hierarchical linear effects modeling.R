# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R

# Intro to Linear mixed models: https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/
# lme4 package vignette describing method with Table 2 describing grouping options:
#   https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

# 1. Load data
# 2. General modeling setup:
#   * Define response variables
#     * Transform response variables
#   * Define predictors
#     * Define interactions
#     * Define grouping variables (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
#         *Using this form: x + (x || g) 1 + x + (1 | g) + (0 + x | g) Uncorrelated random intercept and slope.

#   * Construct formula
#   * Choose sites or states to be included
#   * Filter data to sites and make model df

# 3. Run LME model for all response variables
# 4. Graph results


library(lme4)
library(smwrBase)
library(car)
library(dplyr)

# 1. Load data
df_MMSD <- readRDS(file.path("process","out","mmsd_summary.rds"))
df <- df_MMSD

# 2. General modeling setup:

#  * Define response variables
response <- c("lachno2","bacHum","eColi","ent")


# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
#predictors<- c("Aresid267", "T", "F")
predictors<- c("T", "F")
predictors<- c("F")
predictors<- c("F","T")
#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")



#   * Construct formula
form <- paste("")
for(j in 1:length(interactors)){ 
  form_temp <- paste(paste(predictors,"*",interactors[j],sep=""),collapse = " + ")
  if(j>1) {form <- paste(form_temp, " + ", form) 
  }else form <- form_temp
}



form <- paste("log_response ~ ",form)

# Add groupings

for(i in 1:length(groupings)) {
  
  group_form <- paste("(",paste(predictors, collapse = " + ")," | ",groupings[i], ")")
  form <- formula(paste(form, " + ", group_form))
  
}


# 3. Run LME model for all response variables
filenm <- "mmsd_models.pdf"
pdf(filenm)
for (i in 1:length(response)) {
  #   * transform response variable
  df$log_response <- log10(df[,response[i]])

  #   * Choose sites or states to be included
  sites <- c("MC","MW","UW")
  
  #   * Filter data to sites and make model df
  model_rows <- which(df[,groupings] %in% sites)
  model_columns <- c("log_response", predictors,interactors,groupings)
  model_df <- df[model_rows,model_columns]
  model_df <- na.exclude(model_df)
  
  
  #   
  x <- as.data.frame(scale(model_df[,predictors]))
  names(x) <- predictors
  
  model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,c(interactors, groupings)])
  names(model_df_scaled) <- c("log_response",predictors,interactors,groupings)
  
  #   * Run model
  m <- lmer(form,data=model_df_scaled)
  
  
  
  # model_df_scale <- cbind(as.data.frame(scale(model_df[,-dim(model_df)[2]])),model_df[,dim(model_df)[2]])
  # names(model_df_scale) <- model_columns
  # m <- lmer(form,data=model_df)
  # m <- lmer(form,data=model_df_scaled)
  
  summary(m)
  Anova(m)
  
  
  # 4. Graph results
  colorOptions <- c("orange","yellow2","skyblue","black","springgreen4","blue","grey","darkorchid1")
  names(colorOptions) <- unique(model_df$abbrev)
  legend_names <- unique(model_df$abbrev)

  # Define CSO points
  if(response[i] == "lachno2") CSO_rows <- which(model_df$log_response >5.2)

  plotColors <- colorOptions[model_df[,"abbrev"]]
  levels(as.factor(model_df[,"abbrev"]))
  plotColors2 <- "white";  plotColors2[CSO_rows] <- "black"
  
  axis.log <- ""

  axis.limits <- range(c(model_df$log_response,fitted(m))) #c(1,7)
  
  par(mfcol=c(1,1))
  plot(model_df[,"log_response"],fitted(m),
       xlab="Observed",ylab="Predicted",col=plotColors,pch=20,log=axis.log,ylim=axis.limits,xlim=axis.limits)
  abline(0,1)
  #mtext(paste(Active.Coef.names[2:length(Active.Coef.names)],collapse=", "),side=3,line=1,cex=0.8)
  mtext(paste("Linear mixed effects moedel for MMSD watersheds. Response = ",response[i]),side=3,line=2,font=2,cex=1)
  mtext(form,side=3,line=0)
#  legend(x="topleft",legend=legend_names,col=colorOptions[legend_names],pch=20,text.col=colorOptions,cex=0.7)
  legend(x="topleft",legend=c("MC CSO",legend_names),pt.bg=c("orange",colorOptions[legend_names]),
         col=c("black","white","white","white"),pch=21,text.col=c("orange",colorOptions),cex=0.7)
  
  
  #Highlight CSO points
  points(model_df[,"log_response"],fitted(m),col=plotColors2,bg="white",cex=1.)
  
}
dev.off()
shell.exec(filenm)



