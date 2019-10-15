# Reference to Sam's dissertation:  
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/master/Code/05_analysis_hlm.R
#

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

library(MuMIn)
library(lme4)
library(smwrBase)
library(car)
library(dplyr)
library(cvTools)
library(ggplot2)
library(ggpubr)


source(file.path("model","src","plot_model_cv.R"))

# 1. Load data
df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI

# 2. General modeling setup:

#  * Define response variables
response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")
#response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls")

#Set censored values to detection limit
MDL <- c(225,225,1,225,1)
names(MDL) <- response
for(i in 1:length(response)){df[,response[i]] <- ifelse(df[,response[i]]<=MDL[response[i]],MDL[response[i]],df[,response[i]])}

# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","M")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

#predictors_withou_int <- "Turb"
# ADD THIS AFTER ADDING TURB DATA

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")

site_combos <- list()
site_combos[[1]] <- c("JI","PO", "MA", "CL", "RO", "RM")
site_combos[[2]] <- c("PO", "MA", "CL", "RO", "RM")
site_combos[[3]] <- c("JI","CL", "RO")
site_combos[[4]] <- c("CL", "RO")
site_combos[[5]] <- c("PO", "MA", "RM","JI")
site_combos[[6]] <- c("PO", "MA", "RM")

names(site_combos) <- c("All","no_JI","urban","CL_RO","AG_JI","Agricultural")

# form_names <- c("F","F2","F,T","F,Turb","F,S1","F,A254","F,Aresid","T","T2","T,Turb",
#                 "Turb","Turb2","F,T,Turb", "F,T,Turb 2","F,T,Turb 3",
#                 "F,Turb 2","F,T 2","F,Aresid 2","Aresid","Aresid2","T,Turb 2")
# form <- list()
# form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (F + 1 | abbrev)")
# form[[2]] <- formula("log_response ~ F * cosDate + F * sinDate + (1 | abbrev)")
# form[[3]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate + (F + 1 | abbrev)")
# form[[4]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate + (F + 1 | abbrev)")
# form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + S1.25 * cosDate + S1.25 * sinDate  + (S1.25 | abbrev)")
# form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + A254 * cosDate + A254 * sinDate  + (F | abbrev)")
# form[[7]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate  + (Aresid267 | abbrev)")
# form[[8]] <- formula("log_response ~ T * cosDate + T * sinDate  + (T + 1 | abbrev)")
# form[[9]] <- formula("log_response ~ T * cosDate + T * sinDate + (1 | abbrev)")
# form[[10]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate + (Turbidity_mean + 1 | abbrev)")
# form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate  + (Turbidity_mean | abbrev)")
# form[[12]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + (1 | abbrev)")
# form[[13]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (F + Turbidity_mean | abbrev)")
# form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (F | abbrev)")
# form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (1 | abbrev)")
# form[[16]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate + (1 | abbrev)")
# form[[17]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate + (1 | abbrev)")
# form[[18]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate  + (1 | abbrev)")
# form[[19]] <- formula("log_response ~ Aresid267 * cosDate + Aresid267 * sinDate  + (Aresid267 | abbrev)")
# form[[20]] <- formula("log_response ~ Aresid267 * cosDate + Aresid267 * sinDate  + (1 | abbrev)")
# form[[21]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate + (1 | abbrev)")

# Current sensor variables tried: F, T, S1, Turb. ** S1 was not useful beyond the others
# Easily developed: M
# Current sensors highly correlated with other current sensors: S2 (F), S3 (T), OB (F): r > 0.98

form <- list()
form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[2]] <- formula("log_response ~ T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[3]] <- formula("log_response ~ M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[4]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[7]] <- formula("log_response ~ T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[8]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[9]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[10]] <- formula("log_response ~ Turbidity_mean + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[11]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (Turbidity_mean + 1 | abbrev)")
form[[12]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[13]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[14]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (F + 1 | abbrev)")
form[[16]] <- formula("log_response ~ T * cosDate + T * sinDate + sinDate + cosDate + (T + 1 | abbrev)")
form[[17]] <- formula("log_response ~ M * cosDate + M * sinDate + sinDate + cosDate + (M + 1 | abbrev)")
form[[18]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate + (Turbidity_mean + 1 | abbrev)")
form[[19]] <- formula("log_response ~ F + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")

form_names <- c("F","T","M","Turb","F_T","F_M","T_M","Turb_F","Turb_T","Turb_M","F_T_M",
                "Turb_F_T","Turb_F_M","Turb_T_M",
                "F2","T2","M2","Turb2","F3")

sensors <- c("F","T","M")

turb <- "Turbidity_mean"

names(form) <- form_names[1:length(form)]
# # 3. Run LME model for all response variables

# Set boundary tolerance for singularity consistent with "isSingular()"
options(lmerControl(boundary.tol=1e-4))

f <- 11
i <- 1
s <- 6
sites <- site_combos[[s]]


df$log_response <- log10(df[,response[i]])

#   * Filter data to sites and make model df
model_rows <- which(df[,groupings] %in% sites)
model_columns <- c("log_response", response,predictors,interactors,groupings)
model_df <- df[model_rows,model_columns]
model_df <- na.exclude(model_df)

m <- lmer(form[[f]],data=model_df)

options(na.action = "na.fail")
md <- dredge(m)
subset(md, delta < 4)

summary(md)

par(mar = c(3,5,6,4))
plot(md, labAsExpr = TRUE)

model.avg(md, subset = delta < 4)

mat <- coef(md)
mat[2,]
