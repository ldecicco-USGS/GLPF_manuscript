library(tidyr)
library(ggplot2)
library(lme4)
library(smwrBase)

source(file.path("model","src","plot_model_cv.R"))

# 1. Load data
df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI

# 2. General modeling setup:

#  * Define response variables
response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")
#response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls")

# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","OB1","Aresid267","S1.25","rF_T","A254")

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
site_combos[[5]] <- c("PO", "MA", "RM")

sites <- site_combos[[1]]

names(site_combos) <- c("All","no_JI","urban","CL_RO","Agricultural")

form_names <- c("F","F2","F,T","F,Turb","F,S1","F,A254","F,Aresid","T","T2","T,Turb","Turb","Turb2","F,T,Turb", "F,T,Turb 2","F,T,Turb 3" )
form <- list()
form[[1]] <- formula("log_response ~ F * cosDate + F * sinDate + sinDate + cosDate + (F + 1 | abbrev)")
form[[2]] <- formula("log_response ~ F * cosDate + F * sinDate + (1 | abbrev)")
form[[3]] <- formula("log_response ~ F * cosDate + T * cosDate + F * sinDate + T * sinDate + (F + 1 | abbrev)")
form[[4]] <- formula("log_response ~ F * cosDate + Turbidity_mean * cosDate + F * sinDate + Turbidity_mean * sinDate + (F + 1 | abbrev)")
form[[5]] <- formula("log_response ~ F * cosDate + F * sinDate + S1.25 * cosDate + S1.25 * sinDate  + (S1.25 | abbrev)")
form[[6]] <- formula("log_response ~ F * cosDate + F * sinDate + A254 * cosDate + A254 * sinDate  + (F | abbrev)")
form[[7]] <- formula("log_response ~ F * cosDate + F * sinDate + Aresid267 * cosDate + Aresid267 * sinDate  + (Aresid267 | abbrev)")
form[[8]] <- formula("log_response ~ T * cosDate + T * sinDate  + (T + 1 | abbrev)")
form[[9]] <- formula("log_response ~ T * cosDate + T * sinDate + (1 | abbrev)")
form[[10]] <- formula("log_response ~ T * cosDate + Turbidity_mean * cosDate + T * sinDate + Turbidity_mean * sinDate + (Turbidity_mean + 1 | abbrev)")
form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate  + (Turbidity_mean | abbrev)")
form[[12]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + (1 | abbrev)")
form[[13]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (F + Turbidity_mean | abbrev)")
form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (F | abbrev)")
form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate  + Turbidity_mean * cosDate + Turbidity_mean * sinDate + (1 | abbrev)")


names(form) <- form_names[1:length(form)]
# # 3. Run LME model for all response variables

# Set boundary tolerance for singularity consistent with "isSingular()"
options(lmerControl(boundary.tol=1e-4))

singularity_df <- matrix(nrow = length(form),ncol = length(response))
for (i in 1:length(response)) {
  print(response[i])
  
  
  #filenm <- paste("GLRI_predictions_",response[i],".pdf",sep="")
  #  pdf(filenm)
  
  #   * transform response variable
  df$log_response <- log10(df[,response[i]])
  
  #   * Filter data to sites and make model df
  model_rows <- which(df[,groupings] %in% sites)
  model_columns <- c("log_response", response,predictors,interactors,groupings)
  model_df <- df[model_rows,model_columns]
  model_df <- na.exclude(model_df)
  
  # develop dataframe of model variables
  #   -start with predictors
  x <- as.data.frame(scale(model_df[,predictors]))
  names(x) <- predictors
  
  #   -add in response, interactors, and groups
  model_df_scaled <- cbind(model_df[,"log_response"],x,model_df[,c(interactors, groupings)])
  names(model_df_scaled) <- c("log_response",predictors,interactors,groupings)
  
  
  singularities <- logical()
  for(f in 1:length(form)){
    
    
    print(names(form)[f]);f
    m <- lmer(form[[f]],data=model_df_scaled)
    
    singularities <- c(singularities,isSingular(m))
  }
names(singularities) <- names(form)
  singularity_df[,i] <- singularities
}

singularity_df <- as.data.frame(singularity_df)
names(singularity_df) <- response
singularity_df$model <- make.names(form_names)

singularity_long <- gather(data = singularity_df,key = "Organism",value = "Singular",-model)
singularity_long$model <- factor(singularity_long$model,levels = make.names(form_names))
singularity_long$Singular <- as.numeric(singularity_long$Singular)
ggplot(data=singularity_long, aes(x=model,y=Singular)) +
  geom_bar(stat = "identity") + 
  facet_grid(Organism ~ .)

# summary(m)
# anova(m)
# 
# # response1: form [1:10]. 
# nonconverge[[1]] <- 11
# nonconverge[[2]] <- 11
# nonconverge[[3]] <- 
# nonconverge[[4]] <- 
# nonconverge[[5]] <- 
# nonconverge[[6]] <- 
# 
# singular[[1]] <- c(5,6)
# singular[[2]] <- c()
# singular[[3]] <- c()
# singular[[4]] <- c()
# singular[[5]] <- c()
# singular[[6]] <- c()
# 
# library(dplyr)
# # From http://stackoverflow.com/questions/1181060
# stocks <- tibble(
#   time = as.Date('2009-01-01') + 0:9,
#   X = rnorm(10, 0, 1),
#   Y = rnorm(10, 0, 2),
#   Z = rnorm(10, 0, 4)
# )
# 
# gather(stocks, "stock", "price", -time)
# 
# 
# df <- data.frame(dose=c("D0.5", "D1", "D2"),
#                  len=c(4.2, 10, 29.5))
# 
# ggplot(data=singularity_long, aes(x=model,y=Singular)) +
#   geom_bar(stat="identity") + 
#   facet_grid(Organism ~ .)
# 
# p<-ggplot(data=df, aes(x=dose, y=len)) +
#   geom_bar(stat="identity")
# p
# 
# # Horizontal bar plot
# p + coord_flip()
