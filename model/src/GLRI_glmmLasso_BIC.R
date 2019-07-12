# GLRI version

################## More Elegant Method ############################################
## Idea: start with big lambda and use the estimates of the previous fit (BUT: before
## the final re-estimation Fisher scoring is performed!) as starting values for the next fit;
## make sure, that your lambda sequence starts at a value big enough such that all covariates are
## shrinked to zero;

## Using BIC (or AIC, respectively) to determine the optimal tuning parameter lambda

library(glmmLasso)
library(smwrBase)

source(file.path("model","src","plot_model_cv.R"))

# 1. Load data
df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI

response <- c("Lachno.2.cn.100ml","BACHUM.cn.100mls","E..coli.CFUs.100ml","ENTERO.cn.100mls","Entero.CFUs.100ml")

# Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","OB1","Aresid267","S1.25","rF_T")

interactors <- c("sinDate","cosDate")

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")

i <- 1
df$log_response <- log10(df[,response[i]])

#   * Choose sites or states to be included
sites <- c("JI", "PO", "MA", "CL", "RO", "RM")

#   * Filter data to sites and make model df
model_rows <- which(df[,groupings] %in% sites)
model_columns <- c("log_response", response,predictors,interactors,groupings)
model_df <- df[model_rows,model_columns]
model_df <- na.exclude(model_df)
model_df[,groupings] <- as.factor(model_df[,groupings])
#model_df <- model_df[-which(model_df$Turbidity_mean=="NaN"),]


#   
x <- as.data.frame(scale(model_df[,predictors]))
names(x) <- predictors
model_df <- cbind(model_df[,c("log_response",interactors,groupings)],x)
#df<-data.frame(df)   

#family = poisson(link = log)


# Predictors: "Turbidity_mean", "T", "F","OB1","Aresid267","S1.25","rF_T"
# Interaction terms: "sinDate","cosDate"
# "abbrev"
form1 <- c(paste(predictors,interactors[1], sep=":"), paste(predictors,interactors[2], sep=":"))
form1 <- c(predictors, form1)
form1 <- paste(form1,collapse = " + ")
form1 <- paste("log_response ~ 1 + ",form1)


form1 <- "Turbidity_mean + T + F + Turbidity_mean:sinDate + T:sinDate + F:sinDate + Turbidity_mean:cosDate + T:cosDate + F:cosDate"
form_fixed <- as.formula(paste("log_response ~ 1 + ",form1))

formTest <- as.formula("log_response ~ Turbidity_mean*sinDate + T*sinDate + F*sinDate + OB1*sinDate")


run_glmmLasso_BIC <- function(model_df,form_rnd,form_fixed,lambda){

#specify lambdas
lambda <- seq(50,0,by=-0.5)

BIC_vec<-rep(Inf,length(lambda))

# specify starting values for the very first fit; pay attention that Delta.start has suitable length! 
Delta.start<-as.matrix(t(rep(0,14+6)))

Q.start<-0.1

form_fixed <- as.formula("log_response~T + F + Turbidity_mean + T:sinDate + T:cosDate + F:sinDate + F:cosDate")
form_rnd <- as.formula("abbrev = ~1")

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  glm3 <- glmmLasso(form_fixed,
                    rnd = list(abbrev = ~1 + T),  
                    family = gaussian(link="identity"), data = model_df, 
                    lambda=lambda[j], switch.NR=F,final.re=TRUE,
                    control = list(start=Delta.start[j,],q_start=Q.start[j]))  
  
  
  print(colnames(glm3$Deltamatrix)[2:7][glm3$Deltamatrix[glm3$conv.step,2:7]!=0])
  BIC_vec[j]<-glm3$bic
  Delta.start<-rbind(Delta.start,glm3$Deltamatrix[glm3$conv.step,])
  Q.start<-c(Q.start,glm3$Q_long[[glm3$conv.step+1]])
}

opt3<-which.min(BIC_vec)

plot(BIC_vec)

glm3_final <- glmmLasso(log_response~T + F + Turbidity_mean + T:sinDate + T:cosDate + F:sinDate + F:cosDate,
                        rnd = list(abbrev = ~1),  
                        family = gaussian(link="identity"), data = model_df, lambda=lambda[opt3],
                        switch.NR=F,final.re=TRUE,
                        control = list(start=Delta.start[opt3,],q_start=Q.start[opt3]))  

summary(glm3_final)

return(list(Delta.start[opt3],Q.start[opt3]))
}