################## Second Simple Method ###########################
## Using 5-fold CV to determine the optimal tuning parameter lambda



library(glmmLasso)
library(lmmlasso)
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
predictors_rnd_int <- c("T", "F")#,"OB1","Aresid267","S1.25","rF_T")
predictors_fixed_nonint <- c("Turbidity_mean","Aresid267","rF_T","sinDate","cosDate")
interactors <- c("sinDate","cosDate")
groupings <- c("abbrev")

#Define fixed predictors (do not vary with the groupings)
predictors_fixed_interaction <- c(paste(predictors_rnd_int,interactors[1], sep=":"), paste(predictors_rnd_int,interactors[2], sep=":"))

#Define random predictors (vary with the groupings)
predictors_rnd <- c("T","F")

i <- 1
df$log_response <- log10(df[,response[i]])

#   * Choose sites or states to be included
sites <- c("JI", "PO", "MA", "CL", "RO", "RM")

#   * Filter data to sites and make model df
model_rows <- which(df[,groupings] %in% sites)
model_columns <- c("log_response",predictors_rnd_int,predictors_fixed_nonint)
model_columns_form <- c("log_response",predictors_rnd_int,predictors_fixed_nonint,predictors_fixed_interaction)
form_all <- as.formula(paste("dummy ~",paste(paste(model_columns_form,collapse = " + "),"+ row_num")))
df$dummy <- 0

model_df_init <- df[model_rows,c("dummy",groupings, model_columns)]
model_df_init$row_num <- 1:dim(model_df_init)[1]

options(na.action="na.pass")
model_df <- model.matrix(form_all,model_df_init)

as.factor(model_df_init[model_df[,"row_num"],groupings])
model_df <- cbind(as.factor(model_df_init[model_df[,"row_num"],groupings]),model_df)
colnames(model_df)[1] <- groupings
#model_df <- cbind(model_df,as.factor(model_df_init[model_df[,"row_num"],"log_response"]))

model_df <- na.exclude(model_df)


### set seed
set.seed(123)


#############-------------------------------
# test lmmlasso


# m <- lmmlasso(x=x,y=y,z=z,grp=grp,lambda = 2,standardize = TRUE)
# 
# summary(m)
# print(m)
# m$coefficients


lambda <- seq(from = 20, to = 0, by = -1)
N<-dim(model_df)[1]
ind<-sample(N,N)
kk<-5
nk <- floor(N/kk)
m_AIC <- matrix(Inf,ncol=kk,nrow=length(lambda))
m_BIC <- matrix(Inf,ncol=kk,nrow=length(lambda))
m_deviance <- matrix(Inf,ncol=kk,nrow=length(lambda))
m_logLik <- matrix(Inf,ncol=kk,nrow=length(lambda))
m_objective <- matrix(Inf,ncol=kk,nrow=length(lambda))

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  for (i in 1:kk)
  {
    if (i < kk)
    {
      indi <- ind[(i-1)*nk+(1:nk)]
    }else{
      indi <- ind[((i-1)*nk+1):N]
    }
    
    # Y GIVES ERROR RIGHT NOW. LOOK AT 
    # model_df[, predictors_fixed_interaction]
    x <- as.matrix(cbind(1,model_df[-indi,c(predictors_fixed_nonint,predictors_fixed_interaction)]))
    y <- model_df[-indi,"log_response"]
    z <- as.matrix(model_df[-indi,predictors_rnd])
    grp <- factor(model_df[-indi,groupings])
    
    m <- lmmlasso(x=x,y=y,z=z,grp=grp,lambda = lambda[j],standardize = TRUE)
    
    # glm2 <- try(glmmLasso(form1,
    #                       rnd = list(abbrev = ~1),
    #                       family = family, data =model_df.train, lambda=lambda[j],switch.NR=F,final.re=TRUE,
    #                       control=list(start=Delta.start,q_start=Q.start))
    #             ,silent=TRUE)
    
    
    m_AIC[j,i] <- m$aic
    m_BIC[j,i] <- m$bic
    m_deviance[j,i] <- m$deviance
    m_logLik[j,i] <- m$logLik
    m_objective[j,i] <- m$objective
    
    
  }
  print(mean(m_BIC[j,]))
}


model_AIC_means <- apply(m_AIC,1,mean)
model_BIC_means <- apply(m_BIC,1,mean)
model_deviance_means <- apply(m_deviance,1,mean)
model_objective_means <- apply(m_objective,1,mean)
model_logLik_means <- apply(m_logLik,1,mean)

opt2<-which.min(model_BIC_means)
par(mfrow = c(5,1),mar=c(1,1,1,1))
plot(model_AIC_means,main="AIC")
plot(model_BIC_means,main="BIC")
plot(model_deviance_means,main="Deviance")
plot(model_objective_means,main="Objective")
plot(model_logLik_means,main="Log likelihood")


x <- as.matrix(cbind(1,model_df[,c(predictors_fixed_nonint,predictors_fixed_interaction)]))
y <- model_df[,"log_response"]
z <- as.matrix(model_df[,predictors_rnd])
grp <- factor(model_df[,groupings])


m <- lmmlasso(x=x,y=y,z=z,grp=grp,lambda = lambda[opt2],standardize = TRUE)


summary(m)

m_coef <- coef(m)


fitted_fixed <- x[,names(m_coef)] %*% m_coef
fitted_rnd <- 


par(mfrow=c(1,1))
plot(m$data$y, fitted(m))
