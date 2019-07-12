################## Second Simple Method ###########################
## Using 5-fold CV to determine the optimal tuning parameter lambda



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
predictors<- c("T", "F")#,"Turbidity_mean", "OB1","Aresid267","S1.25","rF_T")

interactors <- c("sinDate","cosDate")

#Develop regression formula for fixed effects
form1 <- c(paste(predictors,interactors[1], sep=":"), paste(predictors,interactors[2], sep=":"))
form1 <- c(predictors, form1)
form1 <- paste(form1,collapse = " + ")
form1 <- paste("log_response ~ 1 + ",form1)

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

### set seed
set.seed(123)

family = gaussian(link="identity")

N<-dim(model_df)[1]

ind<-sample(N,N)

lambda <- seq(50,0,by=-0.5)

kk<-5

nk <- floor(N/kk)

Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))

## first fit good starting model
library(MASS);library(nlme)

PQL<-glmmPQL(log_response~1,random = ~1|abbrev,family=family,data=model_df)

Delta.start<-c(as.numeric(PQL$coef$fixed),rep(0,14),as.numeric(t(PQL$coef$random$abbrev)))

Q.start<-as.numeric(VarCorr(PQL)[1,1])


Delta.start<-as.matrix(t(rep(0,14)))

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
    
    model_df.train<-model_df[-indi,]
    model_df.test<-model_df[indi,]
    
    # glm2 <- try(glmmLasso(log_response~T + F + Turbidity_mean
    #                       + T:sinDate + T:cosDate
    #                       + F:sinDate + F:cosDate,
    #                       rnd = list(abbrev = ~1),
    #                       family = family, data =model_df.train, lambda=lambda[j],switch.NR=F,final.re=TRUE,
    #                       control=list(start=Delta.start,q_start=Q.start))
    #             ,silent=TRUE)

    glm3 <- try(glmmLasso(form1,
                          rnd = list(abbrev = ~1),
                          family = family, data =model_df.train, lambda=lambda[j],switch.NR=F,final.re=TRUE,
                          control=list(start=Delta.start,q_start=Q.start))
                ,silent=TRUE)
    
    if(class(glm2)!="try-error")
    {  
      y.hat<-predict(glm2,model_df.test)    
      
      Devianz_ma[j,i]<-sum(family$dev.resids(model_df.test$log_response,y.hat,wt=rep(1,length(y.hat))))
    }
  }
  print(sum(Devianz_ma[j,]))
}


Devianz_vec<-apply(Devianz_ma,1,sum)

opt2<-which.min(Devianz_vec)
plot(Devianz_vec)

glm2_final <- glmmLasso(log_response~T + F + Turbidity_mean 
                        + T:sinDate + T:cosDate 
                        + F:sinDate + F:cosDate,
                        rnd = list(abbrev = ~1 + F),  
                        family = family, data = model_df, lambda=lambda[opt2],switch.NR=F,final.re=TRUE,
                        control=list(start=Delta.start,q_start=Q.start))

summary(glm2_final)

plot(model_df$log_response, predict(glm2_final))
