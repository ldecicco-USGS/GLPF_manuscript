regressionTree <- function(summaryDF, file_out, response = "bacHum"){
   
  # responses <- c("bacHum","lachno","contamination_rank")
  # j <- responses[3]
  j <- response
  
  IVs <- names(summaryDF)[which(names(summaryDF) == "OB1"):length(names(summaryDF))]
  
  form <- formula(paste(j, "~", paste(IVs,collapse="+")))
  
  summaryDF <- summaryDF[!is.na(summaryDF$contamination_rank),]
  summaryDF$contamination_rank <- factor(summaryDF$contamination_rank)
  
  opt.sum.rt <- rpart(formula=form, data=summaryDF,
                      control=rpart.control(minsplit=40, minbucket=40, cp=0.001))#,xval=dim(summaryDF)[1]))
  
  opt.sum.rt.Prune <- prune(opt.sum.rt,cp=opt.sum.rt$cptable[which.min(opt.sum.rt$cptable[,"xerror"]),"CP"])
  
  plot(as.party(opt.sum.rt), tp_args=list(id=FALSE))

  
  summaryDF$contamination_rank <- as.integer(summaryDF$contamination_rank)
  
  m <- rpartScore(form,data=summaryDF,control=rpart.control(minsplit=40))
  
  saveRDS(m, file= file.path("model","out",file_out))

  return(opt.sum.rt)
}
