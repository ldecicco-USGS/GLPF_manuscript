#Plot observed vs predicted for cv predictions for lme models


plot_model_cv <- function(df_predictions,form){
  # 4. Graph results
  plot <- "off"
  colorOptions <- c("orange","yellow2","skyblue","black","springgreen4","blue","grey","darkorchid1")
  names(colorOptions) <- sort(unique(df_predictions$abbrev))
  legend_names <- sort(unique(df_predictions$abbrev))
  
  # Define CSO points
  CSO_rows <- which(df_predictions$lachno2 >158000)
  
  #set plot colors by site
  plotColors <- colorOptions[df_predictions[,"abbrev"]]
  levels(as.factor(df_predictions[,"abbrev"]))
  plotColors2 <- rep(NA,length(plotColors))
  plotColors2[CSO_rows] <- "black"
  
  axis.log <- ""
  
  axis.limits <- range(c(df_predictions$log_response,df_predictions$predictions)) #c(1,7)
  
  par(mfcol=c(1,1))
  plot(df_predictions[,"log_response"],df_predictions[,"predictions"],
       xlab="Observed",ylab="Predicted",col=plotColors,pch=20,log=axis.log,ylim=axis.limits,xlim=axis.limits)
  abline(0,1)
  #mtext(paste(Active.Coef.names[2:length(Active.Coef.names)],collapse=", "),side=3,line=1,cex=0.8)
  mtext(paste("Linear mixed effects moedel for MMSD watersheds. Response = ",response[i]),side=3,line=2,font=2,cex=1)
  mtext(paste(as.character(form[c(2,1,3)]),collapse=""),side=3,line=0)
  #  legend(x="topleft",legend=legend_names,col=colorOptions[legend_names],pch=20,text.col=colorOptions,cex=0.7)
  legend(x="topleft",legend=c(legend_names),pt.bg=c(colorOptions[legend_names]),
         col=c("black","white","white","white"),pch=21,text.col=c(colorOptions),cex=0.7)
  
  
  #Highlight CSO points
  points(df_predictions[,"log_response"],df_predictions[,"predictions"],col=plotColors2,bg="white",cex=1.)
}