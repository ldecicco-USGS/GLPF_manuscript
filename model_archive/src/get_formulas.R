#Define model formulas for model archive

get_formulas <- function(parm_category){
  
  if (parm_category == "non-cor"){
    form_names <- character()
    for(i in 1:length(sensors)) {
      form[[(i*2-1)]] <- formula(paste("log_response ~ ",paste(sensors[i],c("* sinDate","* cosDate"),collapse=" + "),"+ sinDate + cosDate + (1 | abbrev)"))
      form[[(i*2)]] <- formula(paste("log_response ~ ","Turbidity_mean + ",
                                     paste(sensors[i],c("* sinDate","* cosDate"),collapse=" + "),"+ sinDate + cosDate + (1 | abbrev)"))
      form_names <- c(form_names,sensors[i],paste0(sensors[i],"_","Turb"))
    }
    
    names(form) <- form_names
    
  }else {
    
    
    form <- list()
    form[[1]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[2]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[3]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[4]] <- formula("log_response ~ Turbidity_mean + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[5]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[6]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[7]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[8]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[9]] <- formula("log_response ~ Turbidity_mean + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[10]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[11]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[12]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[13]] <- formula("log_response ~ Turbidity_mean * cosDate + Turbidity_mean * sinDate + T * cosDate + T * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[14]] <- formula("log_response ~ F * cosDate + F * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[15]] <- formula("log_response ~ F * cosDate + F * sinDate + M * cosDate + M * sinDate + sinDate + cosDate + (1 | abbrev)")
    form[[16]] <- formula("log_response ~ M * cosDate + M * sinDate + T * cosDate + T * sinDate + sinDate + cosDate + (1 | abbrev)")
    
    
    form_names <- c("Turb","Turb_F","Turb_T","Turb_M","Turb_F2","Turb_T2","Turb_M2",
                    "Turb_F_T","Turb_F_M","Turb_T_M","Turb_F_T2","Turb_F_M2","Turb_T_M2",
                    "F_T","F_M","T_M")
    
    names(form) <- form_names[1:length(form)]
    return(form)
  }
  
}