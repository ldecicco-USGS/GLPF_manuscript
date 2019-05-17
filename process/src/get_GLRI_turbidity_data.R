# Retrieve continuous water quality data for GLRI sites

library(dataRetrieval)

get_GLRI_turbidity_data <- function() {
  staid <- c("04067500", "04085427", "04087170", "04165500", "04166500", "04176500", "04193490", "04195500")
  names(staid) <- c("EE","OC", "JI", "CL", "RO", "RM", "MA", "PO")
  
  Turb_parm <- "63680"
  bdate <- "2011-07-01"
  edate <- "2013-09-30"
  
  Turb <- readNWISuv(staid,Turb_parm,bdate,edate,tz="UTC")  
  names(Turb)[4] <- "Turbidity"
  
  saveRDS(Turb,file = file.path("raw","GLRI","GLRI_Turbidity.rds"))
  
}