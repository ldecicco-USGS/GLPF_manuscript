#Define model formulas for model archive

# 1. GLRI Jones Island 
##  a. Sensor models
##  b. non-corr models
# 2. GLRI non-Jones Island
##  a. Sensor models
##  b. non-corr models
# 3. MMSD models
##  a. Sensor models
##  b. non-corr models


get_formulas <- function(sites, parm_category){
  
  site_groups <- c("Agriculture", "CL_RO", "JI", "2-sites", "3-sites")
  
  if(parm_category=="sensors"){
    formula_file_names <- c("GLRI_formulas.rds","GLRI_formulas.rds","GLRI Jones Island formulas.rds","MMSD_formulas.rds","MMSD_formulas.rds")
    names(formula_file_names) <- site_groups
  }else if(parm_category == "non-cor"){
    formula_file_names <- c("GLRI_formulas_no_corr.rds","GLRI_formulas_no_corr.rds","GLRI Jones Island formulas no-corr.rds","MMSD_formulas_no_corr.rds","MMSD_formulas_no_corr.rds")
    names(formula_file_names) <- site_groups
  }

  form <- readRDS(paste0("./process/out/",formula_file_names[sites]))
  
  return(form)
}
# 
#     formula_file_names <- c("GLRI_formulas_no_corr.rds","GLRI_formulas_no_corr.rds","JI_formulas.rds","MMSD_formulas.rds","MMSD_formulas.rds")
#     names(formula_file_names) <- site_groups
#     form <- readRDS(formula_file_names[sites])
#     
#     ## 1. Define formulas for GLRI
#     ### a. Jones Island
#     if(sites == "JI") {
#       
#       if(parm_category == "sensors") {
#         form <- list()
#         form[[1]] <- formula("log_response ~ Turbidity_mean + T * cosDate + T * sinDate + sinDate + cosDate")
#         form_names <- c("Turb_T")
#         names(form) <- form_names
#       }else{
#         form <- readRDS(file = "./process/out/GLRI Jones Island formulas no-corr.rds")
#         
#         ### b. Define formulas for GLRI ag and urban models
#       }else if(sites %in% c("Agriculture","CL_RO")) {
#         
#         if (parm_category == "non-corr") {
#           
#           form <- readRDS(file = "./process/out/GLRI_formulas_no_corr.rds")
#           
#         } else {
#           
#           form <- readRDS(form,file = "./process/out/GLRI_formulas.rds")
#           
#         }
#         
#         ## 1. Define formulas for MMSD
#         
#       }else if (sites == "2-sites") {
#         
#         if (parm_category == "non-corr") {
#           
#           form <- readRDS(file = "./process/out/MMSD_CG_BK_formulas.rds")
#           
#         } else {
#           
#           form <- readRDS(file = "./process/out/MMSD_CG_BK_formulas_no_corr.rds")
#           
#         }
#       }else if(sites = "3-sites") {
#         form <- readRDS(file = "./process/out/MMSD_formulas.rds")
#         
#       } else {
#         
#         form <- readRDS(file = "./process/out/MMSD_formulas_no_corr.rds")
#         
#       }
#     }
#   }
#   
#   