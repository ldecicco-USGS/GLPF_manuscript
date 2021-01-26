# Model archive for "Optical properties of water for prediction of wastewater contamination, human-associated bacteria, and fecal indicator bacteria in surface water"

source(file.path("model_archive","src","get_formulas.R"))
       
# Read model table
model_table <- readRDS("model/out/modeling_summary_table.rds")
model_table$project <- c(rep("GLRI",6),rep("MMSD",4))

formulas <- get_formulas()


for(i in 1:dim(sub_model_table)[1]){
  project <- model_table$project[i]
  
#MMSD models
if(project == "MMSD"){
  
# 1. Load data
df_MMSD <- readRDS(file.path("process","out","mmsd_summary.rds"))
df <- df_MMSD

# 2. General modeling setup:

#  * Define response variables
response <- c("lachno2","bacHum","eColi","ent")
response_header_name <- c("Lachno","Bachuman","E. Coli","Entero")
# * Transform seasonal variables
df$sinDate <- fourier(df$psdate)[,1]
df$cosDate <- fourier(df$psdate)[,2]

# Define predictors and interaction terms
predictors<- c("Turbidity_mean", "T", "F","M")

#non_int_predictors <- c("CSO")
interactors <- c("sinDate","cosDate")

# Define grouping variable (sites for MMSD and GLRI or states for GLPF. Maybe hydro condition for GLPF)
groupings <- c("abbrev")

site_combos <- list()
site_combos[[1]] <- c("MC", "MW", "UW")

names(site_combos) <- c("3-sites")
parm_category <- "sensors"

#Define model formulas
form <- get_formulas(parm_category)

# 

model_table_row <- which(model_table$Sites == names(site_combos) & model_table$`Parameter Category` == parm_category)
form_name <- as.character(as.data.frame(model_table)[model_table_row,response_header_name[i]])

model_formula <- form[form_name]

## Now use model formula to run 50-fold x-val and compute NRMSE for this model instance

