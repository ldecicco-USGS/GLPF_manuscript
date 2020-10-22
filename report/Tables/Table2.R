make_table_2 <- function() {
  library(flextable)
  model_summary <- as.data.frame(readRDS("model/out/modeling_summary_table.rds"))
  model_summary$Bachuman <- sub("Turbidity_mean","Turb",model_summary$Bachuman)
  #model_summary$Bachuman <- sub("Turb","Turbidity",model_summary$Bachuman)
  vars <- unique(unlist(strsplit(model_summary$Bachuman,split = "_")))
  var_names <- c("Turbidity","F","T","M","N","A290","A295","T2","Turbidity")
  
  model_summary$Sites <- c("Watershed: Agriculture", "Watershed: Agriculture", 
                           "Watershed: Urban", "Watershed: Urban", "Milwaukee River", 
                           "Milwaukee River", "Subwatershed: Suburban", "Subwatershed: Suburban", 
                           "Subwatershed: Urban", "Subwatershed: Urban")
  
  response <- c("Bachuman","Lachno","Entero","Entero Culture", "E. Coli")
  #model_summary[,response]
  
  for (i in response) {
    model_summary[,i] <- gsub(pattern = "FI_2005",replacement = "FI",x =  model_summary[,i])
    model_summary[,i] <- gsub(pattern = "_",replacement = ", ",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rA290, ",replacement = "A290/",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rA280, ",replacement = "A280/",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rA350, ",replacement = "A350/",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rA275, ",replacement = "A275/",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "Turb",replacement = "Turbidity",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "Turb, ",replacement = "Turbidity, ",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "T2",replacement = "T",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "F2",replacement = "F",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rMrange.25, N",replacement = "M/N",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rMrange.25, F",replacement = "M/F",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rS2.25, S1.25",replacement = "S2/S1",x =  model_summary[,i])
  model_summary[,i] <- gsub(pattern = "rN, S3.25",replacement = "N/T",x =  model_summary[,i])
  }
  
  model_summary[which(is.na(model_summary[,"Entero Culture"])),"Entero Culture"] <- "--"
  model_summary[which(is.na(model_summary[,"RMSE Entero Culture"])),"RMSE Entero Culture"] <- "--"
  
  names(model_summary) <- make.names(names(model_summary))
  
  Header1 <- c("", "", "Human Bacteroides", "Human Bacteroides", 
               "Lachnospiraceae", "Lachnospiraceae", 
               "Enterococci","Enterococci", 
               "Enterococci Culture", "Enterococci Culture", 
               "E. coli", "E. coli")
  
  Header2 <- c("Sites", "Category", rep(c("Variables","NRMSE"),5))
  Type <- c(rep("character",2),rep(c("character","numeric"),5))
  
  header_df <- data.frame(col.keys = names(model_summary),
                          #type = Type,
                          what = Header1,
                          measure = Header2,
                          stringsAsFactors = FALSE)
  
  
  #names(model_summary) <- header2
  table1 <- flextable(model_summary, cwidth = 0.7)
  table1 <- fontsize(table1, size = 7)
  table1 <- width(table1, width = 0.5, j = grep("RMSE", names(model_summary)))
  table1 <- set_header_df(table1,mapping=header_df, key = names(header_df)[1])
  table1 <- height(table1, height = 0.3,
                   part = "header")
  table1 <- fontsize(table1, size = 8, part = "header")
  
  table1 <- align(table1,align="center",part = "all")
  table1 <- align(table1,j=c(1,2,3,5,7,9,11),align="left",part = "all")
  table1 <- align(table1,j=1,align="left")
  table1 <- border_outer(table1, border = NULL, part = "all")
  table1 <- hline(table1,i = 2, part = "header",border=fp_border(color="black",width=2)  )
  table1 <- hline_top(table1,part = "header",border=fp_border(color="black",width = 2)  ) 
  table1 <-merge_v(table1,j=~Sites) 
  table1 <-merge_h(table1,i=1,part="header") 
  table1 <- align(table1,i=1,align="center",part="header")
#  table1 <- set_caption(table1,"Table 2. Explanatory variables and root mean squared errors (RMSE) for regression equations to estimate bacteria concentrations using optical properties of water. [ADD VARIABLE ABBREVIATIONS HERE].")
  
  return(table1)
}