make_table_1 <- function(model_summary) {
  model_summary$Sites <- c("Watershed: Agriculture", "Watershed: Agriculture", 
                           "Watershed: Urban", "Watershed: Urban", "Milwaukee River", 
                           "Milwaukee River", "Subwatersheds", "Subwatersheds")
  names(model_summary) <- make.names(names(model_summary))
  
  Header1 <- c("", "", "Human Bacteroides", "Human Bacteroides", 
               "Lachnospiraceae", "Lachnospiraceae", 
               "Enterococci","Enterococci", 
               "Enterococci Culture", "Enterococci Culture", 
               "E. coli", "E. coli")
  
  Header2 <- c("Sites", "Category", rep(c("Variables","RMSE"),5))
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
  
  return(table1)
}