create_report <- function(...){
  
  input = "index.Rmd"
  input_dir = file.path("report","full_report")
  output_format = "bookdown::gitbook"
  
  original_wd <- getwd()
  setwd(input_dir)
  x <- list(...)
  list2env(x, environment())
  bookdown::render_book(input = input, 
              output_format = output_format)
  
  setwd(original_wd)
}


