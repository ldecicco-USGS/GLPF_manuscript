create_report <- function(input, input_dir, output_format, ...){
  original_wd <- getwd()
  setwd(input_dir)
  x <- list(...)
  list2env(x, environment())
  bookdown::render_book(input = input, 
              output_format = output_format)
  
  setwd(original_wd)
}


