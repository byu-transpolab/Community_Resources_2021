rmarkdown::render("poster/food_insecurity.Rmd", 
                 output_dir = "poster", 
                 output_format = "posterdown::posterdown_betterland")
pagedown::chrome_print("poster/food_insecurity.Rmd", 
                       output = "poster/food_insecurity.pdf")
