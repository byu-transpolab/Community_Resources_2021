rmarkdown::render("poster/food_insecurity.Rmd", 
                 output_dir = "poster", 
                 output_format = "posterdown::posterdown_betterland")
pagedown::chrome_print("food_insecurity_poster.Rmd", 
                       output = "food_insecurity.pdf")
