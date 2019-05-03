library(rmarkdown)

rmarkdown::render(input = './src/Markdown/ru-salaries.Rmd',
                  output_dir = './reports/',
                  output_file = 'ru-salaries.html')
