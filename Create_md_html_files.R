require(knitr) # required for knitting from Rmd to md
require(markdown) # required for md to HTML
require(knitr) # required to convert from Rmd to HTML

# creates md file
knit('Reproducible Reseasrch Project One.Rmd', 'Reproducible Reseasrch Project One.md')

# because Reproducible Reseasrch Project One.Rmd appears to be an R Markdown v2 document.
# used mkardownToHTML function to create html file
markdownToHTML('Reproducible Reseasrch Project One.md', 'Reproducible_Reseasrch_Project_One.html') 

#open file in browser 
browseURL(paste('file://', file.path(getwd(),'Reproducible_Reseasrch_Project_One.html'), sep='')) 

