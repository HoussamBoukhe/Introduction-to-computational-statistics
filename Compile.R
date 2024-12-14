library(knitr) # Compile the code 
knit("Course.Rnw")
system("pdflatex Course.tex")
