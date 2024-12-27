#library(knitr) # Compile the code 
#knit("Course.Rnw")
#system("pdflatex Course.tex")

library(knitr)
knit("Course.Rnw")  # First, knit the .Rnw file into .tex

# Compile using pdflatex
system("pdflatex Course.tex")

# Run bibtex to generate the bibliography
system("bibtex Course")

# Run pdflatex twice to resolve all references
system("pdflatex Course.tex")
system("pdflatex Course.tex")

#system("start Course.pdf")
