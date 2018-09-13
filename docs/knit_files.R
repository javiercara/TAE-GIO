# render files
# knitr::knit("file.Rmd") does not remove intermediate .md and .png files

rmarkdown::render("index.Rmd")
rmarkdown::render("01_Introduccion.Rmd")
rmarkdown::render("02_Regresion_Lineal_Simple.Rmd")
