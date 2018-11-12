# render files
# knitr::knit("file.Rmd") does not remove intermediate .md and .png files

# rmarkdown::render("index.Rmd")
# rmarkdown::render("01_Introduccion.Rmd")
# rmarkdown::render("02_Regresion_Lineal_Simple.Rmd")

# render the whole dir
files = dir()
for (i in 1:length(files)){
  file1 = substr(files[i],nchar(files[i])-2,nchar(files[i]))
  if (file1 == "Rmd"){
    print(files[i])
    #rmarkdown::render(file1)
  }
}
