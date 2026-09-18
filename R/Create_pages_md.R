####### USEFUL TAGS IN bib (TO ADD IN ZOTERO) ######
# "preprint", "R package", "dataset", "code", "taxonomy", "Lecture"

# Install if needed
# devtools::install_github("ropensci/RefManageR")
# library(knitr)
# library()


# To see the bib file as a df
# bibdf <- bib2df(paste0(getwd(),"/Rmarkdown/REFERENCES.bib"))


####### GENERATE SITE PAGES
# First modify the .Rmd files in /Rmarkdown folder then run the code

# pages to update (names of .Rmd files)
pages <- c("articles", "opensciences", "taxo", "cv", "about") 

# page <- "opensciences"

for (page in pages){
  file_in <- paste0(getwd(),"/Rmarkdown/",page,".Rmd")
  file_out <- paste0(getwd(),"/_pages/",page,".md")
  
  knitr::knit(file_in,file_out)
  
}
######### END

