####### USEFUL TAGS IN bib (TO ADD IN ZOTERO) ######
# "preprint", R package", "dataset", "code", "taxonomy", "Lecture"

# Install if needed
# devtools::install_github("ropensci/RefManageR")
# library(knitr)
# library()


# To see the bib file as a df
# bibdf <- bib2df(paste0(getwd(),"/Rmarkdown/REFERENCES.bib"))


####### GENERATE SITE PAGES
# First modify the .Rmd files in /Rmarkdown folder then run the code

# pages to update (names of .Rmd files)
pages <- c("articles", "opensciences", "taxo", "cv", "about", "expbites") 

for (page in pages){
  file_in <- paste0(getwd(),"/Rmarkdown/",page,".Rmd")
  file_out <- paste0(getwd(),"/_pages/",page,".md")
  
  knitr::knit(file_in,file_out)
  
  # remove html tags <a...>
  if (page %in% c("articles", "opensciences", "taxo")){
    txt <- readLines(file_out, encoding = "UTF-8")
    
    # collapse
    txt <- paste(txt, collapse = "\n")
    
    # Supprimer les balises HTML <a...>
    txt_clean <- gsub("</?a[^>]*>", "", txt)
    
    # Supprimer les lien markdown
    txt_clean <- gsub("\\[([^]]+)\\]\\(#[^)]+\\)", "\\1", txt_clean)
    
    # Supprimer les numerotation auto markdown 
    txt_clean <- gsub("\\n(\\d+.\\s)","\\1", txt_clean)
    
    # split
    txt_clean <- strsplit(txt_clean, "\n")[[1]]
    
    # Écrire le résultat
    writeLines(txt_clean, file_out, useBytes = TRUE)
  }

  
}
######### END

