clean_bib <- function(file_out){
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
  
  return(txt_clean)
  
}