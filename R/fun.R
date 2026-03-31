bib2df <- function(file, separate_names = FALSE, merge_lines = FALSE) {
  
  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (grepl("http://|https://|www.", file)) {
    tryCatch(
      { GET(file) },
      error = function(e) {
        stop("Invalid URL: File is not readable.", call. = FALSE)
      }
    )
  } else {
    if (as.numeric(file.access(file, mode = 4)) != 0) {
      stop("Invalid file path: File is not readable.", call. = FALSE)
    }
  }
  
  
  bib <- bib2df_read(file)
  if(merge_lines == TRUE){bib <- bib2df_merge_lines(bib)}
  bib <- bib2df_gather(bib)
  bib <- bib2df_tidy(bib, separate_names)
  return(bib)
}

text_between_curly_brackets <- function(string) {
  
  string <- trimws(string)
  
  if(grepl(".,$",string)){ # remove ending comma, if any
    string <- substring(string,1,nchar(string)-1)
  }
  content <- string
  
  if(grepl("^\\{",string) & grepl(".\\}$",string)){ # remove leading and ending brackets, if any
    min <- min(gregexpr("\\{", string)[[1]])
    max <- max(gregexpr("\\}", string)[[1]])
    content <- substring(string, min + 1, max - 1)
  }
  return(content)
}


bib2df_read <- function(file) {
  bib <- readLines(file)
  bib <- str_replace_all(bib, "[^[:graph:]]", " ")
  bib <- str_replace_all(bib, "=", " = ") # add desired spaces
  bib <- str_replace_all(bib, "  ", " ")   # remove double spaces in case you have it
  return(bib)
}


bib2df_gather <- function(bib) {
  
  from <- which(str_extract(bib, "[:graph:]") == "@")
  to  <- c(from[-1] - 1, length(bib))
  if (!length(from)) {
    return(empty)
  }
  itemslist <- mapply(
    function(x, y) return(bib[x:y]),
    x = from,
    y = to - 1,
    SIMPLIFY = FALSE
  )
  keys <- lapply(itemslist,
                 function(x) {
                   str_extract(x[1], "(?<=\\{)[^,]+")
                 }
  )
  fields <- lapply(itemslist,
                   function(x) {
                     str_extract(x[1], "(?<=@)[^\\{]+")
                   }
  )
  fields <- lapply(fields, toupper)
  
  categories <- lapply(itemslist,
                       function(x) {
                         str_extract(x, "[[:alnum:]_-]+")
                       }
  )
  
  # categories <- lapply(categories, trimws)
  
  dupl <- sum(
    unlist(
      lapply(categories, function(x) sum(duplicated(x[!is.na(x)])))
    )
  )
  
  if (dupl > 0) {
    warning("There were issues with the syntax of the bibTeX.file.
            Some BibTeX entries may have been dropped.
            The result could be malformed.
            Review the .bib file and make sure every single entry starts
            with a '@' and that there are no duplicates entries (same key) or duplicate fields within an entry.")
  }
  
  values <- lapply(itemslist,
                   function(x) {
                     str_extract(x, "(?<==).*")
                   }
  )
  
  values <- lapply(values,
                   function(x) {
                     sapply(x, text_between_curly_brackets, simplify = TRUE, USE.NAMES = FALSE)
                   }
  )
  
  values <- lapply(values, trimws)
  items <- mapply(cbind, categories, values, SIMPLIFY = FALSE)
  items <- lapply(items,
                  function(x) {
                    x <- cbind(toupper(x[, 1]), x[, 2])
                  }
  )
  items <- lapply(items,
                  function(x) {
                    x[complete.cases(x), ]
                  }
  )
  items <- mapply(function(x, y) {
    rbind(x, c("CATEGORY", y))
  },
  x = items, y = fields, SIMPLIFY = FALSE)
  
  items <- lapply(items, t)
  items <- lapply(items,
                  function(x) {
                    colnames(x) <- x[1, ]
                    x <- x[-1, ]
                    return(x)
                  }
  )
  items <- lapply(items,
                  function(x) {
                    x <- t(x)
                    x <- data.frame(x, stringsAsFactors = FALSE)
                    return(x)
                  }
  )
  dat <- bind_rows(c(list(empty), items))
  dat <- as_tibble(dat)
  dat$BIBTEXKEY <- unlist(keys)
  dat
}

empty <- data.frame(
  CATEGORY = character(0L),
  BIBTEXKEY = character(0L),
  ADDRESS = character(0L),
  ANNOTE = character(0L),
  AUTHOR = character(0L),
  BOOKTITLE = character(0L),
  CHAPTER = character(0L),
  CROSSREF = character(0L),
  EDITION = character(0L),
  EDITOR = character(0L),
  HOWPUBLISHED = character(0L),
  INSTITUTION = character(0L),
  JOURNAL = character(0L),
  KEY = character(0L),
  MONTH = character(0L),
  NOTE = character(0L),
  NUMBER = character(0L),
  ORGANIZATION = character(0L),
  PAGES = character(0L),
  PUBLISHER = character(0L),
  SCHOOL = character(0L),
  SERIES = character(0L),
  TITLE = character(0L),
  TYPE = character(0L),
  VOLUME = character(0L),
  YEAR = character(0L),
  stringsAsFactors = FALSE
)


bib2df_tidy <- function(bib, separate_names = FALSE) {
  
  if (dim(bib)[1] == 0) {
    return(bib)
  }
  
  AUTHOR <- EDITOR <- YEAR <- CATEGORY <- NULL
  if ("AUTHOR" %in% colnames(bib)) {
    bib <- bib %>%
      dplyr::mutate(AUTHOR = strsplit(AUTHOR, " and ", fixed = TRUE))
    if (separate_names) {
      bib$AUTHOR <- lapply(bib$AUTHOR, function(x) x %>%
                             format_reverse() %>%
                             format_period() %>%
                             parse_names())
    }
  }
  if ("EDITOR" %in% colnames(bib)) {
    bib <- bib %>%
      mutate(EDITOR = strsplit(EDITOR, " and ", fixed = TRUE))
    if (separate_names) {
      bib$EDITOR <- lapply(bib$EDITOR, function(x) x %>%
                             format_reverse() %>%
                             format_period() %>%
                             parse_names())
    }
  }
  if ("YEAR" %in% colnames(bib)) {
    if (sum(is.na(as.numeric(bib$YEAR))) == 0) {
      bib <- bib %>%
        dplyr::mutate(YEAR = as.numeric(YEAR))
    } else {
      message("Column `YEAR` contains character strings.
              No coercion to numeric applied.")
    }
  }
  bib <- bib %>%
    dplyr::select(CATEGORY, dplyr::everything())
  return(bib)
}

df2bib <- function(x, file = "", append = FALSE, allfields = TRUE) {
  
  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (as.numeric(file.access(dirname(file), mode = 2)) != 0 && file != "") {
    stop("Invalid file path: File is not writeable.", call. = FALSE)
  }
  
  if (any({df_elements <- sapply(x$AUTHOR, inherits, "data.frame")})) {
    x$AUTHOR[df_elements] <- lapply(x$AUTHOR[df_elements], na_replace)
    x$AUTHOR[df_elements] <- lapply(x$AUTHOR[df_elements],
                                    function(x) {
                                      paste(x$last_name,
                                            ", ",
                                            x$first_name,
                                            " ",
                                            x$middle_name,
                                            sep = "")
                                    }
    )
    x$AUTHOR[df_elements] <- lapply(x$AUTHOR[df_elements], trimws)
  }
  
  #names(x) <- capitalize(names(x))
  names(x) <- toupper(names(x))
  fields <- lapply(seq_len(nrow(x)), function(r) {
    rowfields <- rep(list(character(0)), ncol(x))
    names(rowfields) <- names(x)
    for (i in seq_along(rowfields)) {
      f <- x[[i]][r]
      if (is.list(f)) {
        f <- unlist(f)
      }
      rowfields[[i]] <- if (!length(f) | any(is.na(f))) {
        character(0L)
      } else if (names(x)[i] %in% c("AUTHOR", "EDITOR")) {
        paste(f, collapse = " and ")
      } else {
        paste0(f, collapse = ", ")
      }
    }
    rowfields <- rowfields[lengths(rowfields) > 0]
    rowfields <- rowfields[!names(rowfields) %in% c("CATEGORY", "BIBTEXKEY")]
    ######################################################################################
    # This only uses the non-empty fields for the bib file (adds '[nzchar(rowfields)]')
    # if 'allfields' is set to FALSE
    if(allfields) {
      paste0("  ",
             names(rowfields),
             " = {",
             unname(unlist(rowfields)),
             "}",
             collapse = ",\n")
    } else {
      paste0("  ",
             names(rowfields[nzchar(rowfields)]),
             " = {",
             unname(unlist(rowfields[nzchar(rowfields)])),
             "}",
             collapse = ",\n")
    }
    ######################################################################################
  })
  cat(paste0("@",
             capitalize(x$CATEGORY),
             "{",
             x$BIBTEXKEY,
             ",\n",
             unlist(fields),
             "\n}\n",
             collapse = "\n\n"),
      file = file,
      append = append)
  invisible(file)
}

capitalize <- function(string) {
  paste0(substr(string, 1, 1),
         tolower(substr(string, 2, nchar(string))))
}

na_replace <- function(df) {
  df[is.na(df)] <- ""
  return(df)
}
