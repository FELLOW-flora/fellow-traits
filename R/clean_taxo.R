#' Clean taxonomic name
#'
#' @description
#' Remove anything characters in bracket, unnecessary quotes, and abbreviations (e.g. 'sp.')
#' If references follow the latin name, it will be removed using `clean_ref()` function
#'
#' @param x a vector of `characters` with the latin names of taxa
#'
#' @returns A vector of `characters` with the homogenized names
#'
#' @export
clean_taxo <- function(x) {
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # remove everything in bracket
  x <- gsub("\\((.+?)\\)", "", x)
  # or square bracket
  x <- gsub("\\[(.+?)\\]", "", x)
  # simplify alphabet
  x <- gsub("×", "x ", x)
  x <- gsub("\\\"", "", x)
  x <- gsub("\\.", "", x)
  x <- gsub("\\*", "", x)
  x <- gsub(" \\?", "", x)
  x <- gsub("_", " ", x)
  x <- gsub("^Cf ", "", x)
  x <- gsub(" complex$", "", x)
  x <- gsub(" convar$", "", x)
  x <- gsub(" sp$", "", x)
  x <- gsub(" cf$", "", x)
  x <- gsub(" f$", "", x)
  x <- gsub(" gpe$", "", x)
  x <- gsub(" sp$", "", x)
  x <- gsub(" spp$", "", x)
  x <- gsub(" agg$", "", x)
  x <- gsub(" sstr$", "", x)
  x <- gsub(" sect$", "", x)
  x <- gsub(" subsect$", "", x)
  x <- gsub(" section$", "", x)
  x <- gsub(" cf ", " ", x)
  x <- gsub(" f ", " ", x)
  x <- gsub(" spp ", " ", x)
  x <- gsub(" ssp ", " ", x)
  x <- gsub(" subsp ", " ", x)
  x <- gsub(" var ", " ", x)
  x <- gsub(" sl$", "", x)
  x <- gsub(" l$", "", x)
  x <- gsub(" $", "", x)
  x <- gsub("  ", " ", x)
  # if author name (detected by years), remove them
  if (any(grepl("[1-9]", x))) {
    x <- clean_ref(x)
  }
  # make sure it is correctly capitalize
  x <- firstup(x)
  # return clean species name
  return(x)
}

#' Clean references from latin name of taxa
#'
#' @description
#' Remove author names (identified with first letter in uppercase) and years
#'
#' @param x a vector of `characters` with the latin names of taxa
#'
#' @returns A vector of `characters` with the homogenized names
#'
#' @export
clean_ref <- function(x) {
  # split the words
  lx <- strsplit(x, " ")
  # remove author names and merge them
  return(sapply(lx, rm_ref))
}


#' Remove family names and years from a vector of separated words
#'
#' @param x a vector of `characters` representing a single taxa name with words separated as different elements
#'
#' @returns A single `character` with name of taxa without reference
#'
#' @export
rm_ref <- function(x) {
  # remove words staring with upper letter (but not the two first one)
  up <- grepl("\\b[A-Z].*?\\b", x)
  up[1] <- FALSE
  if (length(up) > 1) {
    up[2] <- FALSE
  }
  # remove words with numbers
  num <- grepl("[1-9]", x)
  # remove words with special characters (punctuation)
  pun <- grepl("[,&]", x)
  # remove 'and'
  and <- grepl("^and$", x)
  # remove 'ex'
  and <- grepl("^ex$", x)

  return(paste(x[!(up | num | pun | and)], collapse = " "))
}

#' Clean species list
#'
#' @description
#' Harmonize taxonomic names with `clean_taxo()` and iterate multiple time if needed.
#'
#' @param x a vector of `characters` with the latin names of taxa
#' @param iter a logical element indicating whether the cleaning should be repeated
#'
#' @returns A vector of `characters` with the homogenized names
#'
#' @export
clean_species_list <- function(x, iter = TRUE) {
  if (iter) {
    out_x <- x
    new_x <- clean_taxo(x)
    change <- new_x != out_x
    if (any(change)) {
      # in TRUE to handle NAs
      out_x[change %in% TRUE] <- clean_species_list(new_x[change %in% TRUE])
    }
  } else {
    out_x <- clean_taxo(x)
  }
  return(out_x)
}

# and remove 'x'
get_binomial <- function(x) {
  strsplit(x, " ") |>
    sapply(first_second)
}

get_genus <- function(x) {
  strsplit(x, " ") |>
    sapply(function(i) i[1])
}

first_second <- function(x, sep = " ") {
  # remove 'x' if any
  x <- x[!x %in% 'x']
  # get the first and secong
  if (length(x) > 1) {
    return(paste(x[1], x[2], sep = sep))
  } else {
    return(x[1])
  }
}

firstup <- function(x, rm_hybrid = TRUE) {
  x <- tolower(x)
  if (rm_hybrid) {
    start_x <- grepl("^x ", x)
    substr(x, 1, 1) <- ifelse(
      start_x,
      substr(x, 1, 1),
      toupper(substr(x, 1, 1))
    )
    substr(x, 3, 3) <- ifelse(
      start_x,
      toupper(substr(x, 3, 3)),
      substr(x, 3, 3)
    )
  } else {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  }
  return(x)
}
