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
clean_taxo <- function(x){
  # remove everything in bracket
  x <- gsub("\\((.+?)\\)", "", x)
  x <- gsub("\\\"", "", x)
  x <- gsub("\\.", "", x)
  x <- gsub(" \\?", "", x)
  x <- gsub(" complex", "", x)
  x <- gsub(" sp$", "", x)
  x <- gsub(" cf$", "", x)
  x <- gsub(" f$", "", x)
  x <- gsub(" gpe$", "", x)
  x <- gsub(" sp$", "", x)
  x <- gsub(" spp$", "", x)
  x <- gsub(" agg$", "", x)
  x <- gsub(" sstr$", "", x)
  x <- gsub(" spp ", "", x)
  x <- gsub(" ssp ", "", x)
  x <- gsub(" subsp ", " ", x)
  x <- gsub(" var ", " ", x)
  x <- gsub(" sl$", "", x)
  x <- gsub(" $", "", x)
  x <- gsub("  ", " ", x)
  # if author name (detected by years), remove them
  if (any(grepl("[1-9]", x))){
    x<- clean_ref(x)
  }
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
clean_ref <- function(x){
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
rm_ref <- function(x){
  # remove words staring with upper letter (but not the two first one)
  up <- grepl("\\b[A-Z].*?\\b", x)
  up[1] <- FALSE
  if(length(up)>1) {up[2] <- FALSE}
  # remove words with numbers
  num <- grepl("[1-9]", x)
  # remove words with special characters (punctuation)
  pun <- grepl("[,&]", x)
  # remove 'and'
  and <- grepl("^and$", x)
  # remove 'ex'
  and <- grepl("^ex$", x)

  return(paste(x[!(up|num|pun|and)], collapse = " "))   
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
clean_species_list <- function(x, iter=TRUE){
  if(iter){
    clean_x <- x
    while(any(clean_taxo(clean_x)!=clean_x)){
      clean_x <- clean_taxo(clean_x)
    }
  } else {
    clean_x <- clean_taxo(x)
  }
  return(clean_x)
}
