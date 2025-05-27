#' Value matching from the last argument
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A vector of the same length as `x` containing
#' integers giving the position in `y` of the last match found (if any).
match_from_last <- function(x, y) {
  # match with the reverse vector
  m0 <- match(x, rev(y))
  # transform to get the original element order
  m1 <- length(y) - m0 + 1
  return(m1)
}

#' Testing if the match between two vectors is unique
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A vector of the same length as `x` containing
#' logical values indicating whether there are multiple matches in `y`
unique_match <- function(x, y) {
  # match with the same order
  m0 <- match(x, y)
  # match with opposite order
  m1 <- match_from_last(x, y)
  # compare both match
  return(m0 == m1)
}


#' List all matches
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A list of the same length as `x` containing
#' integers giving the positions in `y` of the all matches found (if any).
matches <- function(x, y) {
  lapply(x, function(i) which(y %in% i))
}


which_taxa <- function(i, ref, binomial = TRUE, genus = TRUE, n_max = 10) {
  # find all exact match
  out <- which(ref %in% i)
  # if none, binomial match (no subspecies)
  if (length(out) == 0 & binomial) {
    out <- which(get_binomial(ref) %in% get_binomial(i))
  }
  # if none, match genus
  if (length(out) == 0 & get_genus(i) == i & genus) {
    out <- which(get_genus(ref) %in% i)
  }
  # make sure that less than n_max matches are given, else none
  if (length(out) > n_max) {
    out <- integer(0)
  }
  return(out)
}

#' List all matches
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A list of the same length as `x` containing
#' integers giving the positions in `y` of the all matches found (if any).
matches_taxa <- function(
  x,
  y,
  binomial = TRUE,
  genus = TRUE,
  n_max = 5,
  as_df = FALSE
) {
  out <- lapply(
    x,
    which_taxa,
    ref = y,
    binomial = binomial,
    genus = genus,
    n_max = n_max
  )
  if (as_df) {
    out <- data.frame(
      "x" = rep(x, sapply(out, length)),
      "match" = unlist(out)
    )
  }
  return(out)
}
