# small handy functions
concat <- function(x, sep=",") {
  paste(sort(unique(tolower(x))), collapse = sep)
}
