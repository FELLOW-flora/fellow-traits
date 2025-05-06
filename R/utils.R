# small handy functions
concat <- function(x, sep = ",") {
  paste(sort(unique(tolower(x))), collapse = sep)
}

paste_unique <- function(x) {
  paste(sort(unique(tolower(x))), collapse = ",")
}
