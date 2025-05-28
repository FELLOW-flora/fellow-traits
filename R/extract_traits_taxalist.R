#' Main function to extract traits
#'
#' @description
#' tbd
#'
#' @param trait_df data.frame with trait information
#' @param trait_sp column name with taxonomic information
#' @param meta_trait metadata of the trait information to extract
#' @param taxa_list list of taxa of interest
#' @param synonyms known synonyms of the taxa_list
#' @param look_subsp whether taxa are simplified to binomial if no trait found
#' @param verbatim print information
#' @param quiet remove warning from as.numeric()
#'
#' @returns A vector of `characters` with the homogenized names
#'
#' @export

extract_trait_taxalist <- function(
  trait_df,
  trait_sp,
  meta_trait,
  taxalist,
  synonyms,
  look_subsp = TRUE,
  look_genus = FALSE,
  max_matches = 10,
  verbatim = TRUE,
  quiet = TRUE,
  clean_taxalist = FALSE,
  clean_synonyms = FALSE
) {
  # many checks need to be defined
  # first make sure there are data.frames
  trait_df <- as.data.frame(trait_df)
  meta_trait <- as.data.frame(meta_trait)
  synonyms <- as.data.frame(synonyms)
  # for instance
  # check that all arguments have the good format
  if (!is.character(trait_sp) | length(trait_sp) != 1) {
    stop("Argument 'trait_sp' must be a character of length 1", call. = FALSE)
  }
  if (!trait_sp %in% names(trait_df)) {
    stop("Argument 'trait_sp' is not found in 'trait_df'", call. = FALSE)
  }
  # check that data.frames have the needed column names
  if (!all(c("original.name", "type", "new.name") %in% names(meta_trait))) {
    msg <- "The columns 'original.name', 'type' and 'new.name' are not found in 'meta_trait'"
    stop(msg, call. = FALSE)
  }
  if (!all(c("synonym_taxa", "accepted_taxa") %in% names(synonyms))) {
    msg <- "The columns 'synonym_taxa' and 'accepted_taxa' are not found in 'synonyms'"
    stop(msg, call. = FALSE)
  }
  if (!all(meta_trait$original.name %in% names(trait_df))) {
    miss <- which(!meta_trait$original.name %in% names(trait_df))
    msg <- paste(meta_trait$original.name[miss], "is missing from 'trait_df'.")
    warning(msg, call. = FALSE)
    meta_trait <- meta_trait[-miss, ]
  }
  if (nrow(meta_trait) == 0) {
    stop("The data.frame 'meta_trait' is empty", call. = FALSE)
  }
  # by default clean_taxalist = FALSE to make the extraction faster
  if (clean_taxalist) {
    taxalist <- clean_species_list(taxalist)
    taxalist <- taxalist[!is.na(taxalist)]
    taxalist <- taxalist[taxalist != ""]
    synonyms$synonym_taxa <- clean_species_list(synonyms$synonym_taxa)
    synonyms$accepted_taxa <- clean_species_list(synonyms$accepted_taxa)
  }
  # check and simplify synonyms, avoid loops
  if (clean_synonyms) {
    # keep only the usefull columns
    synonyms <- synonyms[, c("synonym_taxa", "accepted_taxa")]
    # keep only taxa in taxalist
    synonyms <- synonyms[synonyms$accepted_taxa %in% taxalist, ]
    # remove synonyms that are accepted taxa (avoid loops)
    synonyms <- synonyms[!synonyms$synonym_taxa %in% synonyms$accepted_taxa, ]
    # remove duplicates
    synonyms <- synonyms[!duplicated(synonyms), ]
    # remove synonyms with different accepted names
    rm_syn <- synonyms$synonym_taxa[duplicated(synonyms$synonym_taxa)]
    synonyms <- synonyms[!synonyms$synonym_taxa %in% rm_syn, ]
  }

  # clean species name
  trait_df$clean_taxa <- clean_species_list(trait_df[, trait_sp])
  # replace with synonyms
  is_syn <- trait_df$clean_taxa %in% synonyms$synonym_taxa
  m_syn <- match(trait_df$clean_taxa[is_syn], synonyms$synonym_taxa)
  trait_df$clean_taxa[is_syn] <- ifelse(
    synonyms$accepted_taxa[m_syn] %in% trait_df$clean_taxa,
    trait_df$clean_taxa[is_syn],
    synonyms$accepted_taxa[m_syn]
  )
  # return error if taxalist and trait_sp doesn't match at all
  if (!any(trait_df$clean_taxa %in% taxalist)) {
    stop("The species in 'trait_df' don't match 'taxalist'", call. = FALSE)
  }

  # match species names (including multiple matches)
  match_df <- matches_taxa(
    taxalist,
    trait_df$clean_taxa,
    binomial = look_subsp,
    genus = look_genus,
    n_max = max_matches,
    as_df = TRUE
  )
  # add matches with genus
  oneword_taxa <- taxalist[get_genus(taxalist) == taxalist]
  miss_gen <- oneword_taxa[!oneword_taxa %in% match_df$x]
  gen_df <- match_df[get_genus(match_df$x) %in% miss_gen, ]
  gen_df$x <- get_genus(gen_df$x)
  gen_df <- gen_df[!duplicated(gen_df), ]
  match_df <- rbind(match_df, gen_df)
  if (verbatim) {
    p <- prop.table(table(taxalist %in% match_df$x))
    p <- round(p[2] * 100, 2)
    print(paste("Taxa coverage of the trait database: ", p, "%"))
  }

  oritaxa <- unlist(trait_df[match_df$match, trait_sp])
  out <- data.frame(
    "accepted_taxa" = sort(unique(match_df$x)),
    "original_taxa" = tapply(oritaxa, match_df$x, concat)
  )
  for (i in 1:nrow(meta_trait)) {
    match_i <- unlist(trait_df[match_df$match, meta_trait$original.name[i]])
    if (tolower(meta_trait$type[i]) == "numeric") {
      if (quiet) {
        match_i <- suppressWarnings(as.numeric(match_i))
      } else {
        match_i <- as.numeric(match_i)
      }
      trait_i <- tapply(match_i, match_df$x, mean, na.rm = TRUE)
    } else {
      trait_i <- tapply(as.character(match_i), match_df$x, concat)
    }
    out[meta_trait$new.name[i]] <- trait_i
  }
  # complete with full taxalist
  full_out <- out[match(taxalist, out$accepted_taxa), ]
  full_out$accepted_taxa <- taxalist

  return(full_out)
}
