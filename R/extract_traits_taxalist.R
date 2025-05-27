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
  verbatim = TRUE,
  quiet = TRUE
) {
  # many checks need to be defined
  # for instance
  # check that synonyms are not looping
  # check that all arguments have the good format
  # check that data.frames have the needed column names
  # return error if taxalist and trait_sp doesn't match at all
  # remove NA from taxalist
  # check correspondance taxalist and synonyms
  #
  trait_df <- as.data.frame(trait_df)
  #
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

  # match species names (including multiple matches)
  match_df <- matches_taxa(taxalist, trait_df$clean_taxa, as_df = TRUE)
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
    if (meta_trait$type[i] == "numeric") {
      if (quiet) {
        match_i <- suppressWarnings(as.numeric(match_i))
      } else {
        match_i <- as.numeric(match_i)
      }
      trait_i <- tapply(match_i, match_df$x, mean, na.rm = TRUE)
    } else {
      trait_i <- tapply(as.character(match_i), match_df$x, concat)
    }
    out[meta$new.name[i]] <- trait_i
  }
  # complete with full taxalist
  full_out <- out[match(taxalist, out$accepted_taxa), ]
  full_out$accepted_taxa <- taxalist

  return(full_out)
}
