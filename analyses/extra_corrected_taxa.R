# Get the species taxonomic information of misspelt species
#
# input:
#    species list in derived-data/species_list_taxo.csv
#    raw species list in derived-data/species_list_raw.csv
#
# output: derived-data/corrected_taxa.csv

devtools::load_all()

# might be good to give information back to data provider
# about identified_misspelt_species, per dataset?
taxolist <- read.csv(
  here::here("data", "derived-data", "species_list_taxo.csv")
)

# Load dataset
sp <- read.csv(here::here("data", "derived-data", "species_list_raw.csv"))

# harmonize the species names
sp$clean_taxo <- clean_species_list(sp$taxa)

m1 <- match(sp$clean_taxo, taxolist$original_taxa)
sp$accepted_taxa <- taxolist$accepted_taxa[m1]

notTR <- taxolist$taxref_status[m1] %in% "NOT FOUND"
different <- clean_species_list(sp$accepted_taxa) != sp$clean_taxo
# get the misspelt information
misspelt <- sp[which(different | is.na(m1) | notTR), ]
misspelt <- misspelt[!is.na(misspelt$taxa), ]

m2 <- match(misspelt$clean_taxo, taxolist$original_taxa)

# summary for Taxref
misspelt$in_taxref <- taxolist$taxref_status[m2]
misspelt$in_taxref[is.na(misspelt$in_taxref)] <- "NOT FOUND"
# table(misspelt$in_taxref, useNA = "ifany")

misspelt$accepted_taxref <- taxolist$accepted_taxref[m2]

# get original information from GBIF
ingbif <- rgbif::name_backbone_checklist(misspelt$clean_taxo, strict = TRUE)
misspelt$in_gbif <- ifelse(
  is.na(ingbif$species),
  gsub("ACCEPTED", "TAXREF", taxolist$gbif_status[m2]),
  paste0("EXACT_", ingbif$status)
)
misspelt$in_gbif <- gsub("HIGHERRANK_TAXREF", "HIGHERRANK", misspelt$in_gbif)
misspelt$in_gbif[is.na(misspelt$in_gbif)] <- "NOT FOUND"
# add TOO COARSE for accepted and NA

misspelt$accepted_gbif <- ifelse(
  is.na(ingbif$species),
  taxolist$accepted_gbif[m2],
  ingbif$species
)

misspelt$in_gbif[is.na(ingbif$species)]

names(misspelt)[c(1, 4)] <- c("original_taxa", "simplified_taxa")

write.csv(
  misspelt,
  here::here("data", "derived-data", "corrected_taxa.csv"),
  row.names = FALSE
)
