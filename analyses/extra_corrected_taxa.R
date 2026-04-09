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

# notTR <- taxolist$wfo_status[m1] %in% "NOT FOUND"
# sp$accepted_taxa[is.na(sp$accepted_taxa)] <- "NA"
different <- clean_species_list(sp$accepted_taxa) != sp$clean_taxo
# get the misspelt information
misspelt <- sp[which(different | is.na(m1)), ] #| notTR
misspelt <- misspelt[!is.na(misspelt$taxa), ]

m2 <- match(misspelt$clean_taxo, taxolist$original_taxa)

# summary for WFO
misspelt$in_WFO <- taxolist$wfo_status[m2]
misspelt$in_WFO[is.na(misspelt$in_WFO)] <- "NOT FOUND"
table(misspelt$in_WFO, useNA = "ifany")

misspelt$match <- taxolist$wfo_match[m2]
misspelt$match[is.na(misspelt$match)] <- "NOT FOUND"
table(misspelt$match, useNA = "ifany")

misspelt$accepted_wfo <- taxolist$accepted_wfo[m2]


names(misspelt)[c(1, 4)] <- c("original_taxa", "simplified_taxa")
# View(misspelt)
write.csv(
  misspelt,
  here::here("data", "derived-data", "corrected_taxa.csv"),
  row.names = FALSE
)
