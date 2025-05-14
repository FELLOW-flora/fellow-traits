# Get trait information from Hogson et al. 2024
# DOI: 10.5287/ora-pp4y9nkoz
#
# input:
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    traits in traits/Hodgson_2023/Functional+trait+database+of+arable+weeds+from+Eurasia+and+North+Africa.xlsx
#    metadata in traits/Metatraits.xlsx
# output: traitF_hodgson2023.csv

# 0. Load packages, data, set parameters ----------------------
# if the script is not run from make.R, need to load home made functions (e.g. clean_taxo())
devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load species list with taxonomy
taxolist <- read.csv(here::here(
  "data",
  "derived-data",
  "species_list_taxo.csv"
))
# and synonyms
synonyms <- read.csv(here::here(
  "data",
  "derived-data",
  "species_known_synonyms.csv"
))

# load metadata of traits (defining which traits are kept)
meta <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "Metatraits.xlsx")
)

# load the trait database : baseflor
hodgson <- readxl::read_xlsx(
  here::here(
    "data",
    "raw-data",
    "traits",
    "Hodgson_2023",
    "Functional+trait+database+of+arable+weeds+from+Eurasia+and+North+Africa.xlsx"
  )
)

# 1. Match species names ----------------------
# clean species name
hodgson$taxa <- clean_species_list(hodgson$Species, iter = TRUE)

# replace if known synonyms
is_syn <- hodgson$taxa %in% synonyms$synonym_taxa
hodgson$taxa[is_syn] <- synonyms$accepted_taxa[match(
  hodgson$taxa[is_syn],
  synonyms$synonym_taxa
)]


m1 <- match(taxolist$original_taxa, hodgson$taxa)
m2 <- match(taxolist$accepted_taxref, hodgson$taxa)
m3 <- match(taxolist$accepted_gbif, hodgson$taxa)
m_hodgson <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
print(prop.table(table(!is.na(m_hodgson)))) #49%

# 2. Format trait data ---------------------------
keepT <- meta$original.name[meta$database %in% "Hodgson2023"]
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "Hodgson2023"]),
  "Hodgson2023",
  sep = "_"
)

out <- cbind(
  taxolist$accepted_taxa,
  hodgson$Species[m_hodgson],
  hodgson[m_hodgson, keepT]
)
names(out) <- c("accepted_taxa", newlab)


# 3. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitF_hodgson2023.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
