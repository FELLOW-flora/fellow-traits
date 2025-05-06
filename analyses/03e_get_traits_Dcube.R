# Get trait information from D3: Dispersal and Diaspore Database
# DOI: 10.1016/j.ppees.2013.02.001
#
# input:
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    traits in traits/baseflor.xlsx
#    metadata in traits/Metatraits.xlsx
# output: traitE_dcube.csv
# issue , as decimal ...
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
meta <- readxl::read_xlsx(here::here(
  "data",
  "raw-data",
  "traits",
  "Metatraits.xlsx"
))

# load the trait database : baseflor
dcube <- read.csv(
  here::here(
    "data",
    "raw-data",
    "traits",
    "D3_ScienceDirect_files",
    "1-s2.0-S1433831913000218-mmc1.txt"
  ),
  dec = ",",
  sep = ";",
  encoding = "latin1"
)

# 1. Match species names ----------------------
# remove accent
dcube$name <- iconv(dcube$name, from = "latin1", to = "ASCII//TRANSLIT")

# clean species name
dcube$taxa <- clean_species_list(dcube$name, iter = TRUE)

# replace if known synonyms
is_syn <- dcube$taxa %in% synonyms$synonym_taxa
dcube$taxa[is_syn] <- synonyms$accepted_taxa[match(
  dcube$taxa[is_syn],
  synonyms$synonym_taxa
)]


# m0 <- match(taxolist$accepted_taxa, dcube$taxa) # miss 19 taxa, but not sure what is the best strategy
m1 <- match(taxolist$original_taxa, dcube$taxa)
m2 <- match(taxolist$accepted_taxref, dcube$taxa)
m3 <- match(taxolist$accepted_gbif, dcube$taxa)
m_dcube <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
print(prop.table(table(!is.na(m_dcube)))) #55%

# 2. Format trait data ---------------------------

keepT <- meta$original.name[meta$database %in% "dcube"]
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "dcube"]),
  "Dcube",
  sep = "_"
)

out <- cbind(
  taxolist$accepted_taxa,
  dcube$name[m_dcube],
  dcube[m_dcube, keepT]
)
names(out) <- c("accepted_taxa", newlab)


# 3. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitE_dcube.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
