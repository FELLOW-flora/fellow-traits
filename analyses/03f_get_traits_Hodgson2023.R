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
# if the script is not run from make.R, need to load home made functions (e.g. clean_species_list())
devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load species list with taxonomy
taxolist <- read.csv(
  here::here("data", "derived-data", "species_short_list.csv")
)
# and synonyms
synonyms <- read.csv(
  here::here("data", "derived-data", "species_known_synonyms.csv")
)

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

# 1. Extract trait values ----------------------

out <- extract_trait_taxalist(
  trait_df = hodgson,
  trait_sp = "Species",
  meta_trait = meta[meta$database %in% "Hodgson2023", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 48.24 %
names(out)[-1] <- paste(names(out)[-1], "Hodgson2023", sep = "_")


# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitF_hodgson2023.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
