# Get trait information from Baseflor
# received on 04/04/2025
# version from 07/03/2025: https://www.tela-botanica.org/projets/phytosociologie/porte-documents/
# input:
#    species list in species_short_list.csv
#    synonyms list in species_known_synonyms.csv
#    metadata in traits/Metatraits.xlsx
#    traits in traits/baseflor.xlsx
# output: traitB_baseflor.csv

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
baseflor <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "baseflor.xlsx")
)


# 1. Extract trait values ----------------------
# deal with NA in nomH
# baseflor$nomH <- baseflor$`Nom Phytobase`
# na_taxa <- is.na(baseflor$nomH)
# baseflor$nomH[na_taxa] <- clean_ref(baseflor$NOM_SCIENTIFIQUE[na_taxa])
baseflor$nomH <- clean_ref(baseflor$NOM_SCIENTIFIQUE)
# pre-clean species name
baseflor$nomH <- gsub(" \\*$", "", baseflor$nomH)

out <- extract_trait_taxalist(
  trait_df = baseflor,
  trait_sp = "nomH",
  meta_trait = meta[meta$database %in% "Baseflor", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 94.33 %
names(out)[-1] <- paste(names(out)[-1], "Baseflor", sep = "_")

# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitB_baseflor.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
