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

# 1. Extract trait values ----------------------
# remove accent
dcube$name <- iconv(dcube$name, from = "latin1", to = "ASCII//TRANSLIT")

out <- extract_trait_taxalist(
  trait_df = dcube,
  trait_sp = "name",
  meta_trait = meta[meta$database %in% "Dcube", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 55%
names(out)[-1] <- paste(names(out)[-1], "Dcube", sep = "_")


# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitE_dcube.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
