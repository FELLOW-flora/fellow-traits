# Get trait information from https://www.cahiersagricultures.fr/articles/cagri/pdf/2021/01/cagri210051.pdf
# input:
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    trait data in raw-data/traits/FlorealData/FlorealData.xlsx
#    taxref in raw-data/TAXREF_v18_2025/TAXREFv18.txt
#    metadata in traits/Metatraits.xlsx
# output: traitH_FlorealData.csv

# check out documentation at https://github.com/BioGeoMacro/GIFT

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all()


# Load species list with taxonomy
taxolist <- read.csv(
  here::here("data", "derived-data", "species_short_list.csv")
)

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

# Load TaxRef
taxref <- read.table(
  here::here("data", "raw-data", "TAXREF_v18_2025", "TAXREFv18.txt"),
  sep = "\t",
  header = TRUE
)
taxref$clean_name <- clean_species_list(taxref$LB_NOM)

# check out GIFT metadata
floreal <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "FlorealData", "FlorealData.xlsx")
)


# 1. Extract trait values ----------------------
# clean species name
m_tr <- match(floreal$CD_REF, taxref$CD_REF)
floreal$clean_name <- ifelse(
  is.na(m_tr),
  clean_species_list(floreal$NOM_VALIDE),
  taxref$clean_name[match(floreal$CD_REF, taxref$CD_REF)]
)

out <- extract_trait_taxalist(
  trait_df = floreal,
  trait_sp = "clean_name",
  meta_trait = meta[meta$database %in% "FlorealData", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 31.83 %
names(out)[-1] <- paste(names(out)[-1], "FlorealData", sep = "_")


# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitH_FlorealData.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(!is.na(out), 2, sum))
