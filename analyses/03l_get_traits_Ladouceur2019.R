# Get trait information from  https://doi.org/10.1111/jvs.12784
# Ladouceur, E., Bonomi, C., Bruelheide, H., ... & Jiménez‐Alfaro, B. (2019).
# The functional trait spectrum of European temperate grasslands.
# Journal of Vegetation Science, 30(5), 777-788.
# input:
#    species list in species_short_list.csv
#    synonyms list in species_known_synonyms.csv
#    trait data in raw-data/traits/Ladouceur_2019/jvs12784-sup-0005-supinfo.csv
#    metadata in traits/Metatraits.xlsx
# output: traitL_Ladouceur2019.csv

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all()

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

# Load Ladouceur 2019
ladouceur <- read.csv(
  here::here(
    "data",
    "raw-data",
    "traits",
    "Ladouceur_2019",
    "jvs12784-sup-0005-supinfo.csv"
  )
)
# remove first line
ladouceur <- ladouceur[-1, ]

# 1. Extract trait values ----------------------
out <- extract_trait_taxalist(
  trait_df = ladouceur,
  trait_sp = "Accepted.Name",
  meta_trait = meta[meta$database %in% "Ladouceur2019", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 16.47 %
names(out)[-1] <- paste(names(out)[-1], "Ladouceur2019", sep = "_")


# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitL_Ladouceur2019.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(!is.na(out), 2, sum))
