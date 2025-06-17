# Get trait information from https://gift.uni-goettingen.de/home
# input:
#    species list in species_list_taxo.csv
#    list of interesting traits in GIFT_metadata_filled.csv
# output: so far only percentage of coverage

# check out documentation at https://github.com/BioGeoMacro/GIFT

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all() # or source(here::here("R", "clean_taxo.R"))
# library("GIFT")

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

# check out GIFT metadata
meta_lvl3 <- GIFT::GIFT_traits_meta()
gift_meta <- read.csv(
  here::here("data", "raw-data", "traits", "GIFT", "GIFT_metadata_filled.csv")
)
# weird renaming of lvl3 code ...
gift_meta$Lvl3 <- meta_lvl3$Lvl3[match(gift_meta$Trait2, meta_lvl3$Trait2)]

# download the 32 interesting traits
# db_gift <- GIFT::GIFT_traits(
#   trait_IDs = gift_meta$Lvl3[gift_meta$interesting == 1],
#   agreement = 0.66,
#   bias_ref = FALSE,
#   bias_deriv = FALSE
# )
# dim(db_gift) # 206042    131
# saveRDS(
#   db_gift,
#   here::here("data", "raw-data", "traits", "GIFT_sptraits.rds")
# )
db_gift <- readRDS(
  here::here("data", "raw-data", "traits", "GIFT", "GIFT_sptraits.rds")
)

# 1. Extract trait values ----------------------
col_traits <- grep("^trait_value", names(db_gift))
lv3_traits <- gsub("^trait_value_", "", names(db_gift)[col_traits])
lab_traits <- gift_meta$Trait2[match(lv3_traits, gift_meta$Lvl3)]
names(db_gift)[col_traits] <- lab_traits

out <- extract_trait_taxalist(
  trait_df = db_gift,
  trait_sp = "work_species",
  meta_trait = meta[meta$database %in% "GIFT", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 95.37 %
names(out)[-1] <- paste(names(out)[-1], "GIFT", sep = "_")


# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitG_GIFT.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
