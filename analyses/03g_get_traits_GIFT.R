# Get trait information from https://gift.uni-goettingen.de/home
# input:
#    species list in species_list_taxo.csv
#    list of interesting traits in GIFT_metadata_filled.csv
# output: so far only percentage of coverage

# check out documentation at https://github.com/BioGeoMacro/GIFT

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_taxo())
devtools::load_all() # or source(here::here("R", "clean_taxo.R"))
library("GIFT")

# Load species list with taxonomy
taxolist <- read.csv(
  here::here("data", "derived-data", "species_list_taxo.csv")
)
# and synonyms
synonyms <- read.csv(here::here(
  "data",
  "derived-data",
  "species_known_synonyms.csv"
))

# check out GIFT metadata
meta <- GIFT_traits_meta()
gift_meta <- read.csv(
  here::here("data", "raw-data", "traits", "GIFT_metadata_filled.csv")
)
# weird renaming of lvl3 code ...
gift_meta$Lvl3 <- meta$Lvl3[match(gift_meta$Trait2, meta$Trait2)]

# download the 32 interesting traits
# db_gift <- GIFT_traits(
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
  here::here("data", "raw-data", "traits", "GIFT_sptraits.rds")
)

# 1. Match species names ----------------------
# clean species name
db_gift$taxa <- clean_species_list(db_gift$work_species, iter = TRUE)

# replace if known synonyms
is_syn <- db_gift$taxa %in% synonyms$synonym_taxa
m_syn <- match(db_gift$taxa[is_syn], synonyms$synonym_taxa)
db_gift$taxa[is_syn] <- synonyms$accepted_taxa[m_syn]

m1 <- match(taxolist$original_taxa, db_gift$taxa)
m2 <- match(taxolist$accepted_taxref, db_gift$taxa)
m3 <- match(taxolist$accepted_gbif, db_gift$taxa)
m_gift <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
print(prop.table(table(!is.na(m_gift)))) #86%

# 2. Format trait data ---------------------------
col_traits <- grep("^trait_value", names(db_gift))
lv3_traits <- gsub("^trait_value_", "", names(db_gift)[col_traits])
lab_traits <- gift_meta$Trait2[match(lv3_traits, gift_meta$Lvl3)]
unit_traits <- gift_meta$Units[match(lv3_traits, gift_meta$Lvl3)]
unit_traits <- gsub("²", "2", unit_traits)
unit_traits <- gsub("/g", ".g-1", unit_traits)
type_traits <- gift_meta$type[match(lv3_traits, gift_meta$Lvl3)]
lab_traits <- ifelse(
  type_traits == "numeric",
  paste(lab_traits, unit_traits, sep = "_"),
  lab_traits
)
lab_traits <- gsub("_", "\\.", lab_traits)
newlab <- paste(c("original_taxa", lab_traits), "GIFT", sep = "_")

out <- cbind(
  taxolist$accepted_taxa,
  db_gift$work_species[m_gift],
  db_gift[m_gift, col_traits]
)
names(out) <- c("accepted_taxa", newlab)


# 3. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitG_GIFT.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(is.na(out), 2, sum))
