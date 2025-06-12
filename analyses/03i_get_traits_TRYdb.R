# Get trait information from https://www.try-db.org/de/Datasets.php
# input:
#    species list in species_short_list.csv
#    metadata in traits/Metatraits.xlsx
# output: traitX_TRY.csv

# package rtry not really useful:
# library(rtry)
# packageVersion("rtry")

# 0. Load packages, data, set parameters ----------------------
# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all()

# Load species list
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

# 1. traits from Ecoflora -------------------------------------
ecoflora <- read.table(
  here::here("data", "raw-data", "traits", "TRY", "Ecoflora_41939.txt"),
  sep = "\t",
  header = TRUE,
  quote = ""
)

t1 <- extract_trait_taxalist(
  trait_df = ecoflora,
  trait_sp = "AccSpeciesName",
  meta_trait = meta[meta$database %in% "Ecoflora", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms,
  long = TRUE,
  trait_label = "TraitName",
  trait_value = "StdValue"
) # 50.6%
names(t1)[-1] <- paste(names(t1)[-1], "Ecoflora", sep = "_")

# 2. traits from Biolflor -------------------------------------
biolflor <- read.table(
  here::here("data", "raw-data", "traits", "TRY", "Biolflor_41641.txt"),
  sep = "\t",
  header = TRUE,
  quote = ""
)

t2 <- extract_trait_taxalist(
  trait_df = biolflor,
  trait_sp = "AccSpeciesName",
  meta_trait = meta[meta$database %in% "Biolflor", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms,
  long = TRUE,
  trait_label = "TraitName",
  trait_value = "StdValue"
) # 67.7%
names(t2)[-1] <- paste(names(t2)[-1], "Biolflor", sep = "_")


out <- cbind(
  "accepted_taxa" = taxolist$accepted_taxa,
  t1[, -1],
  t2[, -1]
)

write.csv(
  out,
  file = here::here("data", "derived-data", "traitI_TRYdb.csv"),
  row.names = FALSE
)

# missing traits
print(apply(is.na(out), 2, sum))

#
#
#
# Get metadata
# ecoflora$TraitUnit <- ifelse(
#   ecoflora$UnitName == "",
#   ecoflora$TraitName,
#   paste(ecoflora$TraitName, ecoflora$UnitName, sep = "_")
# )
# ef_meta <- table(ecoflora$TraitUnit) |>
#   as.data.frame()
# ef_meta$db <- "Ecoflora"

# biolflor$TraitUnit <- ifelse(
#   biolflor$UnitName == "",
#   biolflor$TraitName,
#   paste(biolflor$TraitName, biolflor$UnitName, sep = "_")
# )
# bf_meta <- table(biolflor$TraitUnit) |> as.data.frame()
# bf_meta$db <- "Biolflor"
# trait_meta <- rbind(ef_meta, bf_meta)
# names(trait_meta) <- c("Trait", "N", "db")
# trait_meta <- trait_meta[trait_meta$Trait != "", ]
# trait_meta <- trait_meta[order(trait_meta$db, trait_meta$Trait), ]
# write.csv(
#   trait_meta,
#   here::here(
#     "data",
#     "raw-data",
#     "traits",
#     "TRY",
#     "TRY_downloaded_metadata.csv"
#   ),
#   row.names = FALSE
# )
