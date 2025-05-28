# Get trait information from https://floraveg.eu/
# input:
#    species list in derived-data/species_short_list.csv
#    synonyms list in derived-data/species_known_synonyms.csv
#    trait databases in raw-data/traits/FloraVeg
#    metadata in raw-data/traits/Metatraits.xlsx
# output: traitA_floraveg.csv

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

# set the folder with traits data
traitfolder <- here::here("data", "raw-data", "traits", "FloraVeg")

# load metadata of traits (defining which traits are kept)
meta <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "Metatraits.xlsx")
)

# need to define:
# A. what should we do with taxa that are not species (e.g. genus, not present in trait database)?
# B. what should we do with missing species? fuzzy match? look for gbif synonyms?

# 1. Disturbance from Midolo ----------------------
midolo <- readxl::read_xlsx(
  here::here(traitfolder, "disturbance_indicator_values_Midolo_2023.xlsx")
)

t1 <- extract_trait_taxalist(
  trait_df = midolo,
  trait_sp = "species",
  meta_trait = meta[meta$database %in% "Midolo2023", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 86.34 %
names(t1)[-1] <- paste(names(t1)[-1], "Midolo2023", sep = "_")

# 2. Ellenberg from Tichy --------------------
tichy <- readxl::read_xlsx(
  here::here(traitfolder, "Ellenberg_Indicator_values_Tichy_2022.xlsx"),
  sheet = 10
)
names(tichy)[1:2] <- c("SeqID", "Taxon")
tichy <- tichy[-1, ]

# so far, let's focus on average Ellenberg values
# replace NA or x by NA
tichy[tichy == "x"] <- NA
tichy[tichy == "NA"] <- NA

t2 <- extract_trait_taxalist(
  trait_df = tichy,
  trait_sp = "Taxon",
  meta_trait = meta[meta$database %in% "Tichy2022", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 90.5 %
names(t2)[-1] <- paste(names(t2)[-1], "Tichy2022", sep = "_")

# 3. seed dispersal from Losova -----------------
lososova <- readxl::read_xlsx(here::here(
  traitfolder,
  "Lososova_et_al_2023_Dispersal_version2_2024-06-14.xlsx"
))

t3 <- extract_trait_taxalist(
  trait_df = lososova,
  trait_sp = "Taxon",
  meta_trait = meta[meta$database %in% "Lososova2023", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 95.13 %
names(t3)[-1] <- paste(names(t3)[-1], "Lososova2023", sep = "_")

# 4. life form from Dfevojan -----------------
dfevojan <- readxl::read_xlsx(
  here::here(traitfolder, "Life_form_Dfevojan_2023.xlsx")
)

t4 <- extract_trait_taxalist(
  trait_df = dfevojan,
  trait_sp = "FloraVeg.Taxon",
  meta_trait = meta[meta$database %in% "Dfevojan2023", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 95.66%
names(t4)[-1] <- paste(names(t4)[-1], "Dfevojan2023", sep = "_")

# 5. Export -------------------------------------
out <- cbind(
  "accepted_taxa" = taxolist$accepted_taxa,
  t1[, -1],
  t2[, -1],
  t3[, -1],
  t4[, -1]
)

write.csv(
  out,
  file = here::here("data", "derived-data", "traitA_floraveg.csv"),
  row.names = FALSE
)
# missing traits
print(apply(is.na(out), 2, sum))

# why not present?
# taxolist$accepted_taxa[is.na(t4$original_taxa_Dfevojan2023)]
# table(taxolist$accepted_rank[is.na(t4$original_taxa_Dfevojan2023)])

# genus imputation will be delt in 04_merge_traits.R
