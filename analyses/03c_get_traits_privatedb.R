# Get trait information from private trait database
# input:
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    trait databases in data/raw-data/traits/private
#    metadata in traits/Metatraits.xlsx
# output: traitC_privatedb.csv

# if the script is not run from make.R, need to load home made functions (clean_species_list())
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

# set the folder with traits data
traitfolder <- here::here("data", "raw-data", "traits", "private")

# load metadata of traits (which define which traits are kept)
meta <- readxl::read_xlsx(here::here(
  "data",
  "raw-data",
  "traits",
  "Metatraits.xlsx"
))


# 1. Valeur Pollinique ----------------------
yvoz <- read.csv(
  here::here(
    traitfolder,
    "proxys stephane cordeau",
    "Valeurs polliniques estimees.csv"
  ),
  sep = ";"
)

t1 <- extract_trait_taxalist(
  trait_df = yvoz,
  trait_sp = "latin_name",
  meta_trait = meta[meta$database %in% "YvozValPol", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 58.21 %
names(t1)[-1] <- paste(names(t1)[-1], "YvozValPol", sep = "_")


# 2. traits from the SP_Vignes ------------
vignes <- readxl::read_xls(here::here(traitfolder, "TraitSpVignes.xls"))
# dim(vignes) # only 123 species
# too small dataset, is it worth the effort?

t2 <- extract_trait_taxalist(
  trait_df = vignes,
  trait_sp = "Species",
  meta_trait = meta[meta$database %in% "SPVignes", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 8.85 %
names(t2)[-1] <- paste(names(t2)[-1], "SPVignes", sep = "_")


# 3. traits from the AT_VINEDIVERS ------------
spfolder <- here::here("data", "raw-data", "species-list")
vinedivers <- readxl::read_xlsx(
  here::here(spfolder, "At_Fr_Ro_Sp_Species_List_VINEDIVERS.xlsx"),
  sheet = 2
)
names(vinedivers)[1:2] <- c("species", "abb")
# dim(vinedivers) # only 240 species

t3 <- extract_trait_taxalist(
  trait_df = vinedivers,
  trait_sp = "species",
  meta_trait = meta[meta$database %in% "ATVinedivers", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 19.46 %
names(t3)[-1] <- paste(names(t3)[-1], "ATVinedivers", sep = "_")


# 4. Export -------------------------------------
out <- cbind(
  "accepted_taxa" = taxolist$accepted_taxa,
  t1[, -1],
  t2[, -1],
  t3[, -1]
)

write.csv(
  out,
  file = here::here("data", "derived-data", "traitC_privatedb.csv"),
  row.names = FALSE
)

# missing traits
print(apply(is.na(out), 2, sum))
