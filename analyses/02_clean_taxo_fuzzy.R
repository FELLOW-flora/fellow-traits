# Get taxonomic information from World Flora Online (WFO)
# input:
#    species list in species_list_raw.rds
#    taxref in raw-data/TAXREF_v18_2025/TAXREFv18.txt
# output: full taxonomic information in the file species_list_taxo.csv

library("WorldFlora")

# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load dataset
sp <- read.csv(here::here("data", "derived-data", "species_list_raw.csv"))

# 1. get unique species list --------------
# harmonize the species names
sp$clean_taxo <- clean_species_list(sp$taxa)

# keep only unique species (no matter in which database they are listed)
spclean <- sort(unique(sp$clean_taxo))

# remove non-relevant species
rmlist <- c(
  "",
  "Na",
  "Arbre",
  "Espece",
  "Inconnue",
  "Inconue",
  "Pas d'adventice",
  "Repousse",
  "Repousse céréale",
  "Repousses cultivées",
  "Repousses graminées cultivées",
  "Repousses de graminées cultivées",
  "Repousses graminées",
  "Unknown",
  "Unknown dicot",
  "Unknown monocot",
  "Moribund dicot",
  "Moribund grass",
  "Cereal",
  "Espã¨ce non identifiã©e"
)

spclean <- spclean[!spclean %in% rmlist]

# hand cleaning
# remove taxa that is defined coarser than family
# Dicotyledones, ref 187223, is not in Taxref but in
# https://taxref.mnhn.fr/taxref-web/taxa/187223
# Do we keep families: e.g. Brassicaeae?

rmClass <- c(
  "Dicotyledonae",
  "Bryophyta",
  "Angiospermae",
  "Filicophytes",
  "Dicotyledones vraies",
  "Tracheophytes",
  "Tracheophyta"
)
spclean <- spclean[!spclean %in% rmClass]

rmAnimal <- c(
  "Ephippiger perforatus"
)
spclean <- spclean[!spclean %in% rmAnimal]

rmGenusGenus <- c(
  "Anisantha bromus", 
  "Festuca schedonorus", 
  "Ornithogalum muscari",
  "Picris helminthotheca" 
)
spclean <- spclean[!spclean %in% rmGenusGenus]

# harmonize the species names
spclean <- sort(unique(spclean))
print(paste("Number of unique taxa:", length(spclean)))
# 2822

# 2. get accepted name from WFO --------------
# quick comparisons of WFO packages
# wfor package works with API : # https://github.com/worldflora/wfor
# WorldFlora package works with a static version

# file downloaded from:
# https://zenodo.org/records/18007552/files/_DwC_backbone_R.zip?download=1

# Load the WFO data
WFO_file <- here::here(
  "data",
  "raw-data",
  "WFO_Backbone_v.2025.12",
  "classification.csv"
)

WFO_data <- data.table::fread(WFO_file, encoding = "UTF-8")
# WorldFlora::WFO.remember(WFO_file)

# sub for testing only
# sub <- sample(1:length(spclean), size = 50)
df_all <- WFO.match(spclean, WFO.data = WFO_data, verbose = FALSE)
write.csv(
  df_all,
  here::here("data", "derived-data", "wfo_all.csv"),
  row.names = FALSE
)

df_one <- WFO.one(df_all, verbose = FALSE)

df <- data.frame(
  original_taxa = df_one$spec.name.ORIG,
  accepted_wfo = df_one$scientificName,
  wfo_taxonID = df_one$taxonID,
  wfo_rank = df_one$taxonRank,
  wfo_authorship = df_one$scientificNameAuthorship,
  wfo_family = df_one$family,
  wfo_genus = df_one$genus,
  wfo_status = df_one$taxonomicStatus,
  wfo_match = ifelse(
    df_one$Fuzzy,
    "FUZZY",
    ifelse(df_one$Matched, "MATCH", "NOT FOUND")
  )
)

write.csv(
  df,
  here::here("data", "derived-data", "wfo_one.csv"),
  row.names = FALSE
)

# focus on taxa not found
no_wfo <- spclean[is.na(df$accepted_wfo)]
print(paste("Taxa with no match in WFO:", length(no_wfo)))
# 149 taxa

fuzzycheck <- df[df$wfo_match != "MATCH", ]
keep <- get_binomial(fuzzycheck$original_taxa) !=
  get_binomial(fuzzycheck$accepted_wfo)

write.csv(
  fuzzycheck[keep, ],
  file = here::here("data", "derived-data", "fuzzy_wfo.csv"),
  row.names = FALSE
)

# verify and complete the file fuzzy_taxref.csv
# and rename it as verified_fuzzy_taxref.csv
# add previous information
# fuzzy_verified <- read.csv(
#   here::here("data", "derived-data", "verified_fuzzy_taxref.csv")
# )

# 3. merge all and export ----------------------------------
# show missing taxa in WFO
cat(
  "Missing taxa: ",
  paste(df$original_taxa[is.na(df$accepted_wfo)], collapse = ", "),
  "\n"
)

# remove missing taxa
all_df <- df[!is.na(df$accepted_wfo), ]
all_df <- all_df[order(all_df$original_taxa), ]

all_df$accepted_taxa <- clean_species_list(all_df$accepted_wfo)
all_df$full_name <- paste(all_df$accepted_wfo, all_df$wfo_authorship)
all_df$accepted_rank <- all_df$wfo_rank

# add database information
which_db <- table(sp$clean_taxo, sp$database_ID)
db_df <- as.data.frame(apply(which_db > 0, 2, as.numeric))
row.names(db_df) <- row.names(which_db)
db_df <- db_df[match(all_df$original_taxa, row.names(db_df)), ]
names(db_df) <- paste0("in_", names(db_df))

write.csv(
  cbind(all_df, db_df),
  here::here("data", "derived-data", "species_list_taxo.csv"),
  row.names = FALSE
)

# make a short species list for gathering traits
short_df <- all_df[, c("accepted_taxa", "full_name", "wfo_rank")]
short_df <- short_df[!duplicated(short_df), ]
short_df <- short_df[complete.cases(short_df), ]
short_df <- short_df[order(short_df$accepted_taxa), ]
dim(short_df) # remain 2416 taxa

write.csv(
  short_df,
  here::here("data", "derived-data", "species_short_list.csv"),
  row.names = FALSE
)

# 4. create a list of known synonyms ---------------------

# synlist <- WFO.synonyms(df$wfo_taxonID[1], WFO.data = WFO_data, verbose = FALSE)
# by hand is much faster
browse <- WFO_data[WFO_data$acceptedNameUsageID %in% all_df$wfo_taxonID, ]

syn = data.frame(
  "synonym_taxa" = browse$scientificName,
  "synonym_full_name" = paste(
    browse$scientificName,
    browse$scientificNameAuthorship
  ),
  "accepted_taxonID" = browse$acceptedNameUsageID
)

syn$accepted_taxa <- all_df$accepted_taxa[match(
  syn$accepted_taxonID,
  all_df$wfo_taxonID
)]

syn$synonym_taxa <- clean_species_list(syn$synonym_taxa)

# keep only complete rows
syn_df <- syn[complete.cases(syn[, c("synonym_taxa", "accepted_taxa")]), ]

# remove synonyms that are accepted taxa (avoid loops)
syn_df <- syn_df[!syn_df$synonym_taxa %in% syn_df$accepted_taxa, ]

# remove synonyms with different accepted names
multi_acc <- table(syn_df$synonym_taxa, syn_df$accepted_taxa) > 0
n_accepted <- apply(multi_acc, 1, sum)
rm_syn <- names(n_accepted)[n_accepted > 1]
syn_df <- syn_df[!syn_df$synonym_taxa %in% rm_syn, ]

# remove duplicated lines
syn_df <- syn_df[!duplicated(syn_df[, c("synonym_taxa", "accepted_taxa")]), ]

dim(syn_df) #26531 synonyms
write.csv(
  syn_df,
  file = "data/derived-data/species_known_synonyms.csv",
  row.names = FALSE
)
