# Get taxonomic information from World Flora Online (WFO)
# input:
#    species list in species_list_raw.rds
#    taxref in raw-data/TAXREF_v18_2025/TAXREFv18.txt
# output: full taxonomic information in the file species_list_taxo.csv

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

# remove animals
rmNoPlant <- c(
  "Ephippiger perforatus",
  "Arthonia arthonioides"
)
spclean <- spclean[!spclean %in% rmNoPlant]

# remove genus genus taxa
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
# 2815

# 2. get accepted name from WFO --------------
# Load the WFO data
WFO_file <- here::here(
  "data",
  "raw-data",
  "WFO_Backbone_v.2025.12",
  "classification.csv"
)

WFO_data <- data.table::fread(WFO_file, encoding = "UTF-8")

# make hand matching instead of
# WorldFlora::WFO.match() then WorldFlora::WFO.one()
# which is long and require hand verification of fuzzy synonyms
# we use previously matched synonyms (02_clean_taxo_fuzzy.R)
# that have been verified by hand

# order data to make sure accepted match are found first
WFO_data$taxonomicStatus <- factor(
  WFO_data$taxonomicStatus,
  levels = c("Accepted", "Unchecked", "Synonym"),
  ordered = TRUE
)
WFO_data <- WFO_data[order(WFO_data$taxonomicStatus), ]


# 2A Exact match --------------
WFO_data$taxa <- clean_species_list(WFO_data$scientificName)
WFO_data$acceptedID <- ifelse(
  WFO_data$acceptedNameUsageID == "",
  WFO_data$taxonID,
  WFO_data$acceptedNameUsageID
)

WFO_acc <- WFO_data[WFO_data$taxonomicStatus %in% c("Accepted", "Unchecked"), ] # 635240
# make sure accepted is before unchecked

# there are some duplicates, but we take the first match
m_accepted <- match(spclean, WFO_acc$taxa)
df <- data.frame(
  original_taxa = spclean,
  accepted_wfo = WFO_acc$taxa[m_accepted],
  wfo_taxonID = WFO_acc$taxonID[m_accepted],
  wfo_rank = WFO_acc$taxonRank[m_accepted],
  wfo_authorship = WFO_acc$scientificNameAuthorship[m_accepted],
  wfo_family = WFO_acc$family[m_accepted],
  wfo_genus = WFO_acc$genus[m_accepted],
  wfo_status = WFO_acc$taxonomicStatus[m_accepted],
  wfo_match = ifelse(is.na(m_accepted), "NOT FOUND", "Exact")
)
df <- df[!is.na(df$accepted_wfo), ]

# table(is.na(m_accepted)) # 519 not accepted
print(paste("Number of exact match:", nrow(df)))
# 2296

# 2B match on verified synonyms --------------
# then use fuzzy match verified by G. Fried
fuzzy_verified <- read.csv(
  here::here("data", "derived-data", "fuzzy_wfo_v2.csv")
)

# select missing species
miss <- spclean[!spclean %in% df$original_taxa]

# simplify fuzzy verified
# no unnecessary fuzzy taxa
# fuzzy_verified$original_taxa[!fuzzy_verified$original_taxa %in% miss]
fuzzy_verified <- fuzzy_verified[fuzzy_verified$original_taxa %in% miss, ]
# still missing:
# fuzzy_verified$original_taxa[fuzzy_verified$accepted_wfo == ""]
fuzzy_verified <- fuzzy_verified[fuzzy_verified$accepted_wfo != "", ]
miss_with_syn <- miss[miss %in% fuzzy_verified$original_taxa]

fuzzy_verified$accepted_wfo <- clean_species_list(fuzzy_verified$accepted_wfo)

m_fuzzy <- match(miss_with_syn, fuzzy_verified$original_taxa)
syn <- fuzzy_verified$accepted_wfo[m_fuzzy]
sta <- fuzzy_verified$Match[m_fuzzy]
m_fuzzy_wfo <- match(syn, WFO_data$taxa)

df_fuzzy <- data.frame(
  original_taxa = miss_with_syn,
  accepted_wfo = WFO_data$taxa[m_fuzzy_wfo],
  wfo_taxonID = WFO_data$taxonID[m_fuzzy_wfo],
  wfo_rank = WFO_data$taxonRank[m_fuzzy_wfo],
  wfo_authorship = WFO_data$scientificNameAuthorship[m_fuzzy_wfo],
  wfo_family = WFO_data$family[m_fuzzy_wfo],
  wfo_genus = WFO_data$genus[m_fuzzy_wfo],
  wfo_status = WFO_data$taxonomicStatus[m_fuzzy_wfo],
  wfo_match = ifelse(is.na(m_fuzzy_wfo), "NOT FOUND", sta)
)
df_fuzzy <- df_fuzzy[!is.na(df_fuzzy$accepted_wfo), ]

# table(is.na(m_accepted)) # 519 not accepted
print(paste("Number of fuzzy match:", nrow(df_fuzzy)))
# 142

# 2C match on all WFO if not confusion -------
miss <- miss[!miss %in% df_fuzzy$original_taxa]

# match all synonyms
m_wfo <- matches_taxa(
  miss,
  WFO_data$taxa,
  binomial = TRUE,
  genus = FALSE
)

# unique accepted synonyms
um_wfo <- sapply(m_wfo, getid, ref = WFO_data$acceptedID)
m_syn <- match(um_wfo, WFO_data$taxonID)
df_syn <- data.frame(
  original_taxa = miss,
  accepted_wfo = WFO_data$scientificName[m_syn],
  wfo_taxonID = WFO_data$taxonID[m_syn],
  wfo_rank = WFO_data$taxonRank[m_syn],
  wfo_authorship = WFO_data$scientificNameAuthorship[m_syn],
  wfo_family = WFO_data$family[m_syn],
  wfo_genus = WFO_data$genus[m_syn],
  wfo_status = WFO_data$taxonomicStatus[m_syn],
  wfo_match = ifelse(is.na(m_syn), "NOT FOUND", "Synonym")
)
df_syn <- df_syn[!is.na(df_syn$accepted_wfo), ]

# table(is.na(m_accepted)) # 521 not accepted
print(paste("Number of unequivocal synonyms:", nrow(df_syn)))
# 370

# 3. merge all and export ----------------------------------
all_df <- rbind(df, df_fuzzy, df_syn)

all_df <- all_df[order(all_df$original_taxa, all_df$accepted_wfo), ]

# focus on taxa not found
no_wfo <- spclean[!(spclean %in% all_df$original_taxa)]
print(paste("Taxa with no match in WFO:", length(no_wfo)))
# 7 taxa

# show missing taxa in WFO
cat(
  "Missing taxa: ",
  paste(no_wfo, collapse = ", "),
  "\n"
)

# remove missing taxa
all_df$accepted_taxa <- clean_species_list(all_df$accepted_wfo)
all_df$full_name <- paste(all_df$accepted_wfo, all_df$wfo_authorship)
all_df$accepted_rank <- all_df$wfo_rank

# correct the Allium :  not needed
# table(all_df$wfo_match)
# not_in_wfo <- all_df$wfo_status == "Synonym"
# msyn <- match(all_df$accepted_wfo[not_in_wfo], WFO_data$taxa)
# macc <- match(WFO_data$acceptedID[msyn], WFO_data$taxonID)
# all_df$accepted_wfo[not_in_wfo] <- WFO_data$scientificName[macc]

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
dim(short_df) # remain 2471 taxa

write.csv(
  short_df,
  here::here("data", "derived-data", "species_short_list.csv"),
  row.names = FALSE
)

# for record
# sp_dup <- rep(sub[is.na(um_wfo)], sapply(m_wfo, length)[is.na(um_wfo)])
# ind_dup <- unlist(lapply(which(is.na(um_wfo)), function(x) unlist(m_wfo[[x]])))
# m_dup <- match(WFO_data$acceptedID[ind_dup], WFO_data$taxonID)
# df_errors <- data.frame(
#   original_taxa = sp_dup,
#   accepted_wfo = WFO_data$scientificName[m_dup],
#   wfo_taxonID = WFO_data$taxonID[m_dup],
#   wfo_rank = WFO_data$taxonRank[m_dup],
#   wfo_authorship = WFO_data$scientificNameAuthorship[m_dup],
#   wfo_family = WFO_data$family[m_dup],
#   wfo_genus = WFO_data$genus[m_dup],
#   wfo_status = WFO_data$taxonomicStatus[m_dup],
#   wfo_match = ifelse(is.na(m_dup), "NOT FOUND", "DUPLICATED SYNONYMS")
# )
# write.csv(df_errors, here::here("data", "derived-data", "dupsyn_wfo.csv"))

# fuzzycheck <- df[df$wfo_match != "MATCH", ]
# keep <- get_binomial(fuzzycheck$original_taxa) !=
#   get_binomial(fuzzycheck$accepted_wfo)

# write.csv(
#   fuzzycheck[keep, ],
#   file = here::here("data", "derived-data", "fuzzy_wfo.csv"),
#   row.names = FALSE
# )

# 4. create a list of known synonyms ---------------------

# synlist <- WFO.synonyms(df$wfo_taxonID[1], WFO.data = WFO_data, verbose = FALSE)
# by hand is much faster

browse <- WFO_data[WFO_data$acceptedNameUsageID %in% all_df$wfo_taxonID, ]

syn1_df <- data.frame(
  "synonym_taxa" = browse$scientificName,
  "accepted_taxonID" = browse$acceptedNameUsageID
)

syn1_df$accepted_taxa <- all_df$accepted_taxa[match(
  syn1_df$accepted_taxonID,
  all_df$wfo_taxonID
)]

syn1_df$synonym_taxa <- clean_species_list(syn1_df$synonym_taxa)


# keep only complete rows
syn1_df <- syn1_df[
  complete.cases(syn1_df[, c("synonym_taxa", "accepted_taxa")]),
]

# from original dataset
diff_ori <- all_df$original_taxa != all_df$accepted_taxa
syn2_df = data.frame(
  "synonym_taxa" = all_df$original_taxa[diff_ori],
  "accepted_taxonID" = all_df$wfo_taxonID[diff_ori],
  "accepted_taxa" = all_df$accepted_taxa[diff_ori]
)


# merge synonyms data
syn_df <- rbind(syn1_df, syn2_df)

# remove synonyms that are accepted taxa (avoid loops)
syn_df <- syn_df[!syn_df$synonym_taxa %in% syn_df$accepted_taxa, ]


# remove synonyms with different accepted names
multi_acc <- table(syn_df$synonym_taxa, syn_df$accepted_taxa) > 0
n_accepted <- apply(multi_acc, 1, sum)
rm_syn <- names(n_accepted)[n_accepted > 1]
syn_df <- syn_df[!syn_df$synonym_taxa %in% rm_syn, ]

# remove duplicated lines
syn_df <- syn_df[!duplicated(syn_df[, c("synonym_taxa", "accepted_taxa")]), ]

dim(syn_df) #48956 synonyms
write.csv(
  syn_df,
  file = "data/derived-data/species_known_synonyms.csv",
  row.names = FALSE
)
