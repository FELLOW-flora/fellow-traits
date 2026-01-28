# Get and clean single database species list
# keeping three columns:
# taxa : the species name
# original_ID : the original ID of the species (if any) e.g. EPPO code
# database_ID : the ID of the database

# metadata from Dataset description.xlsx
# https://nextcloud.inrae.fr/apps/onlyoffice/s/fzCpq86Zwpo3q8Q?fileId=174729236

devtools::load_all()

# 1. Load dataset ---------------------
spfolder <- here::here("data", "raw-data", "species-list")
# list.files(spfolder, "xlsx")

# tropical_weeds
tropical <- readxl::read_xlsx(
  here::here(spfolder, "species_list_tropical_weeds.xlsx")
)

# reformat
df1 <- data.frame(
  "taxa" = tropical$taxon,
  "original_ID" = tropical$EPPO_code,
  "database_ID" = "tropical_weeds"
)

# clean taxa
df1$clean_taxo <- clean_species_list(df1$taxa)

# harmonize the species names
spclean <- sort(unique(df1$clean_taxo))
print(paste("Number of unique taxa:", length(spclean)))


# 2. compare taxa name with reference from GBIF --------------
# issue of 'Timeout was reached [api.gbif]'
# need to split in 2 for rgbif::name_backbone_checklist()

split2 <- floor(length(spclean) / 2)
checkgbif1 <- rgbif::name_backbone_checklist(spclean[1:split2])
checkgbif2 <- rgbif::name_backbone_checklist(
  spclean[(split2 + 1):length(spclean)]
)
checkgbif <- rbind(checkgbif1, checkgbif2)
gbif_df <- data.frame(
  original_taxa = spclean,
  accepted_gbif = checkgbif$canonicalName,
  gbif_key = checkgbif$usageKey,
  gbif_rank = checkgbif$rank,
  gbif_full_name = checkgbif$scientificName,
  gbif_phylum = checkgbif$phylum,
  gbif_order = checkgbif$order,
  gbif_family = checkgbif$family,
  gbif_status = paste(checkgbif$matchType, checkgbif$status, sep = "_")
)

checkgbif$synonym <- checkgbif$status %in% "SYNONYM"
synkey <- checkgbif$acceptedUsageKey[checkgbif$synonym]

syn_df <- c()
for (i in synkey) {
  di <- rgbif::name_usage(key = i)
  si_df <- data.frame(
    accepted_gbif = di$data$canonicalName,
    gbif_key = di$data$key,
    gbif_rank = di$data$rank,
    gbif_full_name = di$data$scientificName,
    gbif_phylum = di$data$phylum,
    gbif_order = di$data$order,
    gbif_family = di$data$family
  )
  syn_df <- rbind(syn_df, si_df)
}
# add status
syn_df$gbif_status <- paste0(checkgbif$matchType[checkgbif$synonym], "_SYNONYM")

# replace accepted synonyms
gbif_df[checkgbif$synonym, -1] <- syn_df

gbif_df <- gbif_df[order(gbif_df$original_taxa), ]

# remove errors associated to higher rank match
errors <- gbif_df$gbif_rank %in% c("FAMILY", "KINGDOM", "PHYLUM")
gbif_df[errors, -1] <- NA
gbif_df$accepted_rank[errors] <- "NONE_NA"

# Check missing taxa
cat(
  "Missing taxa: ",
  paste(gbif_df$original_taxa[is.na(gbif_df$accepted_gbif)], collapse = ", "),
  "\n"
) # mostly genus

gbif_df$accepted_taxa <- ifelse(
  is.na(gbif_df$accepted_gbif),
  gbif_df$original_taxa,
  gbif_df$accepted_gbif
)
gbif_df$accepted_rank <- ifelse(
  is.na(gbif_df$gbif_rank),
  "GENUS",
  gbif_df$gbif_rank
)

write.csv(
  gbif_df,
  here::here("data", "tropical-data", "species_list_taxo.csv"),
  row.names = FALSE
)

short_df <- gbif_df[, c("accepted_taxa", "accepted_rank")]
short_df <- short_df[!duplicated(short_df), ]
short_df <- short_df[complete.cases(short_df), ]
short_df <- short_df[order(short_df$accepted_taxa), ]
dim(short_df) # remain 1636 taxa
write.csv(
  short_df,
  here::here("data", "tropical-data", "species_short_list.csv"),
  row.names = FALSE
)

# 3. create a list of known synonyms ---------------------
# first original dataset
syn1_df <- data.frame(
  "synonym_taxa" = c(tropical$synonym, tropical$other_synonym),
  "accepted_taxa" = rep(df1$clean_taxo, 2)
)
syn1_df <- syn1_df[complete.cases(syn1_df), ]
syn1_df$accepted_taxa <- gbif_df$accepted_taxa[match(
  syn1_df$accepted_taxa,
  gbif_df$original_taxa
)]

# from gbif synonyms
diff_ori <- gbif_df$original_taxa != gbif_df$accepted_taxa
syn2_df = data.frame(
  "synonym_taxa" = gbif_df$original_taxa[diff_ori],
  "accepted_taxa" = gbif_df$accepted_taxa[diff_ori]
)

# merge, clean and export
syn_df <- rbind(syn1_df, syn2_df)
syn_df$synonym_taxa <- clean_species_list(syn_df$synonym_taxa)
# keep only complete rows
syn_df <- syn_df[complete.cases(syn_df), ]

# keep only rows with synonyms and taxa are different
syn_df <- syn_df[syn_df$synonym_taxa != syn_df$accepted_taxa, ]
# remove synonyms that are accepted taxa (avoid loops)
syn_df <- syn_df[!syn_df$synonym_taxa %in% syn_df$accepted_taxa, ]
# remove synonyms with different accepted names
multi_acc <- table(syn_df$synonym_taxa, syn_df$accepted_taxa) > 0
n_accepted <- apply(multi_acc, 1, sum)
rm_syn <- names(n_accepted)[n_accepted > 1]
syn_df <- syn_df[!syn_df$synonym_taxa %in% rm_syn, ]

# remove duplicated lines
syn_df <- syn_df[!duplicated(syn_df), ]

dim(syn_df) # 375 synonyms
write.csv(
  syn_df,
  file = "data/tropical-data/species_known_synonyms.csv",
  row.names = FALSE
)
