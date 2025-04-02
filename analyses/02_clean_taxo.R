# if not run from make.R, need to load home made functions
# devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load dataset
df <- readRDS(here::here("data", "derived-data", "species_list_raw.rds"))

# 1. get unique species list --------------
# keep only unique species (no matter in which database they are listed)
splist <- sort(unique(df$taxa))
# harmonize the species names
spclean <- sort(unique(clean_taxo(splist)))


# remove non-relevant species
rmlist <- c("", "NA", "Arbre", "Espece", "Inconnue", "Inconue", "Pas d'adventice", 
            "Repousse", "Repousse Céréale", "Repousses cultivées", "Repousses Graminées cultivées",
            "Repousses de graminées cultivées", "Repousses graminées")

spclean <- spclean[!spclean %in% rmlist]

print(paste("Number of unique taxa:", length(spclean))) #1361


# 2. get accepted name from Taxref --------------
# could use rtaxref to use the online API but takes too long and unstable
# much faster and more reliable to use the full database locally
# https://inpn.mnhn.fr/telechargement/referentielEspece/taxref/18.0/menu
taxref <- read.table(here::here("data", "raw-data", "TAXREF_v18_2025", "TAXREFv18.txt"), sep="\t", header = TRUE)
mtr <- match(tolower(spclean), tolower(clean_taxo(taxref$LB_NOM)))

df <- data.frame(
    original_taxa = spclean,
    accepted_taxref = taxref$LB_NOM[mtr],
    taxref_key = taxref$CD_REF[mtr],
    G1_INPN= taxref$GROUP1_INPN[mtr],
    G2_INPN= taxref$GROUP2_INPN[mtr],
    G3_INPN= taxref$GROUP3_INPN[mtr],
    rank = taxref$RANG[mtr],
    full_name = taxref$NOM_COMPLET[mtr],
    habitat = taxref$HABITAT[mtr],
    class = taxref$CLASSE[mtr],
    order = taxref$ORDRE[mtr],
    family = taxref$FAMILLE[mtr]
)

# remove non-matching element
df <- df[!is.na(df$accepted_taxref),]

# focus on taxa no found
no_taxref <- spclean[is.na(mtr)]
print(paste("Taxa not found in Taxref:", length(no_taxref))) #52 only!

# try fuzzy match with stringdist package and Jaro-Winkler distance
fuzzy_mtr <- stringdist::amatch(
    tolower(no_taxref), tolower(clean_taxo(taxref$LB_NOM)),
    maxDist=0.2, method="jw", p=0.005)

# essential step by hand : check fuzzy match
# fuzzycheck <- cbind(original_taxa = no_taxref,
#                     accepted_taxref = taxref$LB_NOM[fuzzy_mtr], 
#                     sdist = stringdist::stringdist(tolower(no_taxref), tolower(clean_taxo(taxref$LB_NOM[fuzzy_mtr])),
#                     method="jw", p=0.005))
# write.csv(fuzzycheck, file=here::here("data", "derived-data", "fuzzy_taxref.csv"), row.names=FALSE)
# verify the file fuzzy_taxref.csv and rename it as checked_fuzzy_taxref.csv
fuzzy_verified <- read.csv(here::here("data", "derived-data", "checked_fuzzy_taxref.csv"))
# make sure to have only missing species name
fuzzy_verified <- fuzzy_verified[fuzzy_verified$original_taxa %in% no_taxref,]

fuzzy_mtr <- match(fuzzy_verified$accepted_taxref, taxref$LB_NOM)

fuzzy_df <- data.frame(
    original_taxa = fuzzy_verified$original_taxa,
    accepted_taxref = taxref$LB_NOM[fuzzy_mtr],
    taxref_key = taxref$CD_REF[fuzzy_mtr],
    G1_INPN= taxref$GROUP1_INPN[fuzzy_mtr],
    G2_INPN= taxref$GROUP2_INPN[fuzzy_mtr],
    G3_INPN= taxref$GROUP3_INPN[fuzzy_mtr],
    rank = taxref$RANG[fuzzy_mtr],
    full_name = taxref$NOM_COMPLET[fuzzy_mtr],
    habitat = taxref$HABITAT[fuzzy_mtr],
    class = taxref$CLASSE[fuzzy_mtr],
    order = taxref$ORDRE[fuzzy_mtr],
    family = taxref$FAMILLE[fuzzy_mtr]
)
fuzzy_df <- fuzzy_df[!is.na(fuzzy_df$accepted_taxref),]

full_df <- rbind(df, fuzzy_df)

# 3. add accepted name from GBIF --------------
checkgbif <- rgbif::name_backbone_checklist(full_df$full_name)
table(checkgbif$status, useNA="ifany") # all found :)

# simplify and select gbif information
gbif_df <- data.frame(
    accepted_gbif = checkgbif$canonicalName,
    gbif_key = checkgbif$usageKey,
    gbif_rank = checkgbif$rank,
    gbif_full_name = checkgbif$scientificName,
    gbif_phylum = checkgbif$phylum,
    gbif_order = checkgbif$order,
    gbif_family = checkgbif$family
)

# look for the accepted name of synonyms
synkey <- checkgbif$acceptedUsageKey[checkgbif$synonym]
syn_df <- c()
for (i in synkey){
    di <- rgbif::name_usage(key=i)
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

# replace accepted synonyms
gbif_df[checkgbif$synonym,] <- syn_df

# merge all and export
full_df <- cbind(full_df, gbif_df)
full_df <- full_df[order(full_df$original_taxa),]

table(spclean %in%full_df$original_taxa) # only 11 missing !!
# could use GBIF to fill the missing ones, but for now it's ok

write.csv(full_df, here::here("data", "derived-data","species_list_taxo.csv"), row.names=FALSE)


