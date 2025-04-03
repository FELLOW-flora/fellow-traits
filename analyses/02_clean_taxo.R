# Get taxonomic information from TaxRef v18 and GBIF
# input: species list in species_list_raw.rds
# output: full taxonomic information in the file species_list_taxo.csv

# if the script is not run from make.R, need to load home made functions (clean_taxo())
devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load dataset
sp <- readRDS(here::here("data", "derived-data", "species_list_raw.rds"))

# 1. get unique species list --------------
# keep only unique species (no matter in which database they are listed)
splist <- sort(unique(sp$taxa))
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
taxref$clean_name <- clean_taxo(taxref$LB_NOM)

mtr <- match(tolower(spclean), tolower(taxref$clean_name))

df <- data.frame(
    original_taxa = spclean,
    accepted_taxref = taxref$LB_NOM[mtr],
    taxref_key = taxref$CD_REF[mtr],
    taxref_rank = taxref$RANG[mtr],
    taxref_full_name = taxref$NOM_COMPLET[mtr],
    taxref_phylum = taxref$CLASSE[mtr],
    taxref_order = taxref$ORDRE[mtr],
    taxref_family = taxref$FAMILLE[mtr],
    taxref_status = "EXACT",
    G1_INPN= taxref$GROUP1_INPN[mtr],
    G2_INPN= taxref$GROUP2_INPN[mtr],
    G3_INPN= taxref$GROUP3_INPN[mtr],
    habitat = taxref$HABITAT[mtr]
)

# remove non-matching element
df <- df[!is.na(df$accepted_taxref),]

# focus on taxa no found
no_taxref <- spclean[is.na(mtr)]
print(paste("Taxa with no exact match in Taxref:", length(no_taxref))) #52 only!

# try fuzzy match with stringdist package and Jaro-Winkler distance
fuzzy_mtr <- stringdist::amatch(
    tolower(no_taxref), tolower(taxref$clean_name),
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
    taxref_rank = taxref$RANG[fuzzy_mtr],
    taxref_full_name = taxref$NOM_COMPLET[fuzzy_mtr],
    taxref_phylum = taxref$CLASSE[fuzzy_mtr],
    taxref_order = taxref$ORDRE[fuzzy_mtr],
    taxref_family = taxref$FAMILLE[fuzzy_mtr],
    taxref_status = "FUZZY",
    G1_INPN= taxref$GROUP1_INPN[fuzzy_mtr],
    G2_INPN= taxref$GROUP2_INPN[fuzzy_mtr],
    G3_INPN= taxref$GROUP3_INPN[fuzzy_mtr],
    habitat = taxref$HABITAT[fuzzy_mtr]
)
fuzzy_df <- fuzzy_df[!is.na(fuzzy_df$accepted_taxref),]

full_df <- rbind(df, fuzzy_df)


# 3. add accepted name from GBIF --------------
checkgbif <- rgbif::name_backbone_checklist(full_df$taxref_full_name)
table(checkgbif$status, checkgbif$matchType, useNA="ifany") # all found :)

# simplify and select gbif information
gbif_df <- data.frame(
    accepted_gbif = checkgbif$canonicalName,
    gbif_key = checkgbif$usageKey,
    gbif_rank = checkgbif$rank,
    gbif_full_name = checkgbif$scientificName,
    gbif_phylum = checkgbif$phylum,
    gbif_order = checkgbif$order,
    gbif_family = checkgbif$family,
    gbif_status = paste(checkgbif$matchType, checkgbif$status, sep="_")
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
        gbif_family = di$data$family,
        gbif_status = "SYNONYM"
    )
    syn_df <- rbind(syn_df, si_df)
}

# replace accepted synonyms
gbif_df[checkgbif$synonym,] <- syn_df

# merge all
full_df <- cbind(full_df, gbif_df)


# 4. deal with missing taxa from Taxref --------------

# focus on taxa not found in TaxRef
miss <- spclean[!spclean %in%full_df$original_taxa]
print(paste("Taxa not found in Taxref:", length(miss))) #11 only!
# strict = TRUE else weird match
table(addgbif$status, addgbif$matchType, useNA="ifany") # 2 not found

gbif_add <- data.frame(
    accepted_gbif = addgbif$canonicalName,
    gbif_key = addgbif$usageKey,
    gbif_rank = addgbif$rank,
    gbif_full_name = addgbif$scientificName,
    gbif_phylum = addgbif$phylum,
    gbif_order = addgbif$order,
    gbif_family = addgbif$family,
    gbif_status = paste(addgbif$matchType, addgbif$status, sep="_")
)

# replace value for the synonym
synkey <- addgbif$acceptedUsageKey[addgbif$synonym]
syngbif <- rgbif::name_usage(key=synkey)
gbif_add[addgbif$synonym,] <- data.frame(
    accepted_gbif = syngbif$data$canonicalName,
    gbif_key = syngbif$data$key,
    gbif_rank = syngbif$data$rank,
    gbif_full_name = syngbif$data$scientificName,
    gbif_phylum = syngbif$data$phylum,
    gbif_order = syngbif$data$order,
    gbif_family = syngbif$data$family,
    gbif_status = "SYNONYM"
)

# replace value for the higher rank
# check issue with higher rank (only one here)
hrank <- rgbif::name_backbone(name = addgbif$verbatim_name[addgbif$matchType=="HIGHERRANK"], verbose=TRUE)
# Vicia ciliatula is synonym of Vicia ciliatula or Vicia lutea; lutea seems better
validkey <- hrank$acceptedUsageKey[hrank$scientificName == "Vicia ciliata Schur"]
synhrbif <- rgbif::name_usage(key=validkey)
gbif_add[addgbif$matchType=="HIGHERRANK",] <- data.frame(
    accepted_gbif = synhrbif$data$canonicalName,
    gbif_key = synhrbif$data$key,
    gbif_rank = synhrbif$data$rank,
    gbif_full_name = synhrbif$data$scientificName,
    gbif_phylum = synhrbif$data$phylum,
    gbif_order = synhrbif$data$order,
    gbif_family = synhrbif$data$family,
    gbif_status = "SYNONYM"
)

# check back in TaxRef
madd <- match(tolower(gbif_add$accepted_gbif), tolower(taxref$clean_name))
taxref_add <- data.frame(
    original_taxa = miss,
    accepted_taxref = taxref$LB_NOM[madd],
    taxref_key = taxref$CD_REF[madd],
    taxref_rank = taxref$RANG[madd],
    taxref_full_name = taxref$NOM_COMPLET[madd],
    taxref_phylum = taxref$CLASSE[madd],
    taxref_order = taxref$ORDRE[madd],
    taxref_family = taxref$FAMILLE[madd],
    taxref_status = ifelse(is.na(madd),"NOT FOUND", "EXACT_GBIF"),
    G1_INPN= taxref$GROUP1_INPN[madd],
    G2_INPN= taxref$GROUP2_INPN[madd],
    G3_INPN= taxref$GROUP3_INPN[madd],
    habitat = taxref$HABITAT[madd]
)

# merge all and export
add_df <- cbind(taxref_add, gbif_add)
all_df <- rbind(full_df, add_df)
all_df <- all_df[order(all_df$original_taxa),]

write.csv(all_df, here::here("data", "derived-data","species_list_taxo.csv"), row.names=FALSE)

# make a sublist of taxa to be checked
tbc <- all_df[!(all_df$taxref_status%in%"EXACT"&all_df$gbif_status%in%"EXACT_ACCEPTED"),] #262 taxa
write.csv(tbc, here::here("data", "derived-data","species_tobechecked.csv"), row.names=FALSE)
# sp[match(tbc$original_taxa[tbc$taxref_status=="NOT FOUND"], sp$taxa),-2]

