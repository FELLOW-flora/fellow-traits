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

# hand cleaning
# remove taxa that is defined coarser than family
# Dicotyledones, ref 187223, is not in Taxref but in 
# https://taxref.mnhn.fr/taxref-web/taxa/187223
rmClass <- c("Dicotyledonae", "Bryophyta")
spclean <- spclean[!spclean %in% rmClass]

# harmonize the species names
spclean <- sort(unique(spclean))
print(paste("Number of unique taxa:", length(spclean))) #1359

# 2. get accepted name from Taxref --------------
# could use rtaxref to use the online API but takes too long and unstable
# much faster and more reliable to use the full database locally
# https://inpn.mnhn.fr/telechargement/referentielEspece/taxref/18.0/menu
taxref <- read.table(here::here("data", "raw-data", "TAXREF_v18_2025", "TAXREFv18.txt"), sep="\t", header = TRUE)
taxref$clean_name <- clean_taxo(taxref$LB_NOM)

mtr <- match(tolower(spclean), tolower(taxref$clean_name))
# for synonyms, make sure to get the accepted information
msyn <- match(taxref$NOM_VALIDE[mtr], taxref$NOM_COMPLET)

df <- data.frame(
    original_taxa = spclean,
    accepted_taxref = taxref$LB_NOM[msyn],
    taxref_key = taxref$CD_REF[msyn],
    taxref_rank = taxref$RANG[msyn],
    taxref_full_name = taxref$NOM_COMPLET[msyn],
    taxref_class = taxref$CLASSE[msyn],
    taxref_order = taxref$ORDRE[msyn],
    taxref_family = taxref$FAMILLE[msyn],
    taxref_status = ifelse(msyn==mtr, "EXACT_ACCEPTED", "EXACT_SYNONYM"),
    G1_INPN= taxref$GROUP1_INPN[msyn],
    G2_INPN= taxref$GROUP2_INPN[msyn],
    G3_INPN= taxref$GROUP3_INPN[msyn],
    habitat = taxref$HABITAT[msyn]
)

# remove non-matching element
df <- df[!is.na(df$accepted_taxref),]

# focus on taxa no found
no_taxref <- spclean[is.na(mtr)]
print(paste("Taxa with no exact match in Taxref:", length(no_taxref))) #51 only!

# try fuzzy match with stringdist package and Jaro-Winkler distance
# fuzzy_mtr <- stringdist::amatch(
#     tolower(no_taxref), tolower(taxref$clean_name),
#     maxDist=0.2, method="jw", p=0.005)

# essential step by hand : check fuzzy match
# fuzzycheck <- cbind(original_taxa = no_taxref,
#                     accepted_taxref = taxref$LB_NOM[fuzzy_mtr], 
#                     sdist = stringdist::stringdist(tolower(no_taxref), tolower(clean_taxo(taxref$LB_NOM[fuzzy_mtr])),
#                     method="jw", p=0.005))
# write.csv(fuzzycheck, file=here::here("data", "derived-data", "fuzzy_taxref.csv"), row.names=FALSE)
# verify and complete the file fuzzy_taxref.csv 
# and rename it as verified_fuzzy_taxref.csv

# few additions based on species_checked_GF.csv
# Aspaacut becomes Asparagus acutifolius
# Podospermum picroides becomes Urospermum picroides
# Festuca myuros is Vulpia alopecuros in Taxref
# https://taxref.mnhn.fr/taxref-web/taxa/621043

fuzzy_verified <- read.csv(here::here("data", "derived-data", "verified_fuzzy_taxref.csv"))
# make sure to have only missing species name
fuzzy_verified <- fuzzy_verified[fuzzy_verified$original_taxa %in% no_taxref,]

fuzzy_mtr <- match(fuzzy_verified$accepted_taxref, taxref$LB_NOM)
# for synonyms, make sure to get the accepted information
fuzzy_syn <- match(taxref$NOM_VALIDE[fuzzy_mtr], taxref$NOM_COMPLET)

fuzzy_df <- data.frame(
    original_taxa = fuzzy_verified$original_taxa,
    accepted_taxref = taxref$LB_NOM[fuzzy_syn],
    taxref_key = taxref$CD_REF[fuzzy_syn],
    taxref_rank = taxref$RANG[fuzzy_syn],
    taxref_full_name = taxref$NOM_COMPLET[fuzzy_syn],
    taxref_class = taxref$CLASSE[fuzzy_syn],
    taxref_order = taxref$ORDRE[fuzzy_syn],
    taxref_family = taxref$FAMILLE[fuzzy_syn],
    taxref_status = ifelse(fuzzy_syn==fuzzy_mtr, "FUZZY_ACCEPTED", "FUZZY_SYNONYM"),
    G1_INPN= taxref$GROUP1_INPN[fuzzy_syn],
    G2_INPN= taxref$GROUP2_INPN[fuzzy_syn],
    G3_INPN= taxref$GROUP3_INPN[fuzzy_syn],
    habitat = taxref$HABITAT[fuzzy_syn]
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
print(paste("Taxa not found in Taxref:", length(miss))) # 9 only!

addgbif <- rgbif::name_backbone_checklist(miss, strict = TRUE)
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
# check if synonyms are needed : no
# maddsyn <- match(taxref$NOM_VALIDE[madd], taxref$NOM_COMPLET)
# table(madd==maddsyn)
taxref_add <- data.frame(
    original_taxa = miss,
    accepted_taxref = taxref$LB_NOM[madd],
    taxref_key = taxref$CD_REF[madd],
    taxref_rank = taxref$RANG[madd],
    taxref_full_name = taxref$NOM_COMPLET[madd],
    taxref_class = taxref$CLASSE[madd],
    taxref_order = taxref$ORDRE[madd],
    taxref_family = taxref$FAMILLE[madd],
    taxref_status = ifelse(is.na(madd),"NOT FOUND", "EXACT_GBIF"),
    G1_INPN= taxref$GROUP1_INPN[madd],
    G2_INPN= taxref$GROUP2_INPN[madd],
    G3_INPN= taxref$GROUP3_INPN[madd],
    habitat = taxref$HABITAT[madd]
)

# 5. merge all and export ----------------------------------
add_df <- cbind(taxref_add, gbif_add)
all_df <- rbind(full_df, add_df)
all_df <- all_df[order(all_df$original_taxa),]

all_df$accepted_taxa <- ifelse(is.na(all_df$accepted_taxref), all_df$accepted_gbif, all_df$accepted_taxref)
all_df$full_name <- ifelse(is.na(all_df$taxref_full_name), all_df$gbif_full_name, all_df$taxref_full_name)

write.csv(all_df, here::here("data", "derived-data","species_list_taxo.csv"), row.names=FALSE)


# make a sublist of taxa to be checked (not needed anymore)
# tbc <- all_df[!(all_df$taxref_status%in%c("EXACT_ACCEPTED","EXACT_SYNONYM") & all_df$gbif_status%in%"EXACT_ACCEPTED"),] #262 taxa
# write.csv(tbc, here::here("data", "derived-data","species_tobechecked.csv"), row.names=FALSE)
# sp[match(tbc$original_taxa[tbc$taxref_status=="NOT FOUND"], sp$taxa),-2]


# 6. create a list of known synonyms ---------------------

# from gbif
synlist <- list()
for (i in 1:nrow(all_df)){
    ti <- rgbif::name_usage(key=all_df$gbif_key[i],data="synonyms")$data
    if(nrow(ti)>0){
        synlist[[i]] = data.frame(
            "synonym_taxa"=ti$canonicalName,
            "synonym_full_name"=ti$scientificName,
            "accepted_taxa"=all_df$accepted_taxa[i]
        )
    }
}
syn1_df <- do.call(rbind, synlist)

# from taxref
taxrefsyn <- taxref[taxref$CD_REF %in% all_df$taxref_key,]
taxrefsyn <- taxrefsyn[taxrefsyn$NOM_VALIDE != taxrefsyn$NOM_COMPLET,]
taxrefsyn$LB_NOM_VALIDE <- taxref$LB_NOM[match(taxrefsyn$NOM_VALIDE, taxref$NOM_COMPLET)]
syn2_df = data.frame(
    "synonym_taxa"=taxrefsyn$LB_NOM,
    "synonym_full_name"=taxrefsyn$NOM_COMPLET,
    "accepted_taxa"=taxrefsyn$LB_NOM_VALIDE 
)

# merge, clean and export
syn_df <- rbind(syn1_df, syn2_df)
syn_df$synonym_taxa <- clean_taxo(syn_df$synonym_taxa)
# remove synonyms that are accepted taxa (avoid loops)
syn_df <- syn_df[!syn_df$synonym_taxa %in% syn_df$accepted_taxa,]
# remove synonyms with different accepted names
n_accepted <- apply(table(syn_df$synonym_taxa, syn_df$accepted_taxa)>0, 1, sum)
rm_syn <- names(n_accepted)[n_accepted>1]
syn_df <- syn_df[!syn_df$synonym_taxa %in% rm_syn,]
syn_df <- syn_df[!duplicated(syn_df$synonym_taxa),]
# only complete rows
syn_df <- syn_df[complete.cases(syn_df),]
dim(syn_df) #35733 synonyms !!
write.csv(syn_df, file = "data/derived-data/species_known_synonyms.csv", row.names=FALSE)




# add species for the genus that are not defined at species level
# species <- all_df$accepted_taxa[all_df$taxref_rank%in% c("ES", "SSES", "VAR")]
# genus <- all_df$accepted_taxa[all_df$taxref_rank%in% "GN"]
# gen_sp <- sapply(strsplit(species, " "), function(x) x[[1]])
# table(genus %in% gen_sp)
# genus[!genus %in% gen_sp]
# taxref$LB_NOM[grep("Tilia", taxref$LB_NOM)]
# other relevant ressources
# Kew’s Plants of the World in taxise

# Leipzig System list of species
# https://github.com/idiv-biodiversity/lcvplants
# Also works on vectors of names
# search_ori <- lcvplants::lcvp_search(all_df$original_taxa)

# checklcvp <- lcvplants::lcvp_search(all_df$full_name, show_correct=TRUE)
# dim(checklcvp)
# dim(all_df)
# table(checklcvp$global.Id == checklcvp$globalId.of.Output.Taxon)
# lcvp_df <- data.frame(
#     accepted_lcvp = checklcvp$Output.Taxon,
#     lcvp_key = checklcvp$usageglobalId.of.Output.TaxonKey,
#     gbif_rank = checklcvp$Rank,
#     gbif_full_name = checklcvp$Output.Taxon,
#     gbif_phylum = checkgbif$phylum,
#     gbif_order = checkgbif$order,
#     gbif_family = checkgbif$family,
#     gbif_status = check
# )
# lcvp_match <- attributes(checklcvp)$match.names
# lcvp_match
# lcvplants::lcvp_summary(checklcvp)
# lcvplants::lcvp_summary
# lcvp_match
# data(tab_lcvp, package = "LCVP")
# str(tab_lcvp)

# tab_lcvp$clean_taxa <- clean_taxo(paste(tab_lcvp$Input.Genus, tab_lcvp$Input.Epitheton, sep=" "))
# mtr <- match(tolower(all_df$accepted_taxa), tolower(tab_lcvp$clean_taxa))
# table(is.na(mtr))
# # for synonyms, make sure to get the accepted information
# msyn <- match(taxref$NOM_VALIDE[mtr], taxref$NOM_COMPLET)

# add World Flora database 
# wfo <- read.table(here::here("data", "raw-data", "WFO_Backbone_v.2023.12", "classfication.csv"), 
#                   sep="\t", header = TRUE, fill = TRUE)
# str(wfo)