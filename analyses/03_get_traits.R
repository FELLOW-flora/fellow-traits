# Load species list with taxonomy
taxolist <- read.csv(here::here("data", "derived-data","species_list_taxo.csv"), )
# set the folder with traits data
traitfolder <- here::here("data", "raw-data", "traits")

# need to define:
# A. which traits are needed
# B. what should we do with taxa that are not species (e.g. genus, not present in trait database)? 
# C. what should we do with missing species? fuzzy match? look for gbif synonyms?



# 1. Disturbance from Modolo ----------------------
modolo <- readxl::read_xlsx(here::here(traitfolder, "disturbance_indicator_values_Midolo_2023.xlsx"))
# clean species name
modolo$taxa <- clean_taxo(modolo$species)
m1 <- match(taxolist$original_taxa, modolo$taxa)
m2 <- match(taxolist$accepted_taxref, modolo$taxa)
m3 <- match(taxolist$accepted_gbif, modolo$taxa)
m_modolo <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
prop.table(table(!is.na(m_modolo))) #69%





# 2. Ellenberg from Tichy --------------------
tichy <- readxl::read_xlsx(here::here(traitfolder, "Ellenberg_Indicator_values_Tichy_2022.xlsx"), sheet = 2)
# clean species name (simplified and original)
tichy$taxa <- clean_taxo(tichy$Taxon)
tichy$taxa2 <- clean_taxo(tichy$Taxon.Original)
m1 <- match(taxolist$original_taxa, tichy$taxa)
m2 <- match(taxolist$accepted_taxref, tichy$taxa)
m3 <- match(taxolist$accepted_gbif, tichy$taxa)
m4 <- match(taxolist$original_taxa, tichy$taxa2)
m5 <- match(taxolist$accepted_taxref, tichy$taxa2)
m6 <- match(taxolist$accepted_gbif, tichy$taxa2)
m_tichy <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
m_tichy <- ifelse(!is.na(m_tichy), m_tichy, ifelse(!is.na(m4),m4, ifelse(!is.na(m5), m5, m6)))
prop.table(table(!is.na(m_tichy))) #87%





# 3. life form from Dfevojan -----------------
dfevojan<- readxl::read_xlsx(here::here(traitfolder, "Life_form_Dfevojan_2023.xlsx"))
dfevojan$taxa <- clean_taxo(dfevojan$FloraVeg.Taxon)
m1 <- match(taxolist$original_taxa, dfevojan$taxa)
m2 <- match(taxolist$accepted_taxref, dfevojan$taxa)
m3 <- match(taxolist$accepted_gbif, dfevojan$taxa)
m_dfevojan <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
prop.table(table(!is.na(m_dfevojan))) #80%





# 4. seed dispersal from Losova -----------------
losova <- readxl::read_xlsx(here::here(traitfolder, "Lososova_et_al_2023_Dispersal_version2_2024-06-14.xlsx"))
losova$taxa <- clean_taxo(losova$Taxon)
m1 <- match(taxolist$original_taxa, losova$taxa)
m2 <- match(taxolist$accepted_taxref, losova$taxa)
m3 <- match(taxolist$accepted_gbif, losova$taxa)
m_losova <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
prop.table(table(!is.na(m_losova))) #80%





# 5. parasitism from Tesitel --------------------
# is this interesting for us?
tesitel<- readxl::read_xlsx(here::here(traitfolder, "Tesitel-et-al-Parasitism-mycotrophy.xlsx"))
tesitel$taxa <- clean_taxo(tesitel$TaxonFloraVeg)
m1 <- match(taxolist$original_taxa, tesitel$taxa)
m2 <- match(taxolist$accepted_taxref, tesitel$taxa)
m3 <- match(taxolist$accepted_gbif, tesitel$taxa)
m_tesitel <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
prop.table(table(!is.na(m_tesitel))) #80%





# why not present? mostly genus
taxolist$original_taxa[is.na(m_losova)]
table(taxolist$gbif_rank[is.na(m_losova)])
# subspecies or genus : trait database are often resolved at species level or lower
# so 142 of our taxa are genus or higher, so always missing :10%
# what should we do with genus?
# get average/median value of the species we have (but what about categorical...)?
