# Get trait information from https://floraveg.eu/
# input:
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    trait databases in data/raw-data/traits
#    metadata in traits/Metatraits.xlsx
# output: traitA_floraveg.csv

# if the script is not run from make.R, need to load home made functions (clean_taxo())
devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load species list with taxonomy
taxolist <- read.csv(here::here(
  "data",
  "derived-data",
  "species_list_taxo.csv"
))
# and synonyms
synonyms <- read.csv(here::here(
  "data",
  "derived-data",
  "species_known_synonyms.csv"
))

# set the folder with traits data
traitfolder <- here::here("data", "raw-data", "traits", "FloraVeg")

# load metadata of traits (which define which traits are kept)
meta <- readxl::read_xlsx(here::here(
  "data",
  "raw-data",
  "traits",
  "Metatraits.xlsx"
))

# need to define:
# A. what should we do with taxa that are not species (e.g. genus, not present in trait database)?
# B. what should we do with missing species? fuzzy match? look for gbif synonyms?

# 1. Disturbance from Midolo ----------------------
midolo <- readxl::read_xlsx(here::here(
  traitfolder,
  "disturbance_indicator_values_Midolo_2023.xlsx"
))
# clean species name
midolo$taxa <- clean_taxo(midolo$species)
# replace if known synonyms
is_syn <- midolo$taxa %in% synonyms$synonym_taxa
midolo$taxa[is_syn] <- synonyms$accepted_taxa[match(
  midolo$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, midolo$taxa)
m2 <- match(taxolist$accepted_taxref, midolo$taxa)
m3 <- match(taxolist$accepted_gbif, midolo$taxa)
m_midolo <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_midolo))) #71% > 75% (with synonyms)

# so far, let's focus on traits only (not their SD)
keepT <- c("species", meta$original.name[meta$database %in% "Midolo2023"])
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "Midolo2023"]),
  "Midolo2023",
  sep = "_"
)
t1 <- midolo[m_midolo, keepT]
names(t1) <- newlab


# 2. Ellenberg from Tichy --------------------
tichy <- readxl::read_xlsx(
  here::here(traitfolder, "Ellenberg_Indicator_values_Tichy_2022.xlsx"),
  sheet = 10
)
names(tichy)[1:2] <- c("SeqID", "Taxon")
tichy <- tichy[-1, ]

# clean species name (simplified and original)
tichy$taxa <- clean_taxo(tichy$Taxon)
# replace if known synonyms
is_syn <- tichy$taxa %in% synonyms$synonym_taxa
tichy$taxa[is_syn] <- synonyms$accepted_taxa[match(
  tichy$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, tichy$taxa)
m2 <- match(taxolist$accepted_taxref, tichy$taxa)
m3 <- match(taxolist$accepted_gbif, tichy$taxa)
m_tichy <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_tichy))) #80%

# so far, let's focus on average Ellenberg values
keepT <- c("Taxon", meta$original.name[meta$database %in% "Tichy2022"])
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "Tichy2022"]),
  "Tichy2022",
  sep = "_"
)
t2 <- tichy[m_tichy, keepT]
names(t2) <- newlab


# 3. seed dispersal from Losova -----------------
lososova <- readxl::read_xlsx(here::here(
  traitfolder,
  "Lososova_et_al_2023_Dispersal_version2_2024-06-14.xlsx"
))
# clean and check synonyms
lososova$taxa <- clean_taxo(lososova$Taxon)
is_syn <- lososova$taxa %in% synonyms$synonym_taxa
lososova$taxa[is_syn] <- synonyms$accepted_taxa[match(
  lososova$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, lososova$taxa)
m2 <- match(taxolist$accepted_taxref, lososova$taxa)
m3 <- match(taxolist$accepted_gbif, lososova$taxa)

m_lososova <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_lososova))) #86%

lososova$`Plant height (m)` <- suppressWarnings(as.numeric(
  lososova$`Plant height (m)`
))

keepT <- c("Taxon", meta$original.name[meta$database %in% "Lososova2023"])
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "Lososova2023"]),
  "Lososova2023",
  sep = "_"
)

t3 <- lososova[m_lososova, keepT]
names(t3) <- newlab

# check that numeric traits are numeric
# metanum <- !is.na(meta$units[meta$database %in% "Lososova_2023"])
# metanum <- meta$type[meta$database %in% "Lososova2023"] == "numeric"
# datanum <- unlist(lapply(t3[, -1], is.numeric))
# if (any(metanum != datanum)) {
#   print(names(datanum)[metanum != datanum])
# }

# 4. life form from Dfevojan -----------------
dfevojan <- readxl::read_xlsx(here::here(
  traitfolder,
  "Life_form_Dfevojan_2023.xlsx"
))
# clean and check synonyms
dfevojan$taxa <- clean_taxo(dfevojan$FloraVeg.Taxon)
is_syn <- dfevojan$taxa %in% synonyms$synonym_taxa
dfevojan$taxa[is_syn] <- synonyms$accepted_taxa[match(
  dfevojan$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, dfevojan$taxa)
m2 <- match(taxolist$accepted_taxref, dfevojan$taxa)
m3 <- match(taxolist$accepted_gbif, dfevojan$taxa)

m_dfevojan <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_dfevojan))) #86%

keepT <- c(
  "FloraVeg.Taxon",
  meta$original.name[meta$database %in% "Dfevojan2023"]
)
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "Dfevojan2023"]),
  "Dfevojan2023",
  sep = "_"
)

t4 <- dfevojan[m_dfevojan, keepT]
names(t4) <- newlab

# 5. parasitism from Tesitel --------------------
# is this interesting for us?
# tesitel<- readxl::read_xlsx(here::here(traitfolder, "Tesitel-et-al-Parasitism-mycotrophy.xlsx"))
# tesitel$taxa <- clean_taxo(tesitel$TaxonFloraVeg)
# m1 <- match(taxolist$original_taxa, tesitel$taxa)
# m2 <- match(taxolist$accepted_taxref, tesitel$taxa)
# m3 <- match(taxolist$accepted_gbif, tesitel$taxa)
# m_tesitel <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
# prop.table(table(!is.na(m_tesitel))) #81%

# 6. Export -------------------------------------
out <- cbind(
  "accepted_taxa" = taxolist$accepted_taxa,
  t1,
  t2,
  t3,
  t4
)

write.csv(
  out,
  file = here::here("data", "derived-data", "traitA_floraveg.csv"),
  row.names = FALSE
)
# missing traits
print(apply(is.na(out), 2, sum))

# why not present? mostly genus
# taxolist$original_taxa[is.na(m_lososova)]
# table(taxolist$gbif_rank[is.na(m_lososova)])

# species <- taxolist$accepted_taxa[
#   taxolist$taxref_rank %in% c("ES", "SSES", "VAR")
# ]
# genus <- taxolist$accepted_taxa[taxolist$taxref_rank %in% "GN"]
# gen_sp <- sapply(strsplit(species, " "), function(x) x[[1]])
# table(genus %in% gen_sp)
# genus[!genus %in% gen_sp]

# # so 142 of our taxa are genus or higher, so always missing :10%
# # what should we do with genus?
# # get average/median value of the species we have (but what about categorical...)?
