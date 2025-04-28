# Get trait information from private trait database
# input:
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    trait databases in data/raw-data/traits/private
#    metadata in traits/Metatraits.xlsx
# output: traitC_privatedb.csv

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
dim(yvoz)
# clean species name
yvoz$taxa <- clean_taxo(yvoz$latin_name)
# replace if known synonyms (none ...)
is_syn <- yvoz$taxa %in% synonyms$synonym_taxa
yvoz$taxa[is_syn] <- synonyms$accepted_taxa[match(
  yvoz$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, yvoz$taxa)
m2 <- match(taxolist$accepted_taxref, yvoz$taxa)
m3 <- match(taxolist$accepted_gbif, yvoz$taxa)
m_yvoz <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_yvoz))) #60

# select the interesting columns
keepT <- c("latin_name", meta$original.name[meta$database %in% "Yvoz_ValPol"])
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "Yvoz_ValPol"]),
  "Yvoz_ValPol",
  sep = "_"
)

t1 <- yvoz[m_yvoz, keepT]
names(t1) <- newlab


# 2. traits from the SP_Vignes ------------
vignes <- readxl::read_xls(here::here(traitfolder, "TraitSpVignes.xls"))
dim(vignes) # only 123 species
# too small dataset, is it worth the effort?

# clean and check synonyms
vignes$taxa <- clean_taxo(vignes$Species)
is_syn <- vignes$taxa %in% synonyms$synonym_taxa
vignes$taxa[is_syn] <- synonyms$accepted_taxa[match(
  vignes$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, vignes$taxa)
m2 <- match(taxolist$accepted_taxref, vignes$taxa)
m3 <- match(taxolist$accepted_gbif, vignes$taxa)

m_vignes <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_vignes))) #9%

keepT <- c("Species", meta$original.name[meta$database %in% "SPVignes"])
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "SPVignes"]),
  "SPVignes",
  sep = "_"
)

t2 <- vignes[m_vignes, keepT]
names(t2) <- newlab


# 3. traits from the AT_VINEDIVERS ------------
spfolder <- here::here("data", "raw-data", "species-list")
vinedivers <- readxl::read_xlsx(
  here::here(spfolder, "At_Fr_Ro_Sp_Species_List_VINEDIVERS.xlsx"),
  sheet = 2
)
names(vinedivers)[1:2] <- c("species", "abb")
dim(vinedivers) # only 240 species
# again very small dataset, is it worth the effort?

# clean and check synonyms
vinedivers$taxa <- clean_taxo(vinedivers$species)
is_syn <- vinedivers$taxa %in% synonyms$synonym_taxa
vinedivers$taxa[is_syn] <- synonyms$accepted_taxa[match(
  vinedivers$taxa[is_syn],
  synonyms$synonym_taxa
)]
m1 <- match(taxolist$original_taxa, vinedivers$taxa)
m2 <- match(taxolist$accepted_taxref, vinedivers$taxa)
m3 <- match(taxolist$accepted_gbif, vinedivers$taxa)
m_vinedivers <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2), m2, m3))
prop.table(table(!is.na(m_vinedivers))) #21%

keepT <- c("species", meta$original.name[meta$database %in% "At_Vinedivers"])
newlab <- paste(
  c("original_taxa", meta$new.name[meta$database %in% "At_Vinedivers"]),
  "At_Vinedivers",
  sep = "_"
)

t3 <- vinedivers[m_vinedivers, keepT]
names(t3) <- newlab


# 4. Export -------------------------------------
out <- cbind(
  "accepted_taxa" = taxolist$accepted_taxa,
  t1,
  t2,
  t3
)

write.csv(
  out,
  file = here::here("data", "derived-data", "traitC_privatedb.csv"),
  row.names = FALSE
)
# missing traits
apply(is.na(out), 2, sum)
table(apply(is.na(out), 1, sum))
View(out)
