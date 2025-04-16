# Get trait information from Baseflor
# received on 04/04/2025
# version from 07/03/2025: https://www.tela-botanica.org/projets/phytosociologie/porte-documents/
# input: 
#    species list in species_list_taxo.csv
#    synonyms list in species_known_synonyms.csv
#    traits in traits/baseflor.xlsx
#    metadata in traits/Metatraits.xlsx
# output: traitB_baseflor.csv

# 0. Load packages, data, set parameters ----------------------
# if the script is not run from make.R, need to load home made functions (e.g. clean_taxo())
devtools::load_all()
# or source(here::here("R", "clean_taxo.R"))

# Load species list with taxonomy
taxolist <- read.csv(here::here("data", "derived-data","species_list_taxo.csv"))
# and synonyms
synonyms<- read.csv(here::here("data", "derived-data","species_known_synonyms.csv"))

# load metadata of traits (defining which traits are kept)
meta <- readxl::read_xlsx(here::here("data", "raw-data", "traits", "Metatraits.xlsx"))

# load the trait database : baseflor
baseflor <- readxl::read_xlsx(here::here("data", "raw-data", "traits", "baseflor.xlsx"))


# 1. Match species names ----------------------
# deal with NA in nomH
na_taxa <- is.na(baseflor$nomH)
baseflor$nomH[na_taxa] <- clean_ref(baseflor$NOM_SCIENTIFIQUE[na_taxa])

# clean species name
baseflor$taxa <- gsub(" \\*$", "", clean_taxo(baseflor$nomH))
# replace if known synonyms
is_syn <- baseflor$taxa %in% synonyms$synonym_taxa
baseflor$taxa[is_syn] <- synonyms$accepted_taxa[match(baseflor$taxa[is_syn], synonyms$synonym_taxa)]


# m0 <- match(taxolist$accepted_taxa, baseflor$taxa) # miss 19 taxa, but not sure what is the best strategy
m1 <- match(taxolist$original_taxa, baseflor$taxa)
m2 <- match(taxolist$accepted_taxref, baseflor$taxa)
m3 <- match(taxolist$accepted_gbif, baseflor$taxa)
m_baseflor <- ifelse(!is.na(m1), m1, ifelse(!is.na(m2),m2, m3))
prop.table(table(!is.na(m_baseflor))) #79%

# 2. Format trait data ---------------------------

keepT <- meta$original.name[meta$database%in%"Baseflor"]
newlab <- paste(c("original_taxa", meta$new.name[meta$database%in%"Baseflor"]), "Baseflor", sep="_")

out <- cbind(
    taxolist$accepted_taxa,
    baseflor$nomH[m_baseflor],
    baseflor[m_baseflor, keepT])
names(out) <- c("accepted_taxa", newlab)


# 3. Export trait data ---------------------------
write.csv(out, 
    file = here::here("data", "derived-data", "traitB_baseflor.csv"), row.names=FALSE)

#summary of NAs
apply(is.na(out),2,sum)

