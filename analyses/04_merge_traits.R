# Merge trait databases that were previously pre-processed
# and impute trait values for genus
#
# input:
#    species list in species_list_taxo.csv
#    trait databases in data/derived-data/traitX_zzz.csv
# output: merged_traits.csv

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_taxo())
devtools::load_all() # or source(here::here("R", "clean_taxo.R"))

# Load species list with taxonomy
taxolist <- read.csv(
  here::here("data", "derived-data", "species_list_taxo.csv")
)

traitdir <- here::here("data", "derived-data")
filelist <- list.files(traitdir)
traitfile <- grep("^trait", filelist, value = TRUE)


datalist = lapply(traitfile, function(x) {
  read.csv(file = here::here(traitdir, x))
})
# sapply(datalist, nrow)
traits <- do.call(cbind, datalist)
# remove duplicated accepted_taxa
traits <- traits[, !duplicated(names(traits))]
dim(traits) #1359 taxa, 150 traits

# fill in the genus values : 128 genus
# table(taxolist$taxref_rank, taxolist$gbif_rank)
is_genus <- taxolist$taxref_rank %in% "GN"

gen <- sapply(strsplit(taxolist$accepted_taxa, " "), function(x) x[[1]])

nums <- unlist(lapply(traits, is.numeric), use.names = FALSE)

mean_genus_num <- aggregate(
  traits[, nums],
  by = list(gen),
  FUN = mean,
  na.rm = TRUE
)
# replace NaN by NA
mean_genus_num[is.na(mean_genus_num)] <- NA
# replace in trait dataset
m_gen <- match(taxolist$accepted_taxref[is_genus], mean_genus_num$Group.1)
traits[is_genus, nums] <- mean_genus_num[m_gen, -1]

# for categorical variable (including taxonomic match)
cats <- !(nums | names(traits) %in% "accepted_taxa")

mean_genus_cat <- aggregate(
  traits[, cats],
  by = list(gen),
  FUN = paste_unique
)
m_gen <- match(taxolist$accepted_taxref[is_genus], mean_genus_cat$Group.1)
traits[is_genus, cats] <- mean_genus_cat[m_gen, -1]

# 3. Export merged trait data ---------------------------
write.csv(
  traits,
  file = here::here("data", "derived-data", "merged_traits.csv"),
  row.names = FALSE
)
