# Merge trait databases that were previously pre-processed
#
# input:
#    species list in species_list_taxo.csv
#    trait databases in data/derived-data/traitX_zzz.csv
# output: merged_traits.csv

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all() # or source(here::here("R", "clean_taxo.R"))

traitdir <- here::here("data", "derived-data")
filelist <- list.files(traitdir)
traitfile <- grep("^trait", filelist, value = TRUE)

datalist = lapply(traitfile, function(x) {
  read.csv(file = here::here(traitdir, x), check.names = FALSE)
})
# check number of rows: sapply(datalist, nrow)
traits <- do.call(cbind, datalist)
# remove duplicated 'accepted_taxa' column
traits <- traits[, !duplicated(names(traits))]
# empty string = NA
traits[traits == ""] <- NA

#remove empty traits
traits <- traits[, apply(is.na(traits), 2, sum) < nrow(traits)]

print(dim(traits)) #2027 taxa, 131 traits


# 3. Export merged trait data ---------------------------
write.csv(
  traits,
  file = here::here("data", "derived-data", "merged_traits.csv"),
  row.names = FALSE
)
