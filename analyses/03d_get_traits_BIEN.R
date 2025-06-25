# Get trait information from https://bien.nceas.ucsb.edu/bien/
# input:
#    species list in species_short_list.csv
#    synonyms list in species_known_synonyms.csv
#    metadata in traits/Metatraits.xlsx
#    trait db downloaded from BIEN::BIEN_trait_species()
# output: traitD_BIEN.csv

# check out documentations:
# https://cran.r-project.org/web/packages/BIEN/index.html
# https://cran.r-project.org/web/packages/BIEN/vignettes/BIEN_tutorial.html

# 0. Load packages, data, set parameters ----------------------
# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all() # or source(here::here("R", "clean_taxo.R"))

# Load species list with taxonomy
taxolist <- read.csv(
  here::here("data", "derived-data", "species_short_list.csv")
)

# load metadata of traits (defining which traits are kept)
meta <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "Metatraits.xlsx")
)

# library("BIEN")

# 1. Download the BIEN database ----------------------
# only keep species or sub-species level
keep <- taxolist$accepted_rank %in% c("SPECIES", "SUBSPECIES", "VARIETY")
sp <- unique(taxolist$accepted_taxa[keep])
bien_db <- BIEN::BIEN_trait_species(sp)
# takes a bit of time to run, but ok... 35 page of records
dim(bien_db) # 344629     13


# 2. Calculate average per trait and per species -----
# one issue with multiple units
# for plant flowering begin (months and date)
# transform date to month
flowering_date <- bien_db$trait_name %in%
  "plant flowering begin" &
  bien_db$unit %in% "date"
# subset the month from the date
bien_db$trait_value[flowering_date] <- substr(
  bien_db$trait_value[flowering_date],
  1,
  regexpr("/", bien_db$trait_value[flowering_date]) - 1
)
# update the unit
bien_db$unit[flowering_date] <- "month"

# add unit to trait name
bien_db$trait_lab <- gsub(" ", ".", paste(bien_db$trait_name, bien_db$unit))
num_trait <- !grepl(".NA$", bien_db$trait_lab)
bien_db$trait_lab <- gsub(".NA$", "", bien_db$trait_lab)

bien_db$trait_value[bien_db$trait_value == ""] <- NA
out <- extract_trait_taxalist(
  trait_df = bien_db,
  trait_sp = "scrubbed_species_binomial",
  meta_trait = meta[meta$database %in% "BIEN", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms,
  long = TRUE,
  trait_label = "trait_lab",
  trait_value = "trait_value"
) # 82.19 %
names(out)[-1] <- paste(names(out)[-1], "BIEN", sep = "_")

# 3. Export -------------------------------------
write.csv(
  out,
  file = here::here("data", "derived-data", "traitD_BIEN.csv"),
  row.names = FALSE
)

# missing traits
print(apply(is.na(out), 2, sum))
