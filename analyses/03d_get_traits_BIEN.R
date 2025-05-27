# Get trait information from https://bien.nceas.ucsb.edu/bien/
# input:
#    species list in species_list_taxo.csv
#    metadata in traits/Metatraits.xlsx
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

library("BIEN")

# 1. Download the BIEN database ----------------------
# only keep species or sub-species level
keep <- taxolist$accepted_rank %in% c("SPECIES", "SUBSPECIES", "VARIETY")
sp <- unique(taxolist$accepted_taxa[keep])
# GBIF (accepted_gbif) had more records than the "accepted_taxa"
bien_db <- BIEN_trait_species(sp)
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

numT <- meta$original.name[meta$database == "BIEN" & meta$type == "numeric"]
# average per trait and per species
bien_num <- bien_db[bien_db$trait_lab %in% numT, ]
trait_av <- tapply(
  as.numeric(bien_num$trait_value),
  list(bien_num$scrubbed_species_binomial, bien_num$trait_lab),
  mean,
  na.rm = TRUE
)
newlabN <- meta$new.name[meta$database == "BIEN" & meta$type == "numeric"]
colnames(trait_av) <- paste(newlabN, "BIEN", sep = "_")
# might be worth checking the sd of the traits ...
# trait_sd <- tapply(as.numeric(bien_num$trait_value), list(bien_num$scrubbed_species_binomial, bien_num$trait_lab), sd, na.rm=TRUE)
# or to manually verify some values
# check <- bien_db$scrubbed_species_binomial%in%"Achillea millefolium" & bien_db$trait_lab%in%"leaf.area.mm2"
# table(bien_db$scrubbed_species_binomial%in%"%Achillea millefolium")

catT <- meta$original.name[meta$database == "BIEN" & meta$type == "categorical"]
bien_cat <- bien_db[bien_db$trait_lab %in% catT, ]
trait_cat <- tapply(
  bien_cat$trait_value,
  list(bien_cat$scrubbed_species_binomial, bien_cat$trait_lab),
  concat
)
newlabC <- meta$new.name[meta$database == "BIEN" & meta$type == "categorical"]
colnames(trait_cat) <- paste(colnames(trait_cat), "BIEN", sep = "_")


# 3. Export -------------------------------------
# merge the traits information
m_num <- match(taxolist$accepted_taxa, row.names(trait_av))
m_cat <- match(taxolist$accepted_taxa, row.names(trait_cat))

out <- cbind(
  "accepted_taxa" = taxolist$accepted_taxa,
  "original_taxa_BIEN" = ifelse(
    is.na(m_num) & is.na(m_cat),
    NA,
    taxolist$accepted_taxa
  ),
  trait_av[m_num, ],
  trait_cat[m_cat, ]
)

write.csv(
  out,
  file = here::here("data", "derived-data", "traitD_BIEN.csv"),
  row.names = FALSE
)
# missing traits
apply(is.na(out), 2, sum)
