#' fellow-traits: A Research Compendium
#'
#' @description
#' extract trait information for the species in the FELLOW database
#'
#' @author Romain Frelat
#' @date 2 April 2025


## Install Dependencies (listed in DESCRIPTION) ----
# rdeps::add_deps() # update automatically the list of dependencies

if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions) -------------

devtools::load_all(here::here())


## Run Project ----

# 1 Merge all species lists in species_list_raw.rds
source(here::here("analyses", "01_get_specieslist.R"))

# 2 Clean taxo with TaxRef and GBIF
source(here::here("analyses", "02_clean_taxo.R"))

# 3 Get traits
source(here::here("analyses", "03_get_traits.R"))
# so far only percentage of species covered by trait database are computed
# trait extraction to be defined ...