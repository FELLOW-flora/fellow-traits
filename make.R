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

# 3 Get traits (one script per type of database)
# FloraVeg.eu
source(here::here("analyses", "03a_get_traits_floraveg.R"))
# baseflor
source(here::here("analyses", "03b_get_traits_baseflor.R"))
# private databases
source(here::here("analyses", "03c_get_traits_privatedb.R"))
# BIEN
source(here::here("analyses", "03d_get_BIEN.R"))
