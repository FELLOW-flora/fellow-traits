#' fellow-traits: A Research Compendium
#'
#' @description
#' extract trait information for the species in the FELLOW database
#'
#' @author Romain Frelat
#' @date 16 May 2025

## Install Dependencies (listed in DESCRIPTION) ----
# rdeps::add_deps() # update automatically the list of dependencies

if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions) -------------

devtools::load_all()


## Run Project ----

# 1 Merge all species lists in species_list_raw.rds
source(here::here("analyses", "01_get_specieslist.R"))

# 2 Clean taxo with TaxRef and GBIF
source(here::here("analyses", "02_clean_taxo.R"))

# 3 Get traits (one script per database)
# FloraVeg.eu
source(here::here("analyses", "03a_get_traits_floraveg.R"))
# baseflor
source(here::here("analyses", "03b_get_traits_baseflor.R"))
# private databases
source(here::here("analyses", "03c_get_traits_privatedb.R"))
# BIEN
source(here::here("analyses", "03d_get_traits_BIEN.R"))
# D3
source(here::here("analyses", "03e_get_traits_Dcube.R"))
# Hodgson 2023
source(here::here("analyses", "03f_get_traits_Hodgson2023.R"))
# GIFT
source(here::here("analyses", "03g_get_traits_GIFT.R"))

# 4 Merge traits
source(here::here("analyses", "04_merge_traits.R"))

# 5 Explore trait dataset
quarto::quarto_render(here::here("analyses", "05_explore_traits.qmd"))
