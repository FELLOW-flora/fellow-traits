#' fellow-traits: A Research Compendium
#'
#' @description
#' extract trait information for the tropical weeds
#'
#' @author Romain Frelat
#' @date 28 January 2026

## Install Dependencies (listed in DESCRIPTION) ----
# rdeps::add_deps() # update automatically the list of dependencies

if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions) -------------
devtools::load_all()

## Run Project ----

# 1 Merge and clean species list with GBIF taxonomical backbone
source(here::here("analyses", "s1_get_clean_taxo.R"))

# 2 Get traits (one script per database)
# FloraVeg.eu
# source(here::here("analyses", "03a_get_traits_floraveg.R"))
# baseflor
source(here::here("analyses", "03b_get_traits_baseflor.R"))
# BIEN
source(here::here("analyses", "03d_get_traits_BIEN.R"))
# D3
source(here::here("analyses", "03e_get_traits_Dcube.R"))
# Hodgson 2023
source(here::here("analyses", "03f_get_traits_Hodgson2023.R"))
# GIFT (takes longer to compute)
# source(here::here("analyses", "03g_get_traits_GIFT.R"))
# TRYdb
source(here::here("analyses", "03i_get_traits_TRYdb.R"))
# Groot
source(here::here("analyses", "03j_get_traits_Groot.R"))
# CLO-PLA
source(here::here("analyses", "03k_get_traits_CLOPLA.R"))
# Ladouceur 2019
source(here::here("analyses", "03l_get_traits_Ladouceur2019.R"))

# 4 Merge traits
source(here::here("analyses", "04_merge_traits.R"))

# 5 Explore trait dataset
quarto::quarto_render(here::here("analyses", "05_explore_traits.qmd"))

# run in less than 30 minutes
