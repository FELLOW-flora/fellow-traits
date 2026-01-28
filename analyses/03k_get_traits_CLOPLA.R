# Get trait information from https://www.cahiersagricultures.fr/articles/cagri/pdf/2021/01/cagri210051.pdf
# input:
#    species list in species_short_list.csv
#    synonyms list in species_known_synonyms.csv
#    trait data in raw-data/traits/CLO-PLA/CLO-PLA-traits.txt
#    metadata in traits/Metatraits.xlsx
# output: traitK_CLOPLA.csv

# 0. Load packages, data, set parameters ----------------------

# if the script is not run from make.R, need to load home made functions (clean_species_list())
devtools::load_all()

# Load species list with taxonomy
taxolist <- read.csv(
  here::here("data", "tropical-data", "species_short_list.csv")
)

# and synonyms
synonyms <- read.csv(
  here::here("data", "tropical-data", "species_known_synonyms.csv")
)

# load metadata of traits (defining which traits are kept)
meta <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "Metatraits.xlsx")
)

# Load CLO-PLA
clopla <- read.table(
  here::here("data", "raw-data", "traits", "CLO-PLA", "CLO-PLA-traits.txt"),
  header = TRUE,
  sep = "\t",
  encoding = "latin1"
)


# 1. Extract trait values ----------------------
out <- extract_trait_taxalist(
  trait_df = clopla,
  trait_sp = "Species_name",
  meta_trait = meta[meta$database %in% "CLO-PLA", ],
  taxalist = taxolist$accepted_taxa,
  synonyms = synonyms
) # 64.23 %
names(out)[-1] <- paste(names(out)[-1], "CLOPLA", sep = "_")


# 2. Export trait data ---------------------------
write.csv(
  out,
  file = here::here("data", "tropical-data", "traitK_CLOPLA.csv"),
  row.names = FALSE
)

#summary of NAs
print(apply(!is.na(out), 2, sum))
