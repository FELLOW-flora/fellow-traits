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

print(dim(traits)) # 2476 taxa, 100 traits


# 3. Export merged trait data ---------------------------
write.csv(
  traits,
  file = here::here("data", "derived-data", "merged_traits.csv"),
  row.names = FALSE
)

# 4. Simplify trait data -------------------------------

# initiate trait value sheet
values <- data.frame("taxa" = traits$accepted_taxa)

# initiate taxonomic sheet
taxo <- data.frame("taxa" = traits$accepted_taxa)

# initiate database sheet
taxolist <- read.csv(
  here::here("data", "derived-data", "species_list_taxo.csv")
)
mt <- match(traits$accepted_taxa, taxolist$accepted_taxa)
istaxref <- taxolist$wfo_status[mt]
db <- data.frame("taxa" = paste0("WFO_", taxolist$wfo_status[mt]))

# along all traits
trdb <- get_last(names(traits))
tr <- get_notlast(names(traits))

remove_traits <- c(
  "accepted",
  "original_taxa",
  grepv("Plant.height", tr), #plant height
  grepv("SLA", tr), #sla
  grepv("Seed.mass", tr), #seed mass
  tr[duplicated(tr)] # make sure no duplicates
)
# yet to be checked:
# "Dispersal.mode"
# "Flower.color"
# "Inflorescence"
# "Lifecycle"

# the simple cases
for (i in which(!tr %in% remove_traits)) {
  values[, tr[i]] <- traits[, i]
  db[, tr[i]] <- ifelse(is.na(traits[, i]), NA, trdb[i])
  taxo[, tr[i]] <- ifelse(
    is.na(traits[, i]),
    NA,
    traits[, paste0("original_taxa_", trdb[i])]
  )
}

# height values
height <- data.frame(
  "Lososova2023" = traits$Plant.height_m_Lososova2023,
  "Ladouceur2019" = traits$Plant.height_cm_Ladouceur2019 / 100,
  "GIFT" = traits$Plant.height_mean_m_GIFT,
  #"BIEN" = traits$Plant.height_m_BIEN,
  "FlorealData" = traits$Plant.height_cm_FlorealData / 100,
  "Ecoflora" = traits$Plant.height_m_Ecoflora,
  "SPVignes" = traits$Plant.height_cm_SPVignes / 100
)

values$Plant.height_m <- apply(height, 1, function(x) x[!is.na(x)][1])
db$Plant.height_m <- apply(height, 1, function(x) names(x)[!is.na(x)][1])
mph <- ifelse(
  is.na(db$Plant.height_m),
  "accepted_taxa",
  paste0("original_taxa_", db$Plant.height_m)
) |>
  match(names(traits))
taxo$Plant.height_m <- ifelse(
  is.na(values$Plant.height_m),
  NA,
  traits[cbind(1:nrow(traits), mph)]
)

# SLA values
sla <- data.frame(
  "Hodgson2023" = traits[, "SLA_mm2.mg-1_Hodgson2023"],
  "Ladouceur2019" = traits[, "SLA_cm2.g-1_Ladouceur2019"] / 10,
  "GIFT" = traits[, "SLA_cm2.g-1_GIFT"] / 10
  #"BIEN" = traits[, "SLA_m2.kg-1_BIEN"]
)

values$SLA_m2.kg.1 <- apply(sla, 1, function(x) x[!is.na(x)][1])
db$SLA_m2.kg.1 <- apply(sla, 1, function(x) names(x)[!is.na(x)][1])
msla <- ifelse(
  is.na(db$SLA_m2.kg.1),
  "accepted_taxa",
  paste0("original_taxa_", db$SLA_m2.kg.1)
) |>
  match(names(traits))
taxo$SLA_m2.kg.1 <- ifelse(
  is.na(values$SLA_m2.kg.1),
  NA,
  traits[cbind(1:nrow(traits), msla)]
)

# seed mass
seed <- data.frame(
  "Lososova2023" = traits$Seed.mass_mg_Lososova2023,
  "Ladouceur2019" = traits$Seed.mass_mg_Ladouceur2019,
  "GIFT" = traits$Seed.mass_g_GIFT * 1000,
  #"BIEN" = traits$Seed.mass_mg_BIEN,
  "Ecoflora" = traits$Seed.mass_mg_Ecoflora,
  "Biolflor" = traits$Seed.mass_mg_Biolflor
)
values$Seed.mass_mg <- apply(seed, 1, function(x) x[!is.na(x)][1])
db$Seed.mass_mg <- apply(seed, 1, function(x) names(x)[!is.na(x)][1])
msm <- ifelse(
  is.na(db$Seed.mass_mg),
  "accepted_taxa",
  paste0("original_taxa_", db$Seed.mass_mg)
) |>
  match(names(traits))
taxo$Seed.mass_mg <- ifelse(
  is.na(values$Seed.mass_mg),
  NA,
  traits[cbind(1:nrow(traits), msm)]
)

# clonal index
clonal <- data.frame(
  "Ladouceur2019" = traits$Clonal.index_Ladouceur2019,
  "CLOPLA" = traits$Clonal.index_CLOPLA
)
values$Clonal.index <- apply(clonal, 1, function(x) x[!is.na(x)][1])
db$Clonal.index <- apply(clonal, 1, function(x) names(x)[!is.na(x)][1])
mci <- ifelse(
  is.na(db$Clonal.index),
  "accepted_taxa",
  paste0("original_taxa_", db$Clonal.index)
) |>
  match(names(traits))
taxo$Clonal.index <- ifelse(
  is.na(values$Clonal.index),
  NA,
  traits[cbind(1:nrow(traits), mci)]
)

# Lifecycle
convLC <- c(
  "Ann" = "Annual",
  "Bienn" = "Biennial",
  "Herb.perenn" = "Perennial",
  "Woody.peren" = "Perennial"
)
traits$convLC_Hodgson2023 <- as.character(convLC[traits$Lifecycle_Hodgson2023])
lifecycle <- data.frame(
  "Hodgson2023" = traits$convLC_Hodgson2023,
  "GIFT" = traits$Lifecycle_GIFT
)
values$Lifecycle <- apply(lifecycle, 1, function(x) x[!is.na(x)][1])
db$Lifecycle <- apply(lifecycle, 1, function(x) names(x)[!is.na(x)][1])
mlc <- ifelse(
  is.na(db$Lifecycle),
  "accepted_taxa",
  paste0("original_taxa_", db$Lifecycle)
) |>
  match(names(traits))
taxo$Lifecycle <- ifelse(
  is.na(values$Lifecycle),
  NA,
  traits[cbind(1:nrow(traits), mlc)]
)

# order traits and remove species without trait values
ordC <- c(1, order(names(values)[-1]) + 1)
rmR <- apply(!is.na(values), 1, sum) > 1

# add trait description
meta <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "Metatraits.xlsx")
)
m0 <- match(names(values)[ordC][-1], meta$new.name)
info <- meta[m0, c(3, 1, 4:8)]
names(info)[1] <- "trait"
info$trait <- names(values)[ordC][-1]

#fmt: skip
info[info$trait == "Plant.height_m", "database"] <- "Lososova2023, Ladouceur2019, GIFT, FlorealData, Ecoflora, SPVignes" # BIEN
#fmt: skip
info[info$trait == "SLA_m2.kg.1",] <- meta[meta$new.name %in% "SLA_m2.kg-1", c(3, 1, 4:8)]
info[info$trait == "SLA_m2.kg-1", "trait"] <- "SLA_m2.kg.1"
#fmt: skip
info[info$trait == "SLA_m2.kg.1", "database"] <- "Hodgson2023, Ladouceur2019, GIFT" # BIEN
#fmt: skip
info[info$trait == "Seed.mass_mg", "database"] <- "Lososova2023, Ladouceur2019, GIFT, Ecoflora, Biolflor" # BIEN

#fmt: skip
info[info$trait == "Clonal.index", "database"] <- "Ladouceur2019, CLOPLA"
#fmt: skip
info[info$trait == "Lifecycle", "database"] <- "Hodgson2023, GIFT"


# add database description
dbmeta <- readxl::read_xlsx(
  here::here("data", "raw-data", "traits", "Metatraits.xlsx"),
  sheet = 2
)

writexl::write_xlsx(
  list(
    traits = values[rmR, ordC],
    database = db[rmR, ordC],
    taxa = taxo[rmR, ordC],
    trait_meta = info,
    db_meta = dbmeta
  ),
  path = here::here("data", "derived-data", "fellow_traits.xlsx"),
)
