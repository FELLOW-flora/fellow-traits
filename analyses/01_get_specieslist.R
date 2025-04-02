# Merge single database species list to FELLOW species list
# keeping three columns:
# taxa : the species name
# original_ID : the original ID of the species (if any) e.g. EPPO code
# database_ID : the ID of the database 

# metadata from Dataset description.xlsx
# https://nextcloud.inrae.fr/apps/onlyoffice/s/fzCpq86Zwpo3q8Q?fileId=174729236


# 1. Load datasets ---------------------
spfolder <- here::here("data", "raw-data", "species-list")
# list.files(spfolder, "xlsx")
# vinedivers
vinedivers <- readxl::read_xlsx(here::here(spfolder, "At_Fr_Ro_Sp_Species_List_VINEDIVERS.xlsx"))
df1 <- data.frame(
  "taxa"=vinedivers$Species_code,
  "original_ID"=vinedivers$ID_sp_db,
  "database_ID"="AT_VINEDIVERS"
)
# to keep in mind: there are some trait values in the excel sheet
# trait_vinedivers <- readxl::read_xlsx(here::here(spfolder, "At_Fr_Ro_Sp_Species_List_VINEDIVERS.xlsx"), sheet=2)

# at_secbivit
at_secbivit <- readxl::read_xlsx(here::here(spfolder, "At_SECBIVIT_SpeciesLIST.xlsx"), .name_repair="universal")
df2 <- data.frame(
  "taxa"=at_secbivit$Species.name,
  "original_ID"=NA,
  "database_ID"="AT_SECBIVIT"
)

# aby
aby <- readxl::read_xlsx(here::here(spfolder, "Fr_ABY_SpeciesList.xlsx"), .name_repair="universal")
df3 <- data.frame(
  "taxa"=aby$Species.name,
  "original_ID"=aby$EPPO,
  "database_ID"="FR_ABY"
)

# bvg : Biovigilance
bvg <- readxl::read_xlsx(here::here(spfolder, "Fr_BVG_BVG_Vignes_Barralis_Fried_Bopp_Maillet_Fenay2006_Herault_Languedoc.xlsx"), .name_repair="universal")
df4 <- data.frame(
  "taxa"=bvg$Name,
  "original_ID"=bvg$Species,
  "database_ID"="FR_BVG"
)
df4 <- df4[!duplicated(df4),]


# ca-sys
casys <- readxl::read_xlsx(here::here(spfolder, "Fr_CASYS_SpeciesList.xlsx"), .name_repair="universal")
df5 <- data.frame(
  "taxa"=casys$Sp.name,
  "original_ID"=casys$EPPO,
  "database_ID"="FR_CASYS"
)

#cbn
cbn <- readxl::read_xlsx(here::here(spfolder, "Fr_CBN_speciesList.xlsx"), 
                         skip =1, .name_repair="universal")
df6 <- data.frame(
  "taxa"=cbn$NOM_VALIDE,
  "original_ID"=cbn$CODE.TAX.REF.12,
  "database_ID"="FR_CBN"
)

#deepimpact
deepi <- readxl::read_xlsx(here::here(spfolder, "Fr_DEEPIMPACT_SpeciesList.xlsx"), 
                         .name_repair="universal")
df7 <- data.frame(
  "taxa"=deepi$Species.names,
  "original_ID"=deepi$EPPO,
  "database_ID"="FR_DeepImpact"
)

#ENIVTH
enivth <- readxl::read_xlsx(here::here(spfolder, "Fr_ENIVTH_SpeciesList.xlsx"), 
                         .name_repair="universal")
df8 <- data.frame(
  "taxa"=enivth$Code,
  "original_ID"=NA,
  "database_ID"="FR_ENIVTH"
)

#FENAY
fenay <- readxl::read_xlsx(here::here(spfolder, "Fr_FENAY_SpeciesList_EPPO_Completed.xlsx"), 
                         .name_repair="universal")
# only EPPO in original files
# the file was completed from https://gd.eppo.int/

df9 <- data.frame(
  "taxa"=fenay$PrefName,
  "original_ID"=fenay$EPPO,
  "database_ID"="FR_FENAY"
)

#FLAVI
flavi <- readxl::read_xlsx(here::here(spfolder, "Fr_FLAVI_SpeciesLIST.xlsx"), 
                         .name_repair="universal")
df10 <- data.frame(
  "taxa"=flavi$species,
  "original_ID"=flavi$id_flavi,
  "database_ID"="FR_FLAVI"
)

#fr_secbivit
fr_secbivit <- readxl::read_xlsx(here::here(spfolder, "Fr_SECBIVIT_SpeciesLIST.xlsx"), 
                         .name_repair="universal")
df11 <- data.frame(
  "taxa"=fr_secbivit$Species,
  "original_ID"=fr_secbivit$EPPO_Code,
  "database_ID"="FR_SECBIVIT"
)

#L. genty
lgenty <- readxl::read_xlsx(here::here(spfolder, "Fr_species_L_Genty_phd.xlsx"), 
                         .name_repair="universal")
df12 <- data.frame(
  "taxa"=gsub("_", " ", lgenty$Espece),
  "original_ID"=NA,
  "database_ID"="FR_LGENTY"
)


#Vine_bdx
vine_bdx <- readxl::read_xlsx(here::here(spfolder, "Fr_Vine_Bdx.xlsx"), 
                         .name_repair="universal")
df13 <- data.frame(
  "taxa"=gsub("_", " ", vine_bdx$complete_species_name),
  "original_ID"=NA,
  "database_ID"="FR_VINE_CA33"
)

#Vine_ca33
vine_ca33 <- readxl::read_xlsx(here::here(spfolder, "Fr_VineCA33_SpeciesList.xlsx" ), 
                         .name_repair="universal")
df14 <- data.frame(
  "taxa"=vine_ca33$Species.names,
  "original_ID"=vine_ca33$EPPO,
  "database_ID"="FR_VINE_CA33"
)


#de_secbivit
de_secbivit <- readxl::read_xlsx(here::here(spfolder, "Ge_SECBIVIT_SpeciesLIST.xlsx" ), 
                         .name_repair="universal")
df15 <- data.frame(
  "taxa"=de_secbivit$Species,
  "original_ID"=NA,
  "database_ID"="DE_SECBIVIT"
)

#ro_secbivit
ro_secbivit <- readxl::read_xlsx(here::here(spfolder, "Ro_SECBIVIT_SpeciesLIST.xlsx"), 
                         .name_repair="universal", col_names=FALSE)
df16 <- data.frame(
  "taxa"=ro_secbivit[,1],
  "original_ID"=NA,
  "database_ID"="RO_SECBIVIT"
)
# not sure why, but can't assign name in data.frame for the first column
names(df16)[1] <- "taxa"


## merge species list ------------------
fulldf <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,
                df10,df11,df12,df13,df14,df15,df16)

saveRDS(fulldf, here::here("data", "derived-data", "species_list_raw.rds"))


