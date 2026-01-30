
devtools::load_all() # or source(here::here("R", "clean_taxo.R"))
library("readxl")
library("openxlsx")
getwd()
meta <- read_excel("data/raw-data/traits/Metatraits.xlsx", sheet = "Database")
sheet1 <- read_excel("data/raw-data/traits/Metatraits.xlsx", sheet = "Traits")

meta2 <-meta %>% 
  mutate(Geo_range = ifelse(Database == "At_Vinedivers", "Austria", NA )) %>%
  mutate(Geo_range = ifelse(Database == "Baseflor", "France", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "BIEN", "World", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Biolflor", "Germany", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "CLO-PLA", "Europe", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Dcube", "Europe", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Ecoflora", "UK", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "FlorealData", "France", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "FR_GRAS_FLOR", "France", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "GIFT", "World", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Hodgson_2023", "Eurasia&NorthAfrica", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Lososova_2023", "Europe", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "SPVignes", "France", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Tichy_2022", "Europe", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Yvoz_ValPol", "France", Geo_range )) %>%
  mutate(Geo_range = ifelse(Database == "Ladouceur_2019", "Europe", Geo_range ))%>%
  mutate(Geo_range = ifelse(Database == "Groot", "World", Geo_range ))

write.xlsx(
  list(
    Sheet1 = sheet1,
    Sheet2 = meta2
  ),
  file = "data/raw-data/traits/Metatraits_geo_range.xlsx"
)
