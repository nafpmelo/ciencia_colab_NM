## Atividade 3 - 
# Natalia Melo - 11/05/2022

library(tidyverse)
library(rgbif)
library(dplyr)
library(CoordinateCleaner)
library(bdc)
library(ggplot2)

### GBIF ###

# checar funcoes
?occ_data

# baixar ocorrencias
donz_gbif <- occ_data(scientificName = "stegastes variabilis", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(donz_gbif)

dim(donz_gbif$data)

# checar campos
donz_gbif$data %>% names


# Prosseguimos selecionando algumas vari?veis que ser?o ?teis para a valida??o dos dados e futuras an?lises, como coordenadas, profundidade, nome da base de dados etc.

donz_gbif1 <- donz_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 

# Note que temos 500 ocorr?ncias, no entanto, vamos ver quantas s?o ?nicas aplicando a fun??o distinct do pacote dplyr
donz_gbif1 <- donz_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(donz_gbif1, unique)

# checar coordenadas v?lidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = donz_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas v?lidas e pr?ximas a capitais (muitas vezes as coordenadas s?o erroneamente associadas a capitais dos pa?ses)

cl <- donz_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))

# verificar coordenadas com flags

# capitais (padr?o ? um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital")  


cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea")  

# investigar niveis suspeitos
donz_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()

# waterBody
donz_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 


# fonte das regioes erradas
#donz_gbif1 %>% 
#  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
#  distinct(datasetName)

# 0 ocorrencias
donz_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))

# filtrar todas do dataset suspeito
#dori_gbif_ok <- dori_gbif1 %>% 
#  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")

library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = donz_gbif1, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Stegastes variabilis")))

# checar profundidade
donz_gbif1 %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

donz_gbif_ok <- donz_gbif1

#####################################

### OBIS ###

install.packages("robis")
library(robis)

donz_obis <- robis::occurrence("Stegastes variabilis")

names(donz_obis)

donz_obis1 <- donz_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
donz_obis1 %>% 
  distinct(flags)


# check NA em datasetName
donz_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
donz_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName)) %>% 
         #!waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
donz_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName)) %>% 
         #!waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)

# ok
donz_obis_ok <- donz_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("NA"))


# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = donz_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Stegastes variabilis")))

#Unir gbif e obis

# ver diferencas
setdiff(names(donz_gbif_ok), names(donz_obis_ok))
setdiff(names(donz_obis_ok), names(donz_gbif_ok))

all_data <- bind_rows(donz_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      donz_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Stegastes variabilis") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Stegastes variabilis")))
