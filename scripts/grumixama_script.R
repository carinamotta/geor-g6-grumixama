#' ---
#' title: Lacunas dos dados de espécies através do espaço e do tempo  
#' authors: Luca Nunes, Otto Marques, Carina Motta
#' date: 2021-11-01
#' ---

package.list <- c("here", 
                  "tidyverse",
                  "sf",
                  "geobr")

#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}
 
#load data 
birds <- readr::read_csv(here::here("dados", "birds.csv"),
                      locale = readr::locale(encoding = "latin1"))

#visualize data
birds

#check class and define as tibble
class(birds)
as_tibble(birds)

#select columns to construct simplified dataset
colnames(birds)

#separando os dados
dados<-tidyr::separate_rows(data = birds,sep = ",")
dados_sep <- dados[,c("Genus","Species","Binomial",
                            "Latitude_decimal_degrees",
                            "Longitude_decimal_degrees",
                            "Locality","Year")]

#replace NAs with unknown (character columns) and 0 (numeric column, Year)
any(is.na(dados_sep))
dados_unknown <-tidyr::replace_na(data = dados_sep, list(Genus = "Unknown",
                                                    Species = "Unknown", 
                                                    Binomial = "Unknown", 
                                                    Nm_municip = "Unknown", 
                                                    Locality = "Unknown", 
                                                    Year = 0))

#drop rows with remaining NAs (only exist in Lat Long)
any(is.na(dados_unknown))
dados_NA <- tidyr::drop_na(dados_unknown)
any(is.na(dados_NA))

#coerce columns as.numeric
summary(dados_NA)
any(is.na(dados_NA))
as.numeric(dados_NA$Latitude_decimal_degrees, 
           dados_NA$Longitude_decimal_degrees,
           dados_NA$Year) 
summary(dados_NA)

#convert lat long to sf
dados_pontos <- dados_NA %>%
  sf :: st_as_sf(coords = c("Longitude_decimal_degrees",
                            "Latitude_decimal_degrees"), 
                            crs = 4326)
#import biomes
bi_2019 <- geobr::read_biomes(year = 2019, showProgress = FALSE)
bi_2019

#transform dados from WGS84 to SIRGAS2000
dados_sirgas2000 <- sf::st_transform(dados_pontos, crs = 4674)

#visualize data
plot(bi_2019$geom)
plot(dados_sirgas2000$geometry, add = TRUE)

#join dados
points_join_biom <- dados_sirgas2000 %>% 
  sf::st_join(x = ., y = bi_2019)
points_join_biom

#separate points within Mata Atlantica 
dados_final <- dplyr::filter(points_join_biom, name_biome == "Mata Atlântica")
as.numeric(dados_final$Year)
summary(dados_final)
plot(dados_final$geometry)

#save new dataset!
write.csv(dados_final, file = here::here("dados", "dados_final.csv"))

