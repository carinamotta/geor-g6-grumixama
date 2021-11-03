#' ---
#' title: Lacunas dos dados de espécies através do espaço e do tempo  
#' authors: Luca Nunes, Otto Marques, Carina Motta
#' date: 2021-11-01
#' ---

package.list <- c("here", 
                  "tidyverse",
                  "sf",
                  "geobr",
                  "raster",
                  "spatstat")

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

--------------------------------------------------------------------------------
#' date: 2021-11-03

#clean up dataset
#points_join_grid <- points_join_grid %>% 
#dplyr::select(-(code_biome:year))
#head(points_join_grid)

#criar planilha com só o poligano de Mata Atlântica
bi_2019_MA <- dplyr::filter(bi_2019, name_biome == "Mata Atlântica")

#projetar dados e Mata Atlântica
dados_projetados <- sf::st_transform(dados_final, 
                                     crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

MA_projetada <- sf::st_transform(bi_2019_MA, 
                                 crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


#criar grid
dados_grid <- sf::st_make_grid(x = dados_projetados, cellsize = 100000, what = "polygons") %>%
  sf::st_as_sf() %>%
  dplyr::filter(sf::st_intersects(x = ., y = MA_projetada, sparse = FALSE))

#plot com grid
plot(dados_projetados$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(dados_grid, col = NA, border = "red", lwd = 2, add = TRUE)

#assign id's to grid dataset so we can join with points
tibble::rowid_to_column(dados_grid, "id")

#add grid dataset information to the points dataset
points_join_grid <- dados_projetados %>% 
  sf::st_join(x = ., y = grid_id)
points_join_grid

#next steps: determine how many points are in each square and create a 
#spreadsheet with this information that we can then join back to the grid
#dataset and plot point density 

#maybe make a binary matrix??

-------
#Alternative method to create a grid (see link below for more information)
#We were not able to extract which points corresponded to which quadrats, 
#created a file type that was hard to work with (quadracount)


#https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html

#dados_final_ppp <- as.ppp(dados_projetados)

#bi_2019_MA_owin <- spatstat.geom::as.owin(MA_projetada)

#dados_final_ppp
#marks(dados_final_ppp)
#dados_final_ppp <- rescale(dados_final_ppp, 1000)
#spatstat.geom::Window(dados_final_ppp) <- bi_2019_MA_owin

#plot(dados_final_ppp, main=NULL, cols=rgb(0,0,0,.2), pch=20)

#Q <- spatstat.geom::quadratcount(dados_final_ppp, nx= 20, ny=20)


#plot(dados_final_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
#plot(Q, add=TRUE) 

#?spatstat.geom

# Compute the density for each quadrat
#Q.d <- intensity(Q)
#print(Q)
#summary(Q)
