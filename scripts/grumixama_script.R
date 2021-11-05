#' ---
#' title: Lacunas dos dados de espécies através do espaço e do tempo  
#' authors: Luca Nunes, Otto Marques, Carina Motta
#' date: 2021-11-01
#' ---

##1 CARREGAR PACOTES------------------------------------------------------------

#facilitar a carregar pacotes 
package.list <- c("here", 
                  "tidyverse",
                  "sf",
                  "geobr",
                  "raster",
                  "spatstat",
                  "ggplot2",
                  "ggspatial",
                  "psych")

#instalar pacotes novos (se ainda não estiver presente)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#carregar pacotes
for(i in package.list){library(i, character.only = T)}


#2 CARREGAR E LIMPAR DADOS------------------------------------------------------

#carregar dados 
birds <- readr::read_csv(here::here("dados", "birds.csv"),
                         locale = readr::locale(encoding = "latin1"), 
                         na = c("Unknown", "NA"))

#excluir linhas sem dados sobre localização
birds <- tidyr::drop_na(data = birds,
                        any_of("Latitude_decimal_degrees"))

#confirmar se foi excluido 
any(is.na(birds$Latitude_decimal_degrees))
any(is.na(birds$Longitude_decimal_degrees))

## 3 CALCULAR NUMERO DOS DADOS DE TRAITS CADA INDIVIDUAL----
#criar matrix binario (tem dados = 1, sem dados = 0) e somar 
#cada linha (individual)

#separar dados imporantes em um arquivo novo
#usamos traits numéricos que foram usado para criar a planilha 
#final (ATLANTIC_BIRD_TRAITS_Spp_Info.csv)
traits <- birds[,c(1, 10:32)]

#substituir dados existentes com 1
traits <- traits %>% 
  mutate_if(is.numeric, ~1 * (. != 0))

#substituir dados NA com 0
traits[is.na(traits)] = 0

#somar numero de traits
traits[,25] <- rowSums(traits[, -1])

#separar a soma em novo arquivo
traits <- traits[,c(1, 25)]

## 4 ADICIONAR NUMERO DE TRAITS EM UMA COLUNA NOVA COM DADOS--------------------

#separar dados que nos vamos usar em um arquivo novo 
bird_traits <- birds[,c(1, 5, 6, 9, 59, 60, 76)]

#combinar dados que nos vamos usar com o numero de traits cada linha
bird_traits <- dplyr::left_join(bird_traits, traits, by = "ID_ABT")

#mudar o nome da coluna para "traits"
bird_traits <- bird_traits %>% 
  dplyr::rename( Traits = ...25) 
bird_traits

##4 FORMATAR DADOS GEOESPACIAS

#transformar nosso arquivo para sf
dados_pontos <- bird_traits %>%
  sf :: st_as_sf(coords = c("Longitude_decimal_degrees",
                            "Latitude_decimal_degrees"), 
                 crs = 4326)
dados_pontos

#importar biomas
bi_2019 <- geobr::read_biomes(year = 2019, showProgress = FALSE)
bi_2019

#importar Brasil
br <- geobr::read_country(2019)
br

#transformar dados de WGS84 para SIRGAS2000
dados_sirgas2000 <- sf::st_transform(dados_pontos, crs = 4674)


#juntar dados para ver quais pontos são foras da Mata Atlântica 
points_join_biom <- dados_sirgas2000 %>% 
  sf::st_join(x = ., y = bi_2019)
points_join_biom

#filtrar os pontos no dentro da Mata Atlântica 
dados_final <- dplyr::filter(points_join_biom, 
                             name_biome == "Mata Atlântica")

#removar colunas de code_biome e year 
dados_final <- dados_final %>%
  dplyr::select(-(code_biome:year))

#criar planilha com só o poligano de Mata Atlântica
bi_2019_MA <- dplyr::filter(bi_2019, name_biome == "Mata Atlântica")
bi_2019_MA

#Exportar shp para ArcGIS para corretar CRS
#sf::st_write(obj = dados_final, dsn = here::here("dados", "dados_final.shp"))
#sf::st_write(obj = bi_2019_MA, dsn = here::here("dados", "bi_2019_MA.shp"))

#colocamos os shp finais nos "dados"

#Importar dados de ArcGIS

#nossos dados
dados_final_Albers_Sir2000 <- sf::st_read(here::here("dados",
                                                     "dados_Albers_Sir2000", 
                                                     "dados_final_Albers_Sir2000.shp"), 
                                          quiet = TRUE)
dados_final_Albers_Sir2000

#polígono da Mata Atlântica
bi_2019_MA_Albers_Sir2000 <- sf::st_read(here::here("dados",
                                                    "MA_Albers_Sir2000", 
                                                    "bi_2019_MA_Albers_Sir2000.shp"), 
                                         quiet = TRUE)
bi_2019_MA_Albers_Sir2000

#polígono do Brasil
br_Albers_Sir2000 <- sf::st_read(here::here("dados",
                                            "BR_Albers_Sir2000", 
                                            "lm_uf_albers_Sir2000.shp"), 
                                 quiet = TRUE)
br_Albers_Sir2000


#criar grid 
dados_grid <- sf::st_make_grid(x = dados_final_Albers_Sir2000,
                               cellsize = 100000, what = "polygons") %>%
  sf::st_as_sf() %>%
  dplyr::filter(sf::st_intersects(x = ., y = bi_2019_MA_Albers_Sir2000, 
                                  sparse = FALSE))

#plot com grid
#plot(dados_final_Albers_Sir2000$geometry, col = "gray", main = NA,
     #axes = TRUE, graticule = TRUE)
#plot(dados_grid, col = NA, border = "red", lwd = 2, add = TRUE)


#atribuir um id para cada quadrícula
grid_id <- tibble::rowid_to_column(dados_grid, "id")

#juntar informaçâo de grid com nossos dados 
points_join_grid <- dados_final_Albers_Sir2000 %>% 
  sf::st_join(x = ., y = grid_id)
points_join_grid



##5 CALCULAR QUANTOS AMOSTRAS POR QUADRÍCULA------------------------------------ 

#renomear arquivo de points_join_grid para manipular ele
samples <- points_join_grid

#redefinir como um tibble
samples <- as.tibble(samples)

#separar colunas de ID_ABT e id (id de quadrícula)
samples <- samples[,c(1, 8)]

#contar numero de amostras por quadrícula
samples <- samples %>% group_by(id) %>% tally()

#juntar com grid_id
grid_samples <- dplyr::left_join(grid_id, samples, by = "id")

#remover as NAs
grid_samples <-tidyr::replace_na(data = grid_samples, list(n = 0))


##6 CALCULAR QUANTOS TRAITS POR QUADRÍCULA--------------------------------------

#renomear arquivo de points_join_grid para manipular ele
traits <- points_join_grid

#redefinir como um tibble
traits <- as.tibble(traits)

#separar colunas de traits e id (id de quadrícula)
traits <- traits[,c(6, 8)]

#contar numero total de traits por quadrícula
traits <- traits %>%
  group_by(id) %>%
  summarise(
    traits_s = sum(traits)
  )

#juntar com grid_samples
samples_traits <- dplyr::left_join(grid_samples, traits, by = "id")

#remover as NAs
samples_traits <-tidyr::replace_na(data = samples_traits, list(traits_s = 0))

#renomear colunas de n e traits_s
samples_traits <- samples_traits %>% 
  dplyr::rename(Amostras = n ,  Traits = traits_s) 


##7 DEFINIR O ANO MAIS RECENTE QUE A QUADRÍCULA FOI AMOSTRADO-------------------  

#renomear arquivo de points_join_grid para manipular ele
year <- points_join_grid

#redefinir como um tibble
year <- as.tibble(year)

#separar colunas de year e id (id de quadrícula)
year <- year[,c(1, 5, 8)]

#colocar NA em vez de 0 
year[year == 0] <- NA

#remover NAs
year <- tidyr::drop_na(data = year,
                       any_of("Year"))

#identificar ano mais recente (o mais antigo, colocar "min" em vez
#de max) 
year <- year %>%
  group_by(id) %>%
  summarise(
    year = min(Year)
  )

samples_traits_year <- dplyr::left_join(samples_traits, year, by = "id")

#renomear a coluna de year
samples_traits_year <- samples_traits_year %>% 
  dplyr::rename(Ano = year) 

#produto final
samples_traits_year

##8 CRIAR OS MAPAS DE GRID------------------------------------------------------

#mapa de amostras
mapa_amostras <- ggplot() +
  geom_sf(data = br_Albers_Sir2000, fill = "gray")+
  geom_sf(data = bi_2019_MA_Albers_Sir2000)+
  geom_sf(data = dados_final_Albers_Sir2000$geometry,)+
  geom_sf(data = samples_traits_year, aes(fill = Amostras), color = gray(.5))+
  scale_fill_viridis_c(trans = "sqrt", alpha = .8) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude")
mapa_amostras

#mapa de traits
mapa_traits <- ggplot() +
  geom_sf(data = br_Albers_Sir2000, fill = "gray")+
  geom_sf(data = bi_2019_MA_Albers_Sir2000)+
  geom_sf(data = dados_final_Albers_Sir2000$geometry,)+
  geom_sf(data = samples_traits_year, aes(fill = Traits), color = gray(.5))+
  scale_fill_viridis_c(trans = "sqrt", alpha = .8) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude")
mapa_traits

#mapa de ano
mapa_ano <- ggplot() +
  geom_sf(data = br_Albers_Sir2000, fill = "gray")+
  geom_sf(data = bi_2019_MA_Albers_Sir2000)+
  geom_sf(data = dados_final_Albers_Sir2000$geometry,)+
  geom_sf(data = samples_traits_year, aes(fill = Ano), color = gray(.5))+
  scale_fill_viridis_c(trans = "sqrt", alpha = .8) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude")
mapa_ano

##9 CRIAR O MAPA DE KERNEL------------------------------------------------------

# Cria uma janela raster
bi_2019_MA_Albers_Sir2000_Owin <- as.owin(bi_2019_MA_Albers_Sir2000)

# Converte os pontos em ppp (raster spatstat)
dados_Albers_ppp <- as.ppp(dados_final_Albers_Sir2000)

#Limita por janela
Window(dados_Albers_ppp) <- bi_2019_MA_Albers_Sir2000_Owin

# Conversão de medida
dados_Albers_ppp_km <- rescale(dados_Albers_ppp, 1000, "km")

#Contagem de Ponto por Quadrícula
Q <- spatstat.geom::quadratcount(dados_Albers_ppp_km, nx=8, ny=8)
plot(Q)

#Intensidade por quadrícula
Q.d <- intensity(Q)
plot(intensity(Q, image = TRUE))

#kernell
kernel <- density(dados_Albers_ppp_km)

#plot
plot(kernel)

#tem que exportar a imagem do kernel no Plot window


##10 ANÁLISES ESTATÍSTICA-------------------------------------------------------

#carregar dados
dados_est <- readr::read_csv(here::here("dados", "dados_est.csv"),
                         locale = readr::locale(encoding = "latin1"))

#histograma
traits_hist <- ggplot(data = dados_est,
      aes(x = Nu_Amostras, fill = Traits)) +
      geom_histogram(bins = 5, alpha = 20)+
      labs(x="Número de Amostras",y= "frequência", title = "NÚMERO DE TRAITS")+
      theme_bw(base_size = 10)+
      geom_density()+
      theme(plot.title = element_text(hjust = 0.5))
traits_hist


#Probabilidade que um trait vai ser medida 

# virando linha em coluna
gather<-gather(dados_est)

#criando colunas
dados_est["total"]<-c("72484") 

#definir coluna de total como integer
dados_est$total <- as.integer(dados_est$total)

#cacular a probabilidade 
dados_est["prob_trait"] <- (dados_est["Nu_Amostras"]/dados_est["total"])*100
dados_est


##11 EXPORTAR OS MAPAS----------------------------------------------------------

ggsave(
  filename = here::here("resultados", "mapa_amostras.png"),
  plot = mapa_amostras, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 720
)

ggsave(
  filename = here::here("resultados", "mapa_traits.png"),
  plot = mapa_traits, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 720
)

ggsave(
  filename = here::here("resultados", "mapa_ano_min.png"),
  plot = mapa_ano, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 720
)

ggsave(
  filename = here::here("resultados", "traits_hist.png"),
  plot = traits_hist, 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 720
)




