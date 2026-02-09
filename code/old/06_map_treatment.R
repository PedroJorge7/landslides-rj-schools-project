library(dplyr)
library(geobr)
library(ggplot2)
library(sf)
library(data.table)
library(ggspatial)

estados <- read_state(code_state = 'all')

municipio <- read_municipality(code_muni = 'all', year = 2017) %>% 
  mutate(code_muni = as.numeric(substr(code_muni,1,6))) %>% 
  filter(abbrev_state == "RJ") %>% 
  mutate(afetados = ifelse(
    name_muni == "Areal"                 | name_muni == "Bom Jardim"                    |
      name_muni == "Nova Friburgo"         | name_muni == "São José Do Vale Do Rio Preto" |
      name_muni == "Sumidouro"             | name_muni == "Petrópolis"                    |
      name_muni == "Teresópolis"           | name_muni == "Santa Maria Madalena"          |
      name_muni == "Sapucaia"              | name_muni == "Paraíba Do Sul"                |
      name_muni == "São Sebastião Do Alto" | name_muni == "Três Rios"                     |
      name_muni == "Cordeiro"              | name_muni == "Carmo"                         |
      name_muni == "Macuco"                | name_muni == "Cantagalo",
    1,0),
    calamidade = ifelse(
      name_muni == "Areal"                 | name_muni == "Bom Jardim"                    |
        name_muni == "Nova Friburgo"         | name_muni == "São José Do Vale Do Rio Preto" |
        name_muni == "Sumidouro"             | name_muni == "Petrópolis"                    |
        name_muni == "Teresópolis",1,0),
    maiores_afetados = ifelse(
      name_muni == "Nova Friburgo"         | name_muni == "Petrópolis"      |
        name_muni == "Teresópolis",1,0),
    arredores = ifelse(name_muni == "Areal"          | name_muni == "Paraíba Do Sul" |
                         name_muni == "Nova Friburgo"                   | name_muni == "Petrópolis"     |
                         name_muni == "Teresópolis"                   | name_muni == "Bom Jardim"     |
                         name_muni == "São José Do Vale Do Rio Preto" |
                         name_muni == "Sumidouro"                     | name_muni == "Cachoeiras De Macacu" |
                         name_muni == "Duas Barras"                   | name_muni == "Sapucaia" |
                         name_muni == "Três Rios"                     | name_muni ==  "Magé" |
                         name_muni == "Guapimirim"                    | name_muni ==  "Silva Jardim" |
                         name_muni == "Duque De Caxias"               | name_muni ==  "Cordeiro",1,0),
    Afetados = afetados + calamidade + maiores_afetados,
    Afetados = 
      ifelse(Afetados==3,"Maiores Afetados",
             ifelse(Afetados==2,"Afetados e calamidade",
                    ifelse(Afetados==1,"Afetados","Não Afetado")))) %>% st_as_sf()


arredores <- subset(municipio,arredores==1) %>% sf::st_transform(32723)

### Leitura dos dados da rais -----

# Definição da abreviação
informacao <- municipio %>% filter(arredores==1)


## Escolas com o CEP
geocode <- bind_rows(lapply(list.files(path = './Desastre e escolas - Pré code/geocode_censo_escolar',
                                       full.names = T,
                                       pattern = '*.parquet'),
                            arrow::read_parquet))
geocode <- subset(geocode, substr(CO_ENTIDADE,1,2) == '33')
geocode <- subset(geocode,Addr_type %in% c('PointAddress','Postal',
                                           'PostalExt','PostalLoc',
                                           'StreetAddress','StreetAddressExt',
                                           'StreetName'))
geocode <- geocode %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) 
geocode <- geocode %>% 
  select(c(pk_cod_entidade = CO_ENTIDADE,geometry))

geocode <- geocode %>% distinct(pk_cod_entidade, .keep_all = T)

## Definindo pontos que houve pico de deslizamento
petropolis <- sf::st_read(dsn="./base/cicatrizes/Cicatriz_Pet_2011_UTM.shp") %>% 
  mutate(name_muni = "Petropolis") %>% select(c(name_muni,geometry))
st_crs(petropolis)

teresopolis <- sf::st_read(dsn="./base/cicatrizes/Cicatriz_Ter_2011_UTM.shp") %>% 
  mutate(name_muni = "Petropolis") %>% select(c(name_muni,geometry))
teresopolis <-  st_transform(teresopolis, crs = st_crs(petropolis)) 
st_crs(teresopolis)

nova_friburgo <- sf::st_read(dsn="./base/cicatrizes/Cicatriz_Nov_2011_UTM.shp") %>% 
  mutate(name_muni = "Petropolis") %>% select(c(name_muni,geometry))

nova_friburgo <-   st_transform(nova_friburgo, crs = st_crs(petropolis))
st_crs(nova_friburgo)

pontos_desastres <- rbind(petropolis,teresopolis,nova_friburgo)

# fix topology
pontos_desastres <- sf::st_make_valid(pontos_desastres)

pontos_desastres <- pontos_desastres %>% sf::st_transform(32723)

## Raio otimo
raio_otimo <- sf::st_read(dsn="./base/runnout/Deslizamentos_Runnout.shp")
temp <- fread("./base/runnout/Runnout.csv") %>% 
  select(c(Runnout3)) %>% 
  mutate(controle_1000_runnout3 = Runnout3+1000,
         controle_2000_runnout3 = Runnout3+2000,
         controle_2500_runnout3 = Runnout3+2500,
         controle_3000_runnout3 = Runnout3+3000)

raio_otimo <- cbind(raio_otimo,temp)

raio_otimo <- raio_otimo %>% sf::st_transform(32723)

mean(raio_otimo$Runnout3) - 2*sd(raio_otimo$Runnout3)
mean(raio_otimo$Runnout3)
mean(raio_otimo$Runnout3) + 2*sd(raio_otimo$Runnout3)

geocode <- sf::st_transform(geocode, sf::st_crs(raio_otimo))
# Interseção das escolas dentro do raio otimo
raio <- sf::st_buffer(raio_otimo$geometry, units::set_units(raio_otimo$Runnout3/1000, km))
aux <- sf::st_intersects(geocode$geometry,raio)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio <- aux1

# Interseção das escolas dentro do raio otimo
raio_controle <- sf::st_buffer(raio_otimo$geometry, units::set_units(raio_otimo$controle_1000_runnout3/1000, km))
aux <- sf::st_intersects(geocode$geometry,raio_controle)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_controle_1000 <- aux1

raio_controle <- sf::st_buffer(raio_otimo$geometry, units::set_units(raio_otimo$controle_2000_runnout3/1000, km))
aux <- sf::st_intersects(geocode$geometry,raio_controle)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_controle_2000 <- aux1

raio_controle <- sf::st_buffer(raio_otimo$geometry, units::set_units(raio_otimo$controle_2500_runnout3/1000, km))
aux <- sf::st_intersects(geocode$geometry,raio_controle)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_controle_2500 <- aux1


raio_controle <- sf::st_buffer(raio_otimo$geometry, units::set_units(raio_otimo$controle_3000_runnout3/1000, km))
aux <- sf::st_intersects(geocode$geometry,raio_controle)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_controle_3000 <- aux1


# Interseção das escolas dentro do raio tratado 2.5km
raio_2.5km <- sf::st_buffer(pontos_desastres$geometry, units::set_units(2.5, km))
aux <- sf::st_intersects(geocode$geometry,raio_2.5km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_2.5km <- aux1


# Interseção das escolas dentro do raio tratado 5km
raio_5km <- sf::st_buffer(pontos_desastres$geometry, units::set_units(5, km))
aux <- sf::st_intersects(geocode$geometry,raio_5km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_5km <- aux1

# Interseção das escolas dentro do raio tratado 7.5km
raio_7.5km <- sf::st_buffer(pontos_desastres$geometry, units::set_units(7.5, km))
aux <- sf::st_intersects(geocode$geometry,raio_7.5km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_7.5km <- aux1

# Interseção das escolas dentro do raio tratado 10km
raio_10km <- sf::st_buffer(pontos_desastres$geometry, units::set_units(10, km))
aux <- sf::st_intersects(geocode$geometry,raio_10km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_10km <- aux1

# Interseção das escolas dentro do raio tratado 12.5km
raio_12.5km <- sf::st_buffer(pontos_desastres$geometry, units::set_units(12.5, km))
aux <- sf::st_intersects(geocode$geometry,raio_12.5km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_12.5km <- aux1

# Interseção das escolas dentro do raio tratado 15km
raio_15km <- sf::st_buffer(pontos_desastres$geometry, units::set_units(15, km))
aux <- sf::st_intersects(geocode$geometry,raio_15km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_15km <- aux1


# Interseção das escolas dentro do raio controle de 20km
raio_20km <-  sf::st_buffer(pontos_desastres, units::set_units(20, km))
aux <- sf::st_intersects(geocode$geometry,raio_20km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_20km <- aux1

# Interseção das escolas dentro do raio controle de 22.5km
raio_22.5km <-  sf::st_buffer(pontos_desastres, units::set_units(22.5, km))
aux <- sf::st_intersects(geocode$geometry,raio_22.5km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_22.5km <- aux1

# Interseção das escolas dentro do raio controle de 25km
raio_25km <-  sf::st_buffer(pontos_desastres, units::set_units(25, km))
aux <- sf::st_intersects(geocode$geometry,raio_25km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_25km <- aux1

# Interseção das escolas dentro do raio controle de 27.5km
raio_27.5km <-  sf::st_buffer(pontos_desastres, units::set_units(27.5, km))
aux <- sf::st_intersects(geocode$geometry,raio_27.5km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_27.5km <- aux1

# Interseção das escolas dentro do raio controle de 30km
raio_30km <-  sf::st_buffer(pontos_desastres, units::set_units(30, km))
aux <- sf::st_intersects(geocode$geometry,raio_30km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_30km <- aux1

raio_70km <-  sf::st_buffer(pontos_desastres, units::set_units(70, km))
aux <- sf::st_intersects(geocode$geometry,raio_70km)
aux1 <- sapply(1:length(aux), function(i){length(aux[[i]])})
aux1[aux1 > 0] <- 1
geocode$raio_70km <- aux1

#geocode <- subset(geocode, raio_70km == 1)

## Distancia mínima
geocode$min_dist <- apply(
  st_distance(geocode$geometry,pontos_desastres$geometry), 1, min)

#summary(outras_escolas$min_dist)


## Histograma -------------------

options(scipen = 999)


#density <- 
ggplot(data = raio_otimo, aes(x = Runnout3 / 1000)) +
  geom_histogram(fill = "steelblue", color = "white", alpha = 0.7, bins = 30) +
  geom_vline(aes(xintercept = mean(Runnout3 / 1000)), linetype = "dashed", color = "darkred", size = 0.8) +
  annotate("text", x = mean(raio_otimo$Runnout3)/1000 + 2, y = Inf, vjust = 1.5,
           label = paste0("Média = ", round(mean(raio_otimo$Runnout3)/1000, 2), " km\n",
                          "DP = ", round(sd(raio_otimo$Runnout3)/1000, 2), " km"),
           hjust = 0, size = 4.2) +
  labs(x = "Run-out distance (km)", y = "Frequência") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "gray85", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggsave('./output/histograma.png', width = 15, height = 12, units = 'cm',
       dpi=300)

## Map

micro <- read_micro_region(code_micro='RJ') %>%
  filter(name_micro == "Serrana" | name_micro == "Nova Friburgo")

#temp <- subset(municipio, calamidade == 1)

#raio_mapa <- raio %>% st_union()

gg1 = ggplot() +
  geom_sf(data = geobr::read_country(), fill = "white") +
  geom_sf(data = subset(estados,code_state==33), fill = "red", color = "red", size = 1.2) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank())


temporary <- municipio %>% filter(Afetados %in% c("Maiores Afetados",
                                                  "Afetados e calamidade")) %>% 
  st_transform(crs = st_crs(4326))

temporary2 <- geobr::read_municipality(code_muni = 'RJ') %>% 
  filter(as.numeric(substr(code_muni,1,6)) %in% temporary$code_muni)

temporary_name <- cbind(temporary2,
                        st_coordinates(st_centroid(temporary2$geom)))

temporary_name <- temporary_name %>% 
  mutate(name_muni = ifelse(name_muni == "São José Do Vale Do Rio Preto",
                            'SJVRP',name_muni),
         Y = ifelse(name_muni == "Nova Friburgo",Y-0.05,Y),
         X = ifelse(name_muni == "Nova Friburgo",X+0.05,X))

rj <- geobr::read_municipality(code_muni = 'RJ')
# m <- readxl::read_excel("./results/10_09/municipios.xls")


# teste <- subset(municipio,code_muni %in% (geocode %>% filter(min_dist <= 30000))$code_muni) %>% 
#   mutate(Situacao = ifelse(as.numeric(substr(code_muni,1,6)) %in% temporary$code_muni,"Directly Affected",
#                            "Indirectly Affected"))

levels = c(
  "Não Afetado" = "Sem Impacto",
  "Afetados" = "Impacto Moderado",
  "Afetados e calamidade" = "Impacto Severo (Calamidade)",
  "Maiores Afetados" = "Impacto Crítico"
)

# Atualizando as categorias com nomes mais claros
municipio$Afetados <- recode_factor(
  municipio$Afetados,
  "Não Afetado" = "Sem Impacto",
  "Afetados" = "Impacto Moderado",
  "Afetados e calamidade" = "Impacto Severo (Calamidade)",
  "Maiores Afetados" = "Impacto Crítico"
)

# Mapa com nova legenda e cores coerentes com intensidade
gg2 <- ggplot() + 
  geom_sf(data = rj, fill = "white", color = "black", size = 0.3) +
  # geom_sf(data = municipio, aes(fill = Afetados), color = "grey40", size = 0.2) +
  # scale_fill_manual(
  #   values = c(
  #     "Sem Impacto" = "white",
  #     "Impacto Moderado" = "#FED976",
  #     "Impacto Severo (Calamidade)" = "#FC4E2A",
  #     "Impacto Crítico" = "#BD0026"
  #   )
  # ) +
  labs(fill = "Nível de Impacto") +
  theme_void() +
  annotation_scale(location = "bl", width_hint = 0.25, pad_x = unit(3.3, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(4, "in"), pad_y = unit(3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )




gg_inset_map1 = cowplot::ggdraw() +
  cowplot::draw_plot(gg2) +
  cowplot::draw_plot(gg1, x = 0.001, y = 0.68,
                     width = 0.3, height = 0.3)

gg_inset_map1

ggsave('affected_region.png', width = 20, height = 12, units = 'cm', dpi=300)

geocode <- geocode %>%
  mutate(variable = case_when(
    raio == 1 ~ "Treated\n(Within Coverage)",
    between(min_dist / 1000, 20, 30) ~ "Control\n(20–30 km)",
    between(min_dist / 1000, 0, 20) ~ "Non-Treated",
    TRUE ~ "Outside Buffer"
  ))



# Filtrar apenas municípios com impacto severo/crítico para zoom e exibição
municipio_foco <- municipio %>%
  filter(name_muni == 'Teresópolis')


# Bounding box original
bbox_zoom <- sf::st_bbox(municipio_foco)

# Fator de expansão (ex: 10% a mais em cada direção)
expand_factor <- 1.5

# Calcular larguras e alturas originais
x_range <- bbox_zoom["xmax"] - bbox_zoom["xmin"]
y_range <- bbox_zoom["ymax"] - bbox_zoom["ymin"]

# Expandir limites
bbox_zoom_expanded <- bbox_zoom
bbox_zoom_expanded["xmin"] <- bbox_zoom["xmin"] - 1.5 * x_range
bbox_zoom_expanded["xmax"] <- bbox_zoom["xmax"] + 1.8 * x_range
bbox_zoom_expanded["ymin"] <- bbox_zoom["ymin"] - 0.6 * y_range
bbox_zoom_expanded["ymax"] <- bbox_zoom["ymax"] + 0.6 * y_range


# Buffer externo de 30 km
buffer_30km <- st_buffer(raio_otimo, dist = 30000)
buffer_30km <- buffer_30km %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_as_sf()

# Buffer interno de 20 km
buffer_20km <- st_buffer(raio_otimo, dist = 20000)
buffer_20km <- buffer_20km %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_as_sf()

# Anel entre 20 km e 30 km
anel_20_30km <- st_difference(buffer_30km, buffer_20km)


buffer_raio_otimo <- st_buffer(raio_otimo, dist = raio_otimo$Runnout3)
buffer_raio_otimo <- buffer_raio_otimo %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_as_sf()



p <- ggplot() +
  # Camada 1: Mapa com impacto (sem legenda)
  geom_sf(data = municipio, #aes(fill = Afetados),
          color = "grey40", size = 0.2,
          inherit.aes = FALSE,
          show.legend = FALSE) +
  # scale_fill_manual(
  #   values = c(
  #     "Sem Impacto" = "white",
  #     "Impacto Moderado" = "white",
  #     "Impacto Severo (Calamidade)" = "#FC4E2A",
  #     "Impacto Crítico" = "#BD0026"
  #   ),
  #   guide = "none"  # <--- remove a legenda
  # ) +
  ggnewscale::new_scale_fill() +
  
  # Camada 2: Raio de cobertura (com legenda)
  # geom_sf(data = raio,
  #         aes(fill = (raio_otimo %>% mutate(Runnout3 = Runnout3 / 1000))$Runnout3),
  #         color = NA) +
  geom_sf(data = buffer_raio_otimo,
          fill = '#B2182B',
          alpha = 0.2,
          color = NA) +
  # viridis::scale_fill_viridis(option = "inferno", direction = -1,
  #                             name = "Coverage Radius (KM)",
  #                             alpha = 0.2,
  #                             # here we use guide_colourbar because it is still a continuous scale
  #                             guide = guide_colorbar(
  #                               direction = "horizontal",
  #                               barheight = unit(2, units = "mm"),
  #                               barwidth = unit(40, units = "mm"),
  #                               draw.ulim = F,
  #                               title.position = 'top',
  #                               # some shifting around
  #                               title.hjust = 0.5,
  #                               order = 1
  #                               #label.hjust = 0.5
  #                             )) +
  
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = anel_20_30km,
          fill = '#2166AC',
          alpha = 0.2,
          color = NA) +
  ggnewscale::new_scale_fill() +
  
  # Camada 3: Pontos de tratamento (sem legenda)
  geom_sf(data = geocode,
          aes(fill = variable), size = 1.5, shape = 21) +
  scale_fill_manual(
    values = c(
      "Treated\n(Within Coverage)" = "#B2182B",
      "Control\n(20–30 km)" = "#2166AC",
      "Non-Treated" = "#999999",
      "Outside Buffer" = "white"
    ),
    breaks = c(
      "Treated\n(Within Coverage)",
      "Control\n(20–30 km)",
      "Non-Treated",
      "Outside Buffer"
    ),
    guide = guide_legend(
      title = "Treatment Classification",
      title.position = "top",
      title.hjust = 0.5
    )
  ) + 

  
  # Zoom na área de interesse
  coord_sf(
    xlim = c(bbox_zoom_expanded["xmin"], bbox_zoom_expanded["xmax"]),
    ylim = c(bbox_zoom_expanded["ymin"], bbox_zoom_expanded["ymax"]),
    expand = FALSE
  ) +
  
  # Tema final
  theme_void() +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(4.8, "in"), pad_y = unit(3.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.box = "vertical",
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.margin = margin()
  )


p

ggsave('pontos deslizamentos.png', width = 15, height = 12, units = 'cm',
       dpi=300)
