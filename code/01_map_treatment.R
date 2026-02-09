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
  mutate(
    afetados = as.integer(name_muni %in% c(
      "Areal","Bom Jardim","Nova Friburgo","São José Do Vale Do Rio Preto",
      "Sumidouro","Petrópolis","Teresópolis","Santa Maria Madalena",
      "Sapucaia","Paraíba Do Sul","São Sebastião Do Alto","Três Rios",
      "Cordeiro","Carmo","Macuco","Cantagalo"
    )),
    calamidade = as.integer(name_muni %in% c(
      "Areal","Bom Jardim","Nova Friburgo","São José Do Vale Do Rio Preto",
      "Sumidouro","Petrópolis","Teresópolis"
    )),
    maiores_afetados = as.integer(name_muni %in% c(
      "Nova Friburgo","Petrópolis","Teresópolis"
    )),
    arredores = as.integer(name_muni %in% c(
      "Areal","Paraíba Do Sul","Nova Friburgo","Petrópolis","Teresópolis",
      "Bom Jardim","São José Do Vale Do Rio Preto","Sumidouro",
      "Cachoeiras De Macacu","Duas Barras","Sapucaia","Três Rios",
      "Magé","Guapimirim","Silva Jardim","Duque De Caxias","Cordeiro"
    )),
    Afetados = afetados + calamidade + maiores_afetados,
    Afetados = ifelse(Afetados == 3, "Maiores Afetados",
               Zifelse(Afetados == 2, "Afetados e calamidade",
               ifelse(Afetados == 1, "Afetados", "Não Afetado")))
  ) %>% 
  st_as_sf()



arredores <- subset(municipio,arredores==1) %>% sf::st_transform(32723)

### Leitura dos dados da rais -----

# Definição da abreviação
informacao <- municipio %>% filter(arredores==1)


## Escolas com o CEP
geocode <- arrow::read_parquet("output/painel_escolas.parquet") %>% 
  filter(!is.na(lon)) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>% 
  select(c(pk_cod_entidade = code_inep,raio, min_dist, geometry))

geocode <- geocode %>% distinct(pk_cod_entidade, .keep_all = T)

## Definindo pontos que houve pico de deslizamento
petropolis <- sf::st_read(dsn="G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/cicatrizes/Cicatriz_Pet_2011_UTM.shp") %>% 
  mutate(name_muni = "Petropolis") %>% select(c(name_muni,geometry))
st_crs(petropolis)

teresopolis <- sf::st_read(dsn="G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/cicatrizes/Cicatriz_Ter_2011_UTM.shp") %>% 
  mutate(name_muni = "Petropolis") %>% select(c(name_muni,geometry))
teresopolis <-  st_transform(teresopolis, crs = st_crs(petropolis)) 
st_crs(teresopolis)

nova_friburgo <- sf::st_read(dsn="G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/cicatrizes/Cicatriz_Nov_2011_UTM.shp") %>% 
  mutate(name_muni = "Petropolis") %>% select(c(name_muni,geometry))

nova_friburgo <-   st_transform(nova_friburgo, crs = st_crs(petropolis))
st_crs(nova_friburgo)

pontos_desastres <- rbind(petropolis,teresopolis,nova_friburgo)

# fix topology
pontos_desastres <- sf::st_make_valid(pontos_desastres)

pontos_desastres <- pontos_desastres %>% sf::st_transform(32723)

## Raio otimo
raio_otimo <- sf::st_read(dsn="G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/runnout/Deslizamentos_Runnout.shp")
temp <- fread("G:/.shortcut-targets-by-id/1K-TPEsFyx_miIiVaxXX3sKwYBAxHWeUj/Natural Disasters and Educational outcomes  Evidence from the 2011 Rio de Janeiro Landslides/input/runnout/Runnout.csv") %>% 
  select(c(Runnout3)) %>% 
  mutate(controle_1000_runnout3 = Runnout3+1000,
         controle_2000_runnout3 = Runnout3+2000,
         controle_2500_runnout3 = Runnout3+2500,
         controle_3000_runnout3 = Runnout3+3000)

raio_otimo <- cbind(raio_otimo,temp)

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


ggsave('./results/histograma.jpg', width = 15, height = 12, units = 'cm',
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

ggsave('./results/affected_region.jpg', width = 20, height = 12, units = 'cm', dpi=300)

geocode <- geocode %>%
  mutate(variable = case_when(
    raio == 1 ~ "Treated\n(Within Coverage)",
    between(min_dist, 20, 30) ~ "Control\n(20–30 km)",
    between(min_dist, 0, 20) ~ "Non-Treated",
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
k <- 1.2
bbox_zoom_expanded <- bbox_zoom
bbox_zoom_expanded["xmin"] <- bbox_zoom["xmin"] - (1.5 * k) * x_range
bbox_zoom_expanded["xmax"] <- bbox_zoom["xmax"] + (1.8 * k) * x_range
bbox_zoom_expanded["ymin"] <- bbox_zoom["ymin"] - (0.6 * k) * y_range
bbox_zoom_expanded["ymax"] <- bbox_zoom["ymax"] + (0.6 * k) * y_range


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
  ggnewscale::new_scale_fill() +
  
  geom_sf(data = buffer_raio_otimo,
          fill = '#B2182B',
          alpha = 0.2,
          color = NA) +
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

ggsave('./results/pontos deslizamentos.jpg', width = 15, height = 12, units = 'cm',
       dpi=300)
