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
      "Areal","Bom Jardim","Nova Friburgo","SÃ£o JosÃ© Do Vale Do Rio Preto",
      "Sumidouro","PetrÃ³polis","TeresÃ³polis","Santa Maria Madalena",
      "Sapucaia","ParaÃ­ba Do Sul","SÃ£o SebastiÃ£o Do Alto","TrÃªs Rios",
      "Cordeiro","Carmo","Macuco","Cantagalo"
    )),
    calamidade = as.integer(name_muni %in% c(
      "Areal","Bom Jardim","Nova Friburgo","SÃ£o JosÃ© Do Vale Do Rio Preto",
      "Sumidouro","PetrÃ³polis","TeresÃ³polis"
    )),
    maiores_afetados = as.integer(name_muni %in% c(
      "Nova Friburgo","PetrÃ³polis","TeresÃ³polis"
    )),
    arredores = as.integer(name_muni %in% c(
      "Areal","ParaÃ­ba Do Sul","Nova Friburgo","PetrÃ³polis","TeresÃ³polis",
      "Bom Jardim","SÃ£o JosÃ© Do Vale Do Rio Preto","Sumidouro",
      "Cachoeiras De Macacu","Duas Barras","Sapucaia","TrÃªs Rios",
      "MagÃ©","Guapimirim","Silva Jardim","Duque De Caxias","Cordeiro"
    )),
    Afetados = afetados + calamidade + maiores_afetados,
    Afetados = ifelse(Afetados == 3, "Maiores Afetados",
               ifelse(Afetados == 2, "Afetados e calamidade",
               ifelse(Afetados == 1, "Afetados", "NÃ£o Afetado")))
  ) %>% 
  st_as_sf()



arredores <- subset(municipio,arredores==1) %>% sf::st_transform(32723)

### Leitura dos dados da rais -----

# DefiniÃ§Ã£o da abreviaÃ§Ã£o
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
           label = paste0("Mean = ", round(mean(raio_otimo$Runnout3)/1000, 2), " km\n",
                          "SD = ", round(sd(raio_otimo$Runnout3)/1000, 2), " km"),
           hjust = 0, size = 4.2) +
  labs(x = "Run-out distance (km)", y = "Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "gray85", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggsave('./results/histograma.jpg', width = 15, height = 10, units = 'cm',
       dpi=300)

## Map

micro <- read_micro_region(code_micro='RJ') %>%
  filter(name_micro == "Serrana" | name_micro == "Nova Friburgo")

#temp <- subset(municipio, calamidade == 1)

#raio_mapa <- raio %>% st_union()

rj <- geobr::read_municipality(code_muni = 'RJ')

displaced_share <- tibble::tribble(
  ~code_muni, ~displaced_share,
  330580, 0.41,
  330340, 0.23,
  330390, 0.17,
  330022, 0.09,
  330515, 0.05,
  330050, 0.04,
  330570, 0.01
)

municipio <- municipio %>%
  left_join(displaced_share, by = 'code_muni')

share_map <- municipio %>%
  filter(!is.na(displaced_share))

bbox_area <- sf::st_bbox(share_map)
x_range <- bbox_area['xmax'] - bbox_area['xmin']
y_range <- bbox_area['ymax'] - bbox_area['ymin']

bbox_area_expanded <- bbox_area
bbox_area_expanded['xmin'] <- bbox_area['xmin'] - 0.35 * x_range
bbox_area_expanded['xmax'] <- bbox_area['xmax'] + 0.35 * x_range
bbox_area_expanded['ymin'] <- bbox_area['ymin'] - 0.25 * y_range
bbox_area_expanded['ymax'] <- bbox_area['ymax'] + 0.25 * y_range

zoom_box <- sf::st_as_sfc(bbox_area_expanded)
sf::st_crs(zoom_box) <- sf::st_crs(municipio)

share_palette <- c('#FFF7BC', '#FEC44F', '#FD8D3C', '#F03B20', '#BD0026')
share_breaks <- c(0.01, 0.05, 0.10, 0.20, 0.40)

legend_map <- ggplot() +
  geom_sf(data = municipio, fill = 'grey80', color = 'grey55', linewidth = 0.2) +
  geom_sf(data = share_map, aes(fill = displaced_share), color = 'grey45', linewidth = 0.2) +
  scale_fill_gradientn(
    colours = share_palette,
    values = scales::rescale(c(0.01, 0.05, 0.10, 0.20, 0.41)),
    limits = c(0.01, 0.41),
    breaks = share_breaks,
    labels = scales::label_percent(accuracy = 1),
    guide = guide_colorbar(
      title = 'Displaced share',
      direction = 'horizontal',
      title.position = 'top',
      title.hjust = 0,
      label.position = 'bottom',
      ticks = FALSE,
      frame.colour = 'grey65',
      barwidth = unit(4.4, 'cm'),
      barheight = unit(0.24, 'cm')
    )
  ) +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.justification = 'left',
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(face = 'bold', size = 8.8),
    legend.text = element_text(size = 7.8),
    legend.spacing.x = unit(0.04, 'cm')
  )

legend_grob <- cowplot::get_legend(legend_map)

main_map <- ggplot() +
  geom_sf(data = municipio, fill = 'grey80', color = 'grey55', linewidth = 0.2) +
  geom_sf(data = share_map, aes(fill = displaced_share), color = 'grey45', linewidth = 0.2) +
  scale_fill_gradientn(
    colours = share_palette,
    values = scales::rescale(c(0.01, 0.05, 0.10, 0.20, 0.41)),
    limits = c(0.01, 0.41),
    guide = 'none'
  ) +
  coord_sf(
    xlim = c(bbox_area_expanded['xmin'], bbox_area_expanded['xmax']),
    ylim = c(bbox_area_expanded['ymin'], bbox_area_expanded['ymax']),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

rj_inset <- ggplot() +
  geom_sf(data = municipio, fill = 'grey80', color = 'grey55', linewidth = 0.15) +
  geom_sf(data = share_map, aes(fill = displaced_share), color = 'grey45', linewidth = 0.15) +
  geom_sf(data = zoom_box, fill = NA, color = 'black', linewidth = 0.55, linetype = 'dashed') +
  scale_fill_gradientn(
    colours = share_palette,
    values = scales::rescale(c(0.01, 0.05, 0.10, 0.20, 0.41)),
    limits = c(0.01, 0.41),
    guide = 'none'
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

brazil_inset <- ggplot() +
  geom_sf(data = geobr::read_country(), fill = 'white', color = 'grey75', linewidth = 0.15) +
  geom_sf(data = subset(estados, code_state == 33), fill = '#F03B20', color = 'grey50', linewidth = 0.35) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

affected_region_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(main_map, x = 0.00, y = 0.15, width = 0.71, height = 0.70) +
  cowplot::draw_plot(rj_inset, x = 0.75, y = 0.60, width = 0.22, height = 0.26) +
  cowplot::draw_plot(brazil_inset, x = 0.79, y = 0.15, width = 0.14, height = 0.21) +
  cowplot::draw_label('Rio de Janeiro', x = 0.86, y = 0.91, fontface = 'bold', size = 13) +
  cowplot::draw_label('Brazil', x = 0.86, y = 0.11, fontface = 'bold', size = 13) +
  cowplot::draw_grob(
    grid::rectGrob(gp = grid::gpar(fill = scales::alpha('white', 0.95), col = 'grey70', lwd = 1.0)),
    x = 0.03, y = 0.17, width = 0.25, height = 0.10
  ) +
  cowplot::draw_grob(legend_grob, x = 0.042, y = 0.191, width = 0.215, height = 0.060) +
  cowplot::draw_grob(
    grid::segmentsGrob(
      x0 = grid::unit(0.71, 'npc'), y0 = grid::unit(0.59, 'npc'),
      x1 = grid::unit(0.75, 'npc'), y1 = grid::unit(0.69, 'npc'),
      arrow = grid::arrow(length = grid::unit(0.11, 'inches'), type = 'closed'),
      gp = grid::gpar(col = 'grey25', lwd = 1.25)
    )
  ) +
  cowplot::draw_grob(
    grid::segmentsGrob(
      x0 = grid::unit(0.86, 'npc'), y0 = grid::unit(0.60, 'npc'),
      x1 = grid::unit(0.86, 'npc'), y1 = grid::unit(0.37, 'npc'),
      arrow = grid::arrow(length = grid::unit(0.11, 'inches'), type = 'closed'),
      gp = grid::gpar(col = 'grey25', lwd = 1.25)
    )
  )

affected_region_plot

ggsave('./results/affected_region.jpg', plot = affected_region_plot, width = 20, height = 11.25, units = 'cm', dpi = 300)

geocode <- geocode %>%
  mutate(variable = case_when(
    raio == 1 ~ "Treated\n(Within Coverage)",
    between(min_dist, 20, 30) ~ "Control\n(20â€“30 km)",
    between(min_dist, 0, 20) ~ "Non-Treated",
    TRUE ~ "Outside Buffer"
  ))



# Filtrar apenas municÃ­pios com impacto severo/crÃ­tico para zoom e exibiÃ§Ã£o
municipio_foco <- municipio %>%
  filter(name_muni == 'TeresÃ³polis')


# Bounding box original
bbox_zoom <- sf::st_bbox(municipio_foco)

# Fator de expansÃ£o (ex: 10% a mais em cada direÃ§Ã£o)
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
      "Control\n(20â€“30 km)" = "#2166AC",
      "Non-Treated" = "#999999",
      "Outside Buffer" = "white"
    ),
    breaks = c(
      "Treated\n(Within Coverage)",
      "Control\n(20â€“30 km)",
      "Non-Treated",
      "Outside Buffer"
    ),
    guide = guide_legend(
      title = "Treatment Classification",
      title.position = "top",
      title.hjust = 0.5
    )
  ) + 

  
  # Zoom na Ã¡rea de interesse
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

ggsave('./results/pontos deslizamentos.jpg', width = 15, height = 10, units = 'cm',
       dpi=300)

