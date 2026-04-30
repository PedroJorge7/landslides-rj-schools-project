library(dplyr)
library(geobr)
library(ggplot2)
library(sf)
library(data.table)
library(ggspatial)

estado_rj <- read_state(code_state = 33, cache = FALSE)

municipio <- read_municipality(code_muni = 'all', year = 2017, cache = FALSE) %>% 
  mutate(code_muni = as.numeric(substr(code_muni,1,6))) %>% 
  filter(abbrev_state == "RJ") %>% 
  mutate(
    afetados = as.integer(name_muni %in% c(
      "Areal","Bom Jardim","Nova Friburgo","SÃƒÆ’Ã‚Â£o JosÃƒÆ’Ã‚Â© Do Vale Do Rio Preto",
      "Sumidouro","PetrÃƒÆ’Ã‚Â³polis","TeresÃƒÆ’Ã‚Â³polis","Santa Maria Madalena",
      "Sapucaia","ParaÃƒÆ’Ã‚Â­ba Do Sul","SÃƒÆ’Ã‚Â£o SebastiÃƒÆ’Ã‚Â£o Do Alto","TrÃƒÆ’Ã‚Âªs Rios",
      "Cordeiro","Carmo","Macuco","Cantagalo"
    )),
    calamidade = as.integer(name_muni %in% c(
      "Areal","Bom Jardim","Nova Friburgo","SÃƒÆ’Ã‚Â£o JosÃƒÆ’Ã‚Â© Do Vale Do Rio Preto",
      "Sumidouro","PetrÃƒÆ’Ã‚Â³polis","TeresÃƒÆ’Ã‚Â³polis"
    )),
    maiores_afetados = as.integer(name_muni %in% c(
      "Nova Friburgo","PetrÃƒÆ’Ã‚Â³polis","TeresÃƒÆ’Ã‚Â³polis"
    )),
    arredores = as.integer(name_muni %in% c(
      "Areal","ParaÃƒÆ’Ã‚Â­ba Do Sul","Nova Friburgo","PetrÃƒÆ’Ã‚Â³polis","TeresÃƒÆ’Ã‚Â³polis",
      "Bom Jardim","SÃƒÆ’Ã‚Â£o JosÃƒÆ’Ã‚Â© Do Vale Do Rio Preto","Sumidouro",
      "Cachoeiras De Macacu","Duas Barras","Sapucaia","TrÃƒÆ’Ã‚Âªs Rios",
      "MagÃƒÆ’Ã‚Â©","Guapimirim","Silva Jardim","Duque De Caxias","Cordeiro"
    )),
    Afetados = afetados + calamidade + maiores_afetados,
    Afetados = ifelse(Afetados == 3, "Maiores Afetados",
               ifelse(Afetados == 2, "Afetados e calamidade",
               ifelse(Afetados == 1, "Afetados", "NÃƒÆ’Ã‚Â£o Afetado")))
  ) %>% 
  st_as_sf()



arredores <- subset(municipio,arredores==1) %>% sf::st_transform(32723)

### Leitura dos dados da rais -----

# DefiniÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o da abreviaÃƒÆ’Ã‚Â§ÃƒÆ’Ã‚Â£o
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
  geom_vline(aes(xintercept = mean(Runnout3 / 1000)), linetype = "dashed", color = "darkred", linewidth = 0.8) +
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

micro <- read_micro_region(code_micro='RJ', cache = FALSE) %>%
  filter(name_micro == "Serrana" | name_micro == "Nova Friburgo")

#temp <- subset(municipio, calamidade == 1)

#raio_mapa <- raio %>% st_union()

rj <- geobr::read_municipality(code_muni = 'RJ', cache = FALSE)

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

share_palette <- c('#FEE5D9', '#FCBBA1', '#FC9272', '#FB6A4A', '#CB181D')
share_limits <- c(0.00, 0.40)
share_breaks <- seq(0.00, 0.40, by = 0.10)
share_values <- scales::rescale(share_breaks, from = share_limits)
map_frame_colour <- '#2F2F2F'
map_fill_colour <- '#F3F3F0'
map_border_colour <- '#8F8F89'
connector_colour <- '#6A6A6A'

affected_panel_theme <- theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

legend_map <- ggplot() +
  geom_sf(data = municipio, fill = '#EEF2F6', color = '#B8C2CC', linewidth = 0.2) +
  geom_sf(data = share_map, aes(fill = displaced_share), color = '#9B6A6A', linewidth = 0.2) +
  scale_fill_gradientn(
    colours = share_palette,
    values = share_values,
    limits = share_limits,
    oob = scales::squish,
    breaks = share_breaks,
    labels = scales::label_percent(accuracy = 1),
    guide = guide_colorbar(
      title = 'Displaced share',
      direction = 'horizontal',
      title.position = 'top',
      title.hjust = 0.5,
      label.position = 'bottom',
      ticks = TRUE,
      frame.colour = 'grey70',
      barwidth = grid::unit(5.4, 'cm'),
      barheight = grid::unit(0.28, 'cm')
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
    legend.spacing.x = grid::unit(0.04, 'cm')
  )

legend_grob <- cowplot::get_legend(legend_map)

main_map <- ggplot() +
  geom_sf(data = municipio, fill = '#EEF2F6', color = '#B8C2CC', linewidth = 0.2) +
  geom_sf(data = share_map, aes(fill = displaced_share), color = '#9B6A6A', linewidth = 0.2) +
  scale_fill_gradientn(
    colours = share_palette,
    values = share_values,
    limits = share_limits,
    oob = scales::squish,
    guide = 'none'
  ) +
  coord_sf(
    xlim = c(bbox_area_expanded['xmin'], bbox_area_expanded['xmax']),
    ylim = c(bbox_area_expanded['ymin'], bbox_area_expanded['ymax']),
    expand = FALSE
  ) +
  affected_panel_theme

rj_inset <- ggplot() +
  geom_sf(data = municipio, fill = '#EEF2F6', color = '#B8C2CC', linewidth = 0.15) +
  geom_sf(data = share_map, aes(fill = displaced_share), color = '#9B6A6A', linewidth = 0.15) +
  geom_sf(data = zoom_box, fill = NA, color = 'black', linewidth = 0.55, linetype = 'dashed') +
  scale_fill_gradientn(
    colours = share_palette,
    values = share_values,
    limits = share_limits,
    oob = scales::squish,
    guide = 'none'
  ) +
  affected_panel_theme

brazil_inset <- ggplot() +
  geom_sf(data = geobr::read_country(cache = FALSE), fill = 'white', color = 'grey75', linewidth = 0.15) +
  geom_sf(data = estado_rj, fill = '#CB181D', color = '#8A7A7A', linewidth = 0.35) +
  affected_panel_theme

affected_region_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(main_map, x = 0.00, y = 0.18, width = 0.71, height = 0.68) +
  cowplot::draw_plot(rj_inset, x = 0.75, y = 0.59, width = 0.22, height = 0.26) +
  cowplot::draw_plot(brazil_inset, x = 0.79, y = 0.19, width = 0.14, height = 0.21) +
  cowplot::draw_label('Rio de Janeiro', x = 0.86, y = 0.91, fontface = 'bold', size = 12.5) +
  cowplot::draw_label('Brazil', x = 0.86, y = 0.11, fontface = 'bold', size = 12.5) +
  cowplot::draw_grob(legend_grob, x = 0.17, y = 0.012, width = 0.47, height = 0.10) +
  cowplot::draw_grob(
    grid::curveGrob(
      x1 = grid::unit(0.60, 'npc'), y1 = grid::unit(0.52, 'npc'),
      x2 = grid::unit(0.75, 'npc'), y2 = grid::unit(0.665, 'npc'),
      curvature = -0.12,
      arrow = grid::arrow(length = grid::unit(0.08, 'inches'), type = 'closed'),
      gp = grid::gpar(col = connector_colour, lwd = 1.0, lineend = 'round')
    )
  ) +
  cowplot::draw_grob(
    grid::curveGrob(
      x1 = grid::unit(0.84, 'npc'), y1 = grid::unit(0.575, 'npc'),
      x2 = grid::unit(0.825, 'npc'), y2 = grid::unit(0.405, 'npc'),
      curvature = 0.14,
      arrow = grid::arrow(length = grid::unit(0.08, 'inches'), type = 'closed'),
      gp = grid::gpar(col = connector_colour, lwd = 1.0, lineend = 'round')
    )
  )
affected_region_plot

ggsave('./results/affected_region.jpg', plot = affected_region_plot, width = 20, height = 11.25, units = 'cm', dpi = 300)

treatment_levels <- c(
  'Treated\n(Within Coverage)',
  'Control\n(20-30 km)',
  'Non-Treated',
  'Outside Buffer'
)
treatment_fill_values <- c(
  'Treated\n(Within Coverage)' = '#C32047',
  'Control\n(20-30 km)' = '#2E6CB1',
  'Non-Treated' = '#AAAAAA',
  'Outside Buffer' = 'white'
)
treatment_outline_values <- c(
  'Treated\n(Within Coverage)' = '#5C1022',
  'Control\n(20-30 km)' = '#173A62',
  'Non-Treated' = '#636363',
  'Outside Buffer' = '#B8B8B8'
)

geocode <- geocode %>%
  mutate(
    variable = case_when(
      raio == 1 ~ 'Treated\n(Within Coverage)',
      between(min_dist, 20, 30) ~ 'Control\n(20-30 km)',
      between(min_dist, 0, 20) ~ 'Non-Treated',
      TRUE ~ 'Outside Buffer'
    ),
    variable = factor(variable, levels = treatment_levels)
  )

buffer_30km <- st_buffer(raio_otimo, dist = 30000)
buffer_30km <- buffer_30km %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast('MULTIPOLYGON') %>%
  st_as_sf()

buffer_20km <- st_buffer(raio_otimo, dist = 20000)
buffer_20km <- buffer_20km %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast('MULTIPOLYGON') %>%
  st_as_sf()

anel_20_30km <- st_difference(buffer_30km, buffer_20km)

buffer_raio_otimo <- st_buffer(raio_otimo, dist = raio_otimo$Runnout3)
buffer_raio_otimo <- buffer_raio_otimo %>%
  st_union() %>%
  st_make_valid() %>%
  st_cast('MULTIPOLYGON') %>%
  st_as_sf()

buffer_30km_map <- st_transform(buffer_30km, st_crs(municipio))
anel_20_30km_map <- st_transform(anel_20_30km, st_crs(municipio))
buffer_raio_otimo_map <- st_transform(buffer_raio_otimo, st_crs(municipio))

bbox_zoom <- sf::st_bbox(buffer_30km_map)
x_range <- bbox_zoom['xmax'] - bbox_zoom['xmin']
y_range <- bbox_zoom['ymax'] - bbox_zoom['ymin']

bbox_zoom_expanded <- bbox_zoom
bbox_zoom_expanded['xmin'] <- bbox_zoom['xmin'] - 0.08 * x_range
bbox_zoom_expanded['xmax'] <- bbox_zoom['xmax'] + 0.08 * x_range
bbox_zoom_expanded['ymin'] <- bbox_zoom['ymin'] - 0.10 * y_range
bbox_zoom_expanded['ymax'] <- bbox_zoom['ymax'] + 0.10 * y_range

geocode_treated <- geocode %>% filter(variable == 'Treated\n(Within Coverage)')
geocode_control <- geocode %>% filter(variable == 'Control\n(20-30 km)')
geocode_non_treated <- geocode %>% filter(variable == 'Non-Treated')
geocode_outside <- geocode %>% filter(variable == 'Outside Buffer')

treatment_legend_plot <- ggplot(
  data = tibble::tibble(
    variable = factor(treatment_levels, levels = treatment_levels),
    x = seq_along(treatment_levels),
    y = 1
  ),
  aes(x, y)
) +
  geom_point(aes(fill = variable, color = variable), shape = 21, size = 3.1, stroke = 0.45) +
  scale_fill_manual(
    values = treatment_fill_values,
    breaks = treatment_levels,
    guide = guide_legend(
      title = 'Treatment Classification',
      title.position = 'top',
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE
    )
  ) +
  scale_color_manual(values = treatment_outline_values, guide = 'none') +
  theme_void() +
  theme(
    legend.position = 'bottom',
    legend.title = element_text(face = 'bold', size = 9),
    legend.text = element_text(size = 7.8),
    legend.key.width = grid::unit(1.0, 'cm'),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0)
  )

treatment_legend_grob <- cowplot::get_legend(treatment_legend_plot)

treatment_map <- ggplot() +
  geom_sf(data = municipio, fill = 'white', color = 'grey40', linewidth = 0.18) +
  geom_sf(data = anel_20_30km_map, fill = '#2166AC', alpha = 0.16, color = NA) +
  geom_sf(data = buffer_raio_otimo_map, fill = '#B2182B', alpha = 0.14, color = NA) +
  geom_sf(data = geocode_outside, shape = 21, size = 1.50, stroke = 0.24, fill = 'white', color = scales::alpha('#3B3B3B', 0.30)) +
  geom_sf(data = geocode_non_treated, shape = 21, size = 1.60, stroke = 0.26, fill = treatment_fill_values['Non-Treated'], color = treatment_outline_values['Non-Treated'], alpha = 0.72) +
  geom_sf(data = geocode_control, shape = 21, size = 1.72, stroke = 0.28, fill = treatment_fill_values['Control\n(20-30 km)'], color = treatment_outline_values['Control\n(20-30 km)'], alpha = 0.94) +
  geom_sf(data = geocode_treated, shape = 21, size = 1.82, stroke = 0.30, fill = treatment_fill_values['Treated\n(Within Coverage)'], color = treatment_outline_values['Treated\n(Within Coverage)'], alpha = 0.98) +
  coord_sf(
    xlim = c(bbox_zoom_expanded['xmin'], bbox_zoom_expanded['xmax']),
    ylim = c(bbox_zoom_expanded['ymin'], bbox_zoom_expanded['ymax']),
    expand = FALSE
  ) +
  annotation_north_arrow(
    location = 'bl',
    which_north = 'true',
    pad_x = grid::unit(0.55, 'cm'),
    pad_y = grid::unit(0.70, 'cm'),
    height = grid::unit(0.95, 'cm'),
    width = grid::unit(0.95, 'cm'),
    style = north_arrow_orienteering
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 2, 4)
  )

treatment_classification_plot <- cowplot::plot_grid(
  treatment_map,
  cowplot::ggdraw() + cowplot::draw_grob(treatment_legend_grob),
  ncol = 1,
  rel_heights = c(0.88, 0.12)
)

treatment_classification_plot

ggsave('./results/pontos deslizamentos.jpg', plot = treatment_classification_plot, width = 15, height = 10, units = 'cm', dpi = 300)
