library(dplyr)
library(geobr)
library(ggplot2)
library(sf)
library(cowplot)
library(grid)

estados <- read_state(code_state = 'all')
municipio <- read_municipality(code_muni = 'all', year = 2017) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6))) %>%
  filter(abbrev_state == 'RJ') %>%
  st_as_sf()

study_area_codes <- c(330022, 330050, 330340, 330390, 330515, 330570, 330580)

municipio <- municipio %>%
  mutate(study_area = code_muni %in% study_area_codes)

study_area_map <- municipio %>%
  filter(study_area)

bbox_area <- st_bbox(study_area_map)
x_range <- bbox_area['xmax'] - bbox_area['xmin']
y_range <- bbox_area['ymax'] - bbox_area['ymin']

bbox_area_expanded <- bbox_area
bbox_area_expanded['xmin'] <- bbox_area['xmin'] - 0.35 * x_range
bbox_area_expanded['xmax'] <- bbox_area['xmax'] + 0.35 * x_range
bbox_area_expanded['ymin'] <- bbox_area['ymin'] - 0.25 * y_range
bbox_area_expanded['ymax'] <- bbox_area['ymax'] + 0.25 * y_range

zoom_box <- st_as_sfc(bbox_area_expanded)
st_crs(zoom_box) <- st_crs(municipio)

main_map <- ggplot() +
  geom_sf(data = municipio, fill = 'grey80', color = 'grey55', linewidth = 0.2) +
  geom_sf(data = study_area_map, fill = '#F03B20', color = 'grey45', linewidth = 0.2) +
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
  geom_sf(data = study_area_map, fill = '#F03B20', color = 'grey45', linewidth = 0.15) +
  geom_sf(data = zoom_box, fill = NA, color = 'black', linewidth = 0.55, linetype = 'dashed') +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

brazil_inset <- ggplot() +
  geom_sf(data = read_country(), fill = 'white', color = 'grey75', linewidth = 0.15) +
  geom_sf(data = subset(estados, code_state == 33), fill = '#F03B20', color = 'grey50', linewidth = 0.35) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

location_plot <- ggdraw() +
  draw_plot(main_map, x = 0.00, y = 0.15, width = 0.71, height = 0.70) +
  draw_plot(rj_inset, x = 0.75, y = 0.60, width = 0.22, height = 0.26) +
  draw_plot(brazil_inset, x = 0.79, y = 0.15, width = 0.14, height = 0.21) +
  draw_label('Rio de Janeiro', x = 0.86, y = 0.91, fontface = 'bold', size = 13) +
  draw_label('Brazil', x = 0.86, y = 0.11, fontface = 'bold', size = 13) +
  draw_grob(
    grid::segmentsGrob(
      x0 = unit(0.71, 'npc'), y0 = unit(0.59, 'npc'),
      x1 = unit(0.75, 'npc'), y1 = unit(0.69, 'npc'),
      arrow = arrow(length = unit(0.11, 'inches'), type = 'closed'),
      gp = gpar(col = 'grey25', lwd = 1.25)
    )
  ) +
  draw_grob(
    grid::segmentsGrob(
      x0 = unit(0.86, 'npc'), y0 = unit(0.60, 'npc'),
      x1 = unit(0.86, 'npc'), y1 = unit(0.37, 'npc'),
      arrow = arrow(length = unit(0.11, 'inches'), type = 'closed'),
      gp = gpar(col = 'grey25', lwd = 1.25)
    )
  )

print(location_plot)

ggsave('./results/study_area_location.jpg', plot = location_plot, width = 20, height = 11.25, units = 'cm', dpi = 300)
