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

rj <- read_municipality(code_muni = 'RJ')

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

bbox_area <- st_bbox(share_map)
x_range <- bbox_area['xmax'] - bbox_area['xmin']
y_range <- bbox_area['ymax'] - bbox_area['ymin']

bbox_area_expanded <- bbox_area
bbox_area_expanded['xmin'] <- bbox_area['xmin'] - 0.24 * x_range
bbox_area_expanded['xmax'] <- bbox_area['xmax'] + 0.24 * x_range
bbox_area_expanded['ymin'] <- bbox_area['ymin'] - 0.18 * y_range
bbox_area_expanded['ymax'] <- bbox_area['ymax'] + 0.18 * y_range

zoom_box <- st_as_sfc(bbox_area_expanded)
st_crs(zoom_box) <- st_crs(municipio)

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

legend_grob <- get_legend(legend_map)

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
  geom_sf(data = read_country(), fill = 'white', color = 'grey75', linewidth = 0.15) +
  geom_sf(data = subset(estados, code_state == 33), fill = '#F03B20', color = 'grey50', linewidth = 0.35) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'white', color = 'black', linewidth = 0.8),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

displaced_plot <- ggdraw() +
  draw_plot(main_map, x = 0.00, y = 0.15, width = 0.71, height = 0.70) +
  draw_plot(rj_inset, x = 0.75, y = 0.60, width = 0.22, height = 0.26) +
  draw_plot(brazil_inset, x = 0.79, y = 0.15, width = 0.14, height = 0.21) +
  draw_label('Rio de Janeiro', x = 0.86, y = 0.91, fontface = 'bold', size = 13) +
  draw_label('Brazil', x = 0.86, y = 0.11, fontface = 'bold', size = 13) +
  draw_grob(
    grid::rectGrob(gp = grid::gpar(fill = scales::alpha('white', 0.95), col = 'grey70', lwd = 1.0)),
    x = 0.045, y = 0.19, width = 0.27, height = 0.11
  ) +
  draw_grob(legend_grob, x = 0.058, y = 0.214, width = 0.235, height = 0.068) +
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

print(displaced_plot)

ggsave('./results/displaced_population_distribution.jpg', plot = displaced_plot, width = 20, height = 11.25, units = 'cm', dpi = 300)


