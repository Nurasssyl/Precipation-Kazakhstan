# 1. PACKAGES

if(
  !require("pacman")
){
  install.packages("pacman")
}

pacman::p_load(
  pRecipe,
  giscoR,
  terra,
  tidyverse,
  rayshader,
  sf,
  classInt
)

# 2. COUNTRY EXTENT

country_sf <- giscoR::gisco_get_countries(
  country = "KZ",
  resolution = "1"
)

# 3. PRECIPATION DATA

pRecipe::download_data(
  dataset = "mswep",
  path = getwd(),
  domain = "raw",
  timestep = "yearly"
)

list.files()

mswep_data <- terra::rast(
  "mswep_tp_mm_global_197902_202301_025_yearly.nc"
) |>
  terra::crop(
    country_sf
  )

terra::plot(mswep_data[[1]])
plot(sf::st_geometry(country_sf), add = TRUE)


ggsave("visualization.png", plot = map1, width = 10, height = 8, dpi = 300)

# Рассчитать средние осадки по годам
yearly_average <- mswep_df |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    avg_precipitation = mean(precipitation, na.rm = TRUE)
  )

# Добавить средние значения к визуализации
map1 <- ggplot(
  data = mswep_df
) +
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = precipitation
    )
  ) +
  geom_contour(
    aes(
      x = x,
      y = y,
      z = precipitation 
    ), color = "white"
  ) +
  geom_sf(
    data = country_sf,
    fill = "transparent",
    color = "grey10",
    size = .5
  ) +
  scale_fill_gradientn(
    name = "mm",
    colors = colors,
    breaks = breaks,
    labels = round(breaks, 0),
    limits = c(
      min(mswep_df$precipitation),
      max(mswep_df$precipitation)
    )
  ) +
  facet_wrap(~year) +
  guides(
    fill = guide_colourbar(
      direction = "vertical",
      barheight = unit(50, "mm"),
      barwidth = unit(5, "mm"),
      title.position = "top",
      label.position = "right",
      title.hjust = .5,
      label.hjust = .5,
      ncol = 1,
      byrow = FALSE
    )
  ) +
  geom_text(
    data = yearly_average,
    aes(
      x = -Inf, y = -Inf, label = round(avg_precipitation, 1)
    ),
    hjust = -0.1, vjust = -1, size = 3, color = "black",
    inherit.aes = FALSE
  ) +
  theme_for_the_win()

# Сохранить визуализацию
ggsave("precipitation_by_year.png", plot = map1, width = 10, height = 8, dpi = 300)