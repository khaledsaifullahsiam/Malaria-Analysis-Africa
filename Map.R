library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")
View(df)
map_data_year <- df %>% filter(Year == 2022)

africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
merged_map <- africa_map %>%
  left_join(map_data_year, by = c("name" = "Country"))

# Analytical map 
ggplot(data = merged_map) +
  geom_sf(aes(fill = `Malaria incidence`), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    option = "plasma", 
    trans = "log10", 
    name = "Malaria Incidence\n(Log Scale)",
    na.value = "grey85"
  ) +
  labs(
    title = "Malaria Incidence Across Africa (2022)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )