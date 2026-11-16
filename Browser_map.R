
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(plotly)
library(htmlwidgets)

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")
map_data_year <- df %>% filter(Year == 2022)
africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

merged_map <- africa_map %>%
  left_join(map_data_year, by = c("name" = "Country"))

p <- ggplot(data = merged_map) +
  geom_sf(aes(
    fill = `Malaria incidence`,
    text = paste("Country:", name, "<br>Malaria Incidence:", `Malaria incidence`)
  ), color = "white", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", name = "Incidence", na.value = "grey85") +
  labs(title = "Malaria Incidence Across Africa (2022)") +
  theme_minimal()

interactive_map <- ggplotly(p, tooltip = "text")

saveWidget(interactive_map, "Africa_Malaria_Map.html", selfcontained = TRUE)
browseURL("Africa_Malaria_Map.html")