library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(MASS)

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")
data_2022 <- df %>% filter(Year == 2022)


nb_model <- glm.nb(`Malaria cases` ~ y1 + y2 + y3 + y5, data = data_2022)
data_2022$Predicted <- predict(nb_model, type = "response")
data_2022$Residuals <- data_2022$`Malaria cases` - data_2022$Predicted

africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
merged_map <- africa_map %>% left_join(data_2022, by = c("name" = "Country"))

ggplot(data = merged_map) +
  geom_sf(aes(fill = Residuals), color = "black", size = 0.2) +
  scale_fill_gradient2(
    low = "blue",      # Over-prediction (Negative residuals)
    mid = "white",     # Perfect fit (Zero)
    high = "red",      # Under-prediction (Positive residuals)
    midpoint = 0,
    name = "Residuals\n(Actual - Pred)",
    na.value = "grey85"
  ) +
  labs(
    title = "Model Residual Map (NB - 2022)",
    subtitle = "Blue = Over-prediction | Red = Under-prediction"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", face = "bold"),
    plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(family = "Arial", face = "bold", size = 12, hjust = 0),
    legend.title = element_text(family = "Arial", face = "bold", size = 11),
    legend.text = element_text(family = "Arial", face = "bold", size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )