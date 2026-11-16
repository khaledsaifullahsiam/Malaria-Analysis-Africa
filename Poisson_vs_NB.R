library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(MASS)      
library(pscl)     
library(patchwork) 

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")
data_2022 <- df %>% filter(Year == 2022)

#Poisson Model
pois_model <- glm(`Malaria cases` ~ y1 + y2 + y3 + y5, data = data_2022, family = poisson)
data_2022$Pred_Poisson <- predict(pois_model, type = "response")

#NB
nb_model <- glm.nb(`Malaria cases` ~ y1 + y2 + y3 + y5, data = data_2022)
data_2022$Pred_NB <- predict(nb_model, type = "response")

africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
merged_map <- africa_map %>% left_join(data_2022, by = c("name" = "Country"))

#Maps
p1 <- ggplot(data = merged_map) +
  geom_sf(aes(fill = Pred_Poisson), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", name = "Predicted", na.value = "grey85") +
  labs(title = "Poisson") +
  theme_minimal() + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

p2 <- ggplot(data = merged_map) +
  geom_sf(aes(fill = Pred_NB), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", name = "Predicted", na.value = "grey85") +
  labs(title = "NegBin") +
  theme_minimal() + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

p3 <- ggplot(data = merged_map) +
  geom_sf(aes(fill = Pred_ZINB), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", name = "Predicted", na.value = "grey85") +
  labs(title = "ZINB") +
  theme_minimal() + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

#Combine maps
(p1 | p2) + plot_annotation(title = "Malaria Case Predictions (Poisson vs NB)")