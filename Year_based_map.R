library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(MASS)

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")
data_2022 <- df %>% filter(Year == 2022)

# Negative Binomial
nb_model <- glm.nb(`Malaria cases` ~ y1 + y2 + y3 + y5, data = data_2022)
data_2022$Predicted <- predict(nb_model, newdata = data_2022, type = "response", na.action = na.exclude)
data_2022$Residuals <- data_2022$`Malaria cases` - data_2022$Predicted

africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
merged_map <- africa_map %>% left_join(data_2022, by = c("name" = "Country"))

ggplot(data = merged_map) +
  geom_sf(aes(fill = Residuals), color = "black", size = 0.2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Residuals\n(Actual - Pred)", na.value = "grey85") +
  labs(title = "Model Residual Map (Negative Binomial - 2022)",
       subtitle = "Blue = Over-prediction | Red = Under-prediction") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



selected_years <- c(2000, 2008, 2014, 2022)
df_years <- df %>% filter(Year %in% selected_years)

merged_years <- africa_map %>%
  inner_join(df_years, by = c("name" = "Country"))

ggplot(data = merged_years) +
  geom_sf(aes(fill = `Malaria incidence`), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", name = "Incidence", na.value = "grey85") +
  facet_wrap(~ Year, ncol = 2) +
  labs(title = "Evolution of Malaria Incidence Across Years in Africa") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = 12))