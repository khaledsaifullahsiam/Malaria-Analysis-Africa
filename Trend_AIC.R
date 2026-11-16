library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rnaturalearth)
library(MASS)
library(pscl)

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")

#Figure:Annual Malaria Cases Trend
annual_trend <- df %>%
  group_by(Year) %>%
  summarize(Total_Cases_Millions = sum(`Malaria cases`, na.rm = TRUE) / 1e6)

ggplot(annual_trend, aes(x = Year, y = Total_Cases_Millions)) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(color = "#1f77b4", size = 2.5) +
  labs(
    title = "Annual Malaria Cases in Africa (2000–2022)",
    subtitle = "Longitudinal Trend Analysis",
    x = "Year",
    y = "Reported malaria cases (millions)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Figure: Top 10 African Countries (2022)
top_10_countries <- df %>%
  filter(Year == 2022) %>%
  mutate(Cases_Millions = `Malaria cases` / 1e6) %>%
  arrange(desc(Cases_Millions)) %>%
  slice(1:10)

ggplot(top_10_countries, aes(x = reorder(Country, Cases_Millions), y = Cases_Millions)) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Top 10 Countries:Malaria Cases (2022)",
    x = "Country",
    y = "Reported malaria cases (millions)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Figure:AIC Comparison (Poisson vs. NB)
aic_data <- data.frame(
  Model = c("Poisson", "Negative Binomial"),
  AIC = c(150225857.28, 29931.31)
)

ggplot(aic_data, aes(x = Model, y = AIC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_log10() +
  labs(
    title = "Model Comparison: AIC Value (Log Scale)",
    y = "AIC (log scale)",
    x = "Model Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")

# Model Fitting & Diagnostics
data_2022 <- df %>% filter(Year == 2022)

# Poisson,NB,ZINB
pois_model <- glm(`Malaria cases` ~ y1 + y2 + y3 + y5, data = data_2022, family = poisson)

nb_model <- glm.nb(`Malaria cases` ~ y1 + y2 + y3 + y5, data = data_2022)
print(summary(nb_model))
zinb_model <- zeroinfl(`Malaria cases` ~ y1 + y2 + y3 + y5 | 1, 
                       data = data_2022, 
                       dist = "negbin",
                       control = zeroinfl.control(maxit = 1000))
print(summary(zinb_model))