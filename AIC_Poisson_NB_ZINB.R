library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(pscl)

df <- read_excel("C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx")

pois_model <- glm(`Malaria cases` ~ y1 + y2 + y3 + y5, data = df, family = poisson)
nb_model <- glm.nb(`Malaria cases` ~ y1 + y2 + y3 + y5, data = df)
zinb_model <- zeroinfl(`Malaria cases` ~ y1 + y2 + y3 + y5 | 1, data = df, dist = "negbin", control = zeroinfl.control(maxit = 1000))

aic_bic_df <- data.frame(
  Model = c("Poisson", "Negative Binomial", "ZINB"),
  AIC = c(AIC(pois_model), AIC(nb_model), AIC(zinb_model)),
  BIC = c(BIC(pois_model), BIC(nb_model), BIC(zinb_model))
) %>% pivot_longer(cols = c(AIC, BIC), names_to = "Criterion", values_to = "Value")

ggplot(aic_bic_df, aes(x = Model, y = Value, fill = Criterion)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_y_log10() +
  labs(
    title = "Model Comparison: AIC and BIC",
    subtitle = "Lower values indicate better model fit",
    y = "Information Criterion (Log Scale)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )