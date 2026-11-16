library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)       
library(pscl)      
library(AER)      
library(scales)     
library(sf)       
library(rnaturalearth)
library(rnaturalearthdata)

file_path <- "C:/Users/ALPHA TECH/OneDrive/Desktop/Africa/Africa_data.xlsx"
df <- read_excel(file_path)
View(df)
df <- df %>%
  rename(
    cases = `Malaria cases`,
    incidence = `Malaria incidence`
  ) %>%
  mutate(
    Country = as.factor(Country),
    Year = as.numeric(Year),
    cases = as.numeric(cases),
    incidence = as.numeric(incidence)
  ) %>%
  arrange(Country, Year)

# Dataset summary
cat("Total Observations:", nrow(df), "\n")
cat("Total Countries:", n_distinct(df$Country), "\n")

zero_count <- sum(df$cases == 0, na.rm = TRUE)
zero_pct <- (zero_count / nrow(df)) * 100
cat(sprintf("Observed Zero Counts: %d (%.2f%%)\n", zero_count, zero_pct))

#Poisson, Negative Binomial (NB), and ZINB
poisson_model <- glm(cases ~ Year + Country, 
                     data = df, 
                     family = poisson(link = "log"))

# Overdispersion Statistic (Pearson Chi-Square / Residual DF)
pearson_chi2 <- sum(residuals(poisson_model, type = "pearson")^2)
dispersion_stat <- pearson_chi2 / df.residual(poisson_model)
cat(sprintf("Poisson Overdispersion Statistic: %.2f\n", dispersion_stat)) # ~ 145139.35
cat(sprintf("Poisson AIC: %.2f\n", AIC(poisson_model))) # ~ 150225857.28

#NB
nb_model <- glm.nb(cases ~ Year + Country, data = df)

# Extract Poster Metrics: Dispersion Parameter (alpha = 1 / theta), Beta, IRR, CI
alpha_nb <- 1 / nb_model$theta
beta_year <- coef(nb_model)["Year"]
irr_year <- exp(beta_year)
ci_irr <- exp(confint.default(nb_model)["Year", ])
p_val_year <- summary(nb_model)$coefficients["Year", "Pr(>|z|)"]

cat("\n--- Negative Binomial (NB) Results ---\n")
cat(sprintf("NB AIC: %.2f\n", AIC(nb_model))) # ~ 29931.31
cat(sprintf("Dispersion Parameter (alpha): %.4f\n", alpha_nb)) # ~ 0.2693
cat(sprintf("Year Coefficient (beta): %.5f\n", beta_year)) # ~ -0.00660
cat(sprintf("IRR (Annual Change): %.4f\n", irr_year)) # ~ 0.9934
cat(sprintf("95%% CI for IRR: %.4f - %.4f\n", ci_irr[1], ci_irr[2]))
cat(sprintf("p-value: %.4f\n", p_val_year))

# Expected Zero Counts under NB
mu_nb <- predict(nb_model, type = "response")
theta_nb <- nb_model$theta
expected_zeros_nb <- sum((theta_nb / (theta_nb + mu_nb))^theta_nb)
cat(sprintf("Expected Zeros (NB): %.0f\n", expected_zeros_nb)) # ~ 32

#ZINB
zinb_model <- zeroinfl(cases ~ Year + Country | 1, 
                       data = df, 
                       dist = "negbin")

cat("\n--- ZINB Results ---\n")
cat(sprintf("ZINB AIC: %.2f\n", AIC(zinb_model)))
cat(sprintf("ZINB BIC: %.2f\n", BIC(zinb_model)))

# ==============================================================================
# 4. Model Comparison (AIC & BIC Table and Bar Chart - Figure 2)
# ==============================================================================
model_comp <- data.frame(
  Model = c("Poisson", "Negative Binomial", "ZINB"),
  AIC = c(AIC(poisson_model), AIC(nb_model), AIC(zinb_model)),
  BIC = c(BIC(poisson_model), BIC(nb_model), BIC(zinb_model))
)
print(model_comp)

model_comp_long <- pivot_longer(model_comp, cols = c("AIC", "BIC"), 
                                names_to = "Criterion", values_to = "Value")

p_aic_bic <- ggplot(model_comp_long, aes(x = Model, y = Value, fill = Criterion)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_y_log10(labels = scientific) +
  labs(title = "Model Comparison: AIC and BIC",
       subtitle = "Lower values indicate better model fit",
       y = "Information Criterion (Log Scale)", x = "Model") +
  theme_minimal()
print(p_aic_bic)


#Forecasting & Model Validation
df_lag <- df %>%
  group_by(Country) %>%
  mutate(
    lag_cases = lag(cases, 1),
    lag_incidence = lag(incidence, 1),
    pop_est = ifelse(incidence > 0, (cases / incidence) * 1000, NA)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_cases))

# Train-Test Split (Train: 2001-2021, Test: 2022)
train_data <- df_lag %>% filter(Year < 2022)
test_data  <- df_lag %>% filter(Year == 2022)

lag_poisson <- glm(cases ~ log1p(lag_incidence) + offset(log(pop_est + 1)) + Country, 
                   data = train_data %>% filter(!is.na(pop_est)), 
                   family = poisson(link = "log"))

test_valid <- test_data %>% filter(!is.na(pop_est) & cases > 0)
test_valid$pred_cases <- predict(lag_poisson, newdata = test_valid, type = "response")

mare <- mean(abs(test_valid$cases - test_valid$pred_cases) / test_valid$cases) * 100
cat(sprintf("\nLagged Poisson MARE (2022 Validation): %.2f%%\n", mare))


annual_trend <- df %>%
  group_by(Year) %>%
  summarise(Total_Cases_Millions = sum(cases, na.rm = TRUE) / 1e6)

p_trend <- ggplot(annual_trend, aes(x = Year, y = Total_Cases_Millions)) +
  geom_line(color = "#2b5c8f", linewidth = 1.2) +
  geom_point(color = "#2b5c8f", size = 2.5) +
  labs(title = "Annual Malaria Cases in Africa (2000–2022)",
       subtitle = "Longitudinal Trend Analysis",
       x = "Year", y = "Reported malaria cases (millions)") +
  theme_minimal()
print(p_trend)

#Top 10 Countries: Malaria Cases (2022)
top10_2022 <- df %>%
  filter(Year == 2022) %>%
  arrange(desc(cases)) %>%
  slice_head(n = 10) %>%
  mutate(Cases_Millions = cases / 1e6)

p_top10 <- ggplot(top10_2022, aes(x = reorder(Country, Cases_Millions), y = Cases_Millions)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  coord_flip() +
  labs(title = "Top 10 Countries: Malaria Cases (2022)",
       x = "Country", y = "Reported malaria cases (millions)") +
  theme_minimal()
print(p_top10)

# Pearson Residuals vs Fitted Values
df$pois_fitted <- fitted(poisson_model)
df$pois_resid  <- residuals(poisson_model, type = "pearson")
df$nb_fitted   <- fitted(nb_model)
df$nb_resid    <- residuals(nb_model, type = "pearson")

p_res_pois <- ggplot(df, aes(x = pois_fitted, y = pois_resid)) +
  geom_point(alpha = 0.5, color = "#2b5c8f") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(title = "Pearson Residuals vs Fitted: Poisson Model",
       x = "Fitted Values (Log Scale)", y = "Pearson Residuals") +
  theme_minimal()

p_res_nb <- ggplot(df, aes(x = nb_fitted, y = nb_resid)) +
  geom_point(alpha = 0.5, color = "#2ca25f") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(title = "Pearson Residuals vs Fitted: Negative Binomial Model",
       x = "Fitted Values (Log Scale)", y = "Pearson Residuals") +
  theme_minimal()

print(p_res_pois)
print(p_res_nb)

#Malaria Incidence Across Africa (2022)
africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

df_2022 <- df %>% filter(Year == 2022)
map_data_2022 <- africa_map %>%
  left_join(df_2022, by = c("name" = "Country"))

p_map <- ggplot(map_data_2022) +
  geom_sf(aes(fill = incidence), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", na.value = "grey80",
                       name = "Malaria Incidence\n(Log Scale)") +
  labs(title = "Malaria Incidence Across Africa (2022)") +
  theme_void()
print(p_map)