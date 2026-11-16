# Top 10 African Countries (2022)
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
