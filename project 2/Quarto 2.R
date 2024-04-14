library(readr)
Unicef_Metadata_continents_ <- read_csv("Unicef Metadata (continents).csv")
View(Unicef_Metadata_continents_)

library(readr)
Unicef_Indicator_2 <- read_csv("Unicef Indicator 2.csv")
View(Unicef_Indicator_2)

#Package Installation
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gapminder")
install.packages("maps")
install.packages ("maps")
install.packages("gridExtra")

#Package Libraries
library("tidyverse")
library("plotly")
library("dplyr")
library("ggplot2")
library("gapminder")
library("maps")
library("gridExtra")

#Data Join
Unicef_data_join_2020 <- Unicef_Metadata_continents_ %>%
  filter(year == 2020)

total_data_join <- full_join(Unicef_data_join_2020, Unicef_Indicator_2)
total_data_join <- full_join(Unicef_data_join_2020, Unicef_Indicator_2, by = c("country" = "country"))
total_data_join <- Unicef_data_join_2020 %>%
  full_join(Unicef_Indicator_2, by = c("country", "year" = "time_period"))

#Map Visualization by Country
map_world <- map_data("world")
map_data_join <- full_join(total_data_join, map_world, by = c("country" = "region"))

country_map <- ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = Probability) + 
  geom_polygon() +
  scale_fill_gradient(low = "orange", high = "red", na.value = "grey80") +
  labs(title = "2020 Global Maternal Mortality Probabilities for 15-Year-Old Girls by Country",
       subtitle = "countries shaded in grey have no data due to discrepancies in their names",
       fill = "Maternal Mortality Probabilities") +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
print(country_map)

#Bar Chart Visualization by Continent
map_data_join_continent <- full_join(total_data_join, map_world, by = c("country" = "region"))
map_data_join_continent_mean <- map_data_join_continent %>%
  group_by(continent) %>%
  summarise(mean_indicator = mean(Probability, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(continent))

barchart <- ggplot(map_data_join_continent_mean, aes(x = continent, y = mean_indicator, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Asia" = "lightblue", "Europe" = "yellow", "Africa" = "lightgreen", "Oceania" = "lightpink", "Americas" = "orange")) +
  theme_minimal(base_family = "serif") +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 45), text = element_text(family = "serif")) +
  aes(reorder(continent, mean_indicator)) +
  labs(x = "Continent", y = "Maternal Mortality Probabilities") +
  labs(title = "2020 Average Global Maternal Mortality Probabilities for 15-Year-Old Girls by Continent") +
  guides(colour = "none")

ggplotly(barchart)

#Scatter Plot Visualization by Continent
scatter_plot_1 <- total_data_join %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(x = `Life expectancy`, y = Probability, color = continent, size = `Population, total`)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("lightblue", "yellow", "lightgreen", "lightpink", "orange")) +
  labs(title = "Relationship between Maternal Mortality Probabilities and Life Expectancy", y = "Maternal Mortality Probabilities", x = "Life Expectancy") +
  theme(text = element_text(family = "serif"), title = element_text(size = 12, face = "bold")) +
  guides(size = "none")

#Scatter Plot 2
scatter_plot_2 <- total_data_join %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(x = `GDP per capita`, y = Probability, color = continent, size = `Population, total`)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("lightblue", "yellow", "lightgreen", "lightpink", "orange")) +
  labs(title = "Relationship between Maternal Mortality Probability and GDP per Capita", y = "Maternal Mortality Probability") +  theme(text = element_text(family = "serif"), title = element_text(size = 12, face = "bold")) +
  guides(size = "none")

#Scatter Plot 3
scatter_plot_3 <- total_data_join %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(x = `GNI`, y = `Probability`, color = continent, size = `Population, total`)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("lightblue", "yellow", "lightgreen", "lightpink", "orange")) +
  labs(title = "Relationship between Maternal Mortality Probabilities and GNI", y = "Maternal Mortality Probabilities") +
  theme(text = element_text(family = "serif"), title = element_text(size = 12, face = "bold")) +
  guides(size = "none")

options(scipen = 999)

# Side by Side
grid.arrange(scatter_plot_2, scatter_plot_3, ncol = 2)

