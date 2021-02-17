# Analysis of UNHCR Data


# Set up ------------------------------------------------------------------

library(tidyverse)
library(countrycode)
all_data <- read.csv("data/population.csv", skip = 14)

# Simple Exploration
dim(all_data)
unique(all_data$Year)
data <- all_data %>% 
  filter(Year == 2020) %>% 
  select(contains("Country"), Asylum.seekers)

country_of_interest <- "USA"
country_name <- countrycode(country_of_interest, origin = 'iso3c', destination = 'country.name')
country_data <- data %>% 
  filter(Country.of.asylum..ISO. == country_of_interest)


# High Level Questions ----------------------------------------------------

# How many countries do asylum seekers come from (into country of interest)
num_countries <- nrow(country_data)

# How many people sought asylum in this country in 2020?
num_people <- country_data %>% 
  summarize(total_people = sum(Asylum.seekers)) %>% 
  pull()

top_10_countries <- country_data %>% 
  top_n(10, wt = Asylum.seekers) %>% 
  arrange(-Asylum.seekers) %>%
  select(Country.of.origin, Asylum.seekers)


# Make a Map --------------------------------------------------------------
shapefile <- map_data("world")
# Get iso3c
shapefile <- shapefile %>% 
  mutate(Country.of.origin..ISO. = countrycode(region, origin = 'country.name', destination = 'iso3c')) %>% 
  left_join(country_data, by = "Country.of.origin..ISO.")

asylum_map <- ggplot(data = shapefile)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Asylum.seekers)) + 
  labs(title = paste("Number of People Seeking Asylum in", country_name)) +
  theme_minimal()
