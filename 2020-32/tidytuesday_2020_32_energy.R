library(tidytuesdayR)
library(hrbrthemes)
library(tidyverse)
library(rworldmap)
library(scales)
library(mapproj)

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)
energy_types <- tuesdata$energy_types

# Clean data and get top energy source in 2018
energy_types_2 <- energy_types %>%
  pivot_longer(-c(country, country_name, type, level), names_to = "year", values_to = "energy") %>%
  within(country_name[country == 'UK'] <- 'United Kingdom') %>%
  within(country_name[country == 'EL'] <- 'Greece') %>%
  within(country_name[country == 'BA'] <- 'Bosnia and Herz.') %>%
  within(country_name[country == 'CZ'] <- 'Czech Rep.') %>%
  within(country_name[country == 'MK'] <- 'Macedonia') %>%
  filter(year == "2018", type == "Hydro" | type == "Solar" | type == "Wind") %>%
  group_by(country, country_name, year) %>% top_n(n=1) %>%
  filter(energy != 0)

# Generate world map
worldMap <- getMap()

# Show only countries from the dataset
show_europe <- which(worldMap$NAME %in% energy_types_2$country_name)

# Get lat and long coordinates of Europe countries
country_coords <- lapply(show_europe, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

country_coords <- do.call("rbind", country_coords)

# Match in 2018 hydro values and types
country_coords$energy <-energy_types_2$energy[match(country_coords$region, energy_types_2$country_name)]
country_coords$type <-energy_types_2$type[match(country_coords$region, energy_types_2$country_name)]

# Create ggplot map
my_map <- ggplot() + 
  geom_polygon(data = country_coords, aes(x = long, y = lat, group = region, fill = type),
                                  colour = "black", size = 0.5) +
  labs(title = 'Renewable Energy in Europe', subtitle = 'Top Energy Source by Country in 2018',
      caption = 'Data: Eurostat | Viz: @aarpurd') +
  coord_map(xlim = c(-13, 50),  ylim = c(33, 70)) +
  scale_fill_manual(values = c("#0EBFE9", "#ffea00", "#4DBD33")) +
  theme_ipsum_pub() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank(),
        panel.spacing = unit(0.1, "lines"))

# Plot map
my_map
  
# Save map
ggsave("tidytuesday_2020_32_EuropeMap.png", width = 15, height = 18, units = "cm")

