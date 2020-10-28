library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggtext)
library(ggforce)
library(extrafont)
library(magick)
library(cowplot)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)

# Load fonts
#("turbine", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/NCAA_Michigan_St_Spartans.otf")  # Use the actual file path
#showtext_auto()
font_1 <- "turbine"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 44)
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

world <- ne_countries(scale = "medium", returnclass = "sf")

# Group data
clean_wind <- wind_turbine %>% 
  filter(province_territory =='Nova Scotia')

# Chart data
ggplot(data = world) +
  geom_sf() +
  geom_point(data = clean_wind, aes(x = longitude, y = latitude, color=manufacturer), size = 4, 
             shape = "\u274B") +
  coord_sf(xlim = c(-67, -59), ylim = c(43, 47.2), expand = FALSE)+
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
    
  )