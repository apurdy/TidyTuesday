library(tidytuesdayR)
library(tidyverse)
library(here)
library(lubridate)
library(glue)
library(ggtext)
library(ggimage)
library(rcartocolor)
library(cowplot)
library(ISOcodes)
library(showtext)

# Load fonts
# font_add("phone", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/bell-centennial-address.ttf")  # Use the actual file path
# showtext_auto()
font_1 <- "phone"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 46)
mobile <- tuesdata$mobile
landline <-tuesdata$landline

# Get Alpha-2 country codes
country_codes <- ISO_3166_1 %>% 
  select(Alpha_2,Alpha_3)

# Clean data
mob_clean<- mobile %>% 
  filter(continent=='Americas' & year==2017) %>% 
  inner_join(landline, by=c("entity", "year")) %>% 
  select(entity, code.x, continent.x, total_pop.y, mobile_subs, landline_subs) %>%
  filter(!is.na(mobile_subs) & !is.na(landline_subs)) %>% 
  mutate(mobile_subs=round(mobile_subs,1), land_subs=round(landline_subs, 1), index = 1:n()) %>% 
  mutate(more_subs = case_when(mobile_subs>100 ~ "More mobile subs than people", TRUE ~ "Less mobile subs than people")) %>% 
  inner_join(country_codes, by=c("code.x"="Alpha_3")) %>% 
  mutate(entity=recode(entity, `Saint Kitts and Nevis`="Saint Kitts")) %>% 
  mutate(entity=recode(entity, `Saint Vincent and the Grenadines`="Saint Vincent"))

# Chart data
my_plot <- ggplot(mob_clean) +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1, fill = more_subs), color = "white") +
  geom_flag(aes(x=-0.9, y= 0.8, image = Alpha_2),hjust=0, size=0.2)+
  # full country name
  geom_text(aes(label = entity, x = -0.7, y = 0.45), hjust = 0,
            size = 10, family = ("phone"), color = "white") +
  # country code       
  geom_text(aes(label = code.x, x = -0.7, y = 0.05), hjust = 0,
            size = 38, family = ("phone"), fontface = "bold", color = "white") +
  # phone stats      
  geom_text(aes(label = paste0(land_subs, " | ", mobile_subs),
                x = -0.7, y = -0.5), hjust = 0,
            size = 16, family = ("phone"), fontface="bold", color = "grey90") +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) + 
  scale_fill_manual(values = c("#3d6c6f","#ef7b4f")) +
  facet_wrap(~ index, ncol = 10) +
  labs(
    title = "The Periodic Table of Phone Usage",
    subtitle="in the Americas (2017)",
    caption = "Data: OurWorldInData.org  | Viz: @aarpurd"
  ) + 
  theme_void(base_family = "phone") +
  theme(
    legend.position = "bottom",
    legend.margin=margin(-92,-100,0,450),
    # legend.spacing.x = unit(0.8, 'cm'),
    legend.text = element_text(margin = margin(0, 0, 0, 0), size=36),
    legend.title = element_blank(),
    legend.direction="vertical",
    #legend.text.align = 0,
    strip.text = element_blank(),
    panel.spacing = unit(2, "points"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "phone", face = "bold", size = 180,
                              margin = margin(0, 0, 0, 0), hjust = 0),
    plot.subtitle = element_text(family = "phone", face = "bold", size = 180,
                              margin = margin(10, 0, 60, 0), hjust = 0),
    plot.caption = element_text(family = "phone", hjust = 0.5, size = 48,
                                margin = margin(40, 0, 0, 50))
  ) 

my_plot

my_plot_2 <- ggplot(subset(mob_clean, index == 1)) +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1), fill = "#ef7b4f", color = "white") +
  
  # text
  geom_text(aes(label = entity, x = -0.7, y = 0.45), hjust = 0,
            size = 10, family = ("phone"), color = "white") +
  geom_text(aes(label = code.x, x = -0.7, y = 0.05), hjust = 0,
            size = 38, family = ("phone"), fontface = "bold", color = "white") +
  geom_text(aes(label = paste0(land_subs, " | ", mobile_subs),
                x = -0.7, y = -0.5), hjust = 0,
            size = 16, family = ("phone"), fontface="bold", color = "grey90") +
  geom_text(aes(x = -1.1, y = 0.8, hjust = 1,
                label = "Flag"), size = 12,  family = ("phone"), color = "black") +
  # geom_label(aes(x = -1.1, y = -0.9, hjust = 0,
  #                label = "More mobile subs than people"), size = 10,  family = ("phone"), fontface = "bold", fill = "#ff7d4d", color = "white", label.r = unit(0, "lines")) +

  geom_text(aes(x = -1.1, y = 0.45, hjust = 1,
                label = "Country"), size = 12,  family = ("phone"), color = "black") +
  geom_text(aes(x = -1.1, y = 0.05, hjust = 1,
                label = "Abbreviation"), size = 14,  family = ("phone"), fontface = "bold", color = "black") +
  geom_text(aes(x = -1.1, y = -0.5, hjust = 1,
                label = "Landline | Mobile (subs per 100 people)"), size = 11,  family = ("phone"), color = "black") +
  geom_flag(aes(x=-0.9, y= 0.8, image = Alpha_2),hjust=0, size=0.09, asp=2.6)+
  #geom_flag(aes(x=0, y= 0.8, image = Alpha_2),hjust=0)+
  #scale_size_identity()+
  coord_fixed(ratio=1,xlim = c(-4, 1), ylim = c(-1, 1)) +
  theme_void() +
  theme(
    legend.position = "none"
  ) 

my_plot_2

# Combine plots
#ggdraw(my_plot) + draw_plot(my_plot_2, 0.49, 0.81, 0.6, 0.15)
ggdraw(my_plot) + draw_plot(my_plot_2, 0.54, 0.80, 0.6, 0.15)

# Save plot
ggsave("tidytuesday_2020_46_PhonesTable.png",bg="#f5f5f5", width = 15, height = 9.5)