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

# Load fonts
#("ncaa", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/NCAA_Michigan_St_Spartans.otf")  # Use the actual file path
#showtext_auto()
font_1 <- "ncaa"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 41)
tournament <- tuesdata$tournament

# Group data
conf_champ <- tournament %>% 
  select(school, tourney_finish) %>%
  mutate(school=recode(school, `Southern California`="USC")) %>% 
  filter(tourney_finish %in% c('Champ','N2nd')) %>% 
  mutate(win=ifelse(tourney_finish=='Champ', 1, 0)) %>%
  group_by(school) %>%
  summarise(finish=n(), win=sum(win)) %>%
  mutate(win_pos=ifelse(win==0,1,win)) %>%
  mutate(rate=paste0(round(win / finish * 100, 0), "%")) %>%
  top_n(., 10, finish) %>%
  top_n(., 10, school) %>%
  arrange(finish) %>% 
  mutate(n = -5:4) %>% 
  rowwise() %>%
  mutate(
    x = list(c(-0.9, 0, 0, -0.9)),
    y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))
  ) %>% 
  unnest(cols = c(x, y))

# Plot data
my_plot <- ggplot(conf_champ)+
  geom_rect(aes(xmin = -5.2, ymin = n*4 - 1.4,
                xmax = -0.9, ymax = n*4 + 1.4), fill = "black", color = NA) +
  geom_polygon(aes(x, y, group = school), fill = "#3d3d3d", color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = finish, ymax = n*2 + 0.7), fill = "#ffbc76", color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = win, ymax = n*2 + 0.7), fill = "#ff8200", color = NA) +
  geom_text(aes(-3.05, n*4, label = school), family = "ncaa", fontface = "bold", color = "#FFFFFF", hjust = 0.5, size = 18, check_overlap = TRUE) +
  geom_text(aes(finish+0.4, n*2, label = finish), family = "ncaa", fontface = "bold", color = "#FFFFFF", size = 20, check_overlap = TRUE) +
  geom_text(aes(win_pos/2, n*2, label = rate), fontface = "bold", color = "#FFFFFF", size = 12, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0, 12, 2), labels = seq(0, 12, 2)) +
  labs(title = "best of the best",
       subtitle = "NCAA Women's Basketball Championship Game Appearances\n and Win % (1982-2018)",
       caption = "Data: FiveThirtyEight | Viz: @aarpurd") +
  theme_minimal() +
  theme(plot.title = element_text(colour = "white", family = "ncaa", face = "bold", size = 140, hjust = 0.5, margin = margin(25,0,0,0)),
        plot.subtitle = element_text(colour = "white", face = "bold", size = 46, hjust = 0.5, lineheight =0.3 , margin = margin(0,0,0,0)),
        plot.caption = element_text(colour = "white", face = "bold", size = 38, hjust = 0.5, margin = margin(10,0,0,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(20, 20, 20, 20))

#my_plot

# Save plot
ggsave("tidytuesday_2020_41_NCAABar.png",bg="#005eb8", width = 20, height = 24,units = "cm")