library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)

# Load fonts
# font_add("JFWildWood", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/JFWilwod.ttf")  # Use the actual file path
# showtext_auto()

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
threats <- tuesdata$threats

# Group data
grp_threats <- threats %>% 
  mutate(year_last_seen = fct_relevel(year_last_seen, "Before 1900")) %>% 
  arrange(year_last_seen) %>%
  filter(!is.na(year_last_seen)) %>% 
  group_by(year_last_seen, threat_type) %>% 
  summarise(n = sum(threatened)) %>%
  arrange(year_last_seen, desc(n), threat_type) %>% 
  mutate(ranking = row_number(),threat_type) 

# Chart
ggplot(data = grp_threats, aes(x = year_last_seen, y = ranking, group = threat_type)) +
  geom_line(aes(color = threat_type, alpha = 1), size = 1.5) +
  geom_point(aes(color = threat_type, size = n)) +
  scale_y_reverse(breaks = 1:12) +
  geom_text(data = grp_threats %>% filter(year_last_seen == "2000-2020"),
           aes(label = threat_type, x = 7.3, color = threat_type) ,hjust = 0, fontface = "bold", size = 12) +
  coord_cartesian(xlim = c(1,9.4)) +
  theme_ipsum_ps() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size=26, margin = margin(r = -20)),
        axis.text.x = element_text(size = 26),
        axis.title.y = element_blank(),
        plot.background=element_rect(fill="#ddbea0"),
        panel.background = element_rect(fill ="#ddbea0"),
        panel.border = element_rect(fill=NA,color="#ddbea0"),
        plot.title = element_text(family = "JFWildWood", size = 100, color = "#5c0512"),
        plot.caption = element_text(size =24),
        plot.subtitle = element_text(size = 36)
        ) +
  labs(y = "Rank",
       title = "Countdown to Extinction",
       subtitle = "Biggest threats to plant species ranked over time",
       caption = 'Data: IUCN | Viz: @aarpurd'
       ) +
  scale_color_manual(values = c("#072C8F","#191A1A","#6B238E","#708090","#708090","#708090","#708090","#1A7D00","#708090","#708090","#708090","#F70020"))

ggsave("tidytuesday_2020_34_PlantsBump.png", width = 22, height = 16, units = "cm")
