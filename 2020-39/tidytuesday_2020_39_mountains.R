library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggtext)

# Load fonts
#font_add("mountain", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/Mountain Script - Brush Font.ttf")  # Use the actual file path
#showtext_auto()
font_1 <- "mountain"

# Colors
pal <- c("#ffffff", "#dde5f4", "#b1c4d8" ,"#668eab","#5f6350")

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)
members <- tuesdata$members
expeditions <- tuesdata$expeditions
peaks <- tuesdata$peaks

# Get top 5 peaks by member
top_peaks <- members %>%
count(peak_name, sort = TRUE) %>%
  top_n(5)

# Group data
clean_mountain <- members %>%
  filter(peak_name %in% top_peaks$peak_name & year >= 1970) %>%
  mutate(decade=year-year %% 10) %>%
  select(peak_name, decade, injured) %>%
  group_by(peak_name, decade, injured) %>% 
  count(injured) %>% 
  pivot_wider(names_from=injured, values_from=n) %>% 
  mutate(`TRUE` = replace_na(`TRUE`, 0)) %>% 
  mutate(rate=round((`TRUE`/(`TRUE`+`FALSE`)),3))

# Plot data
my_plot<- ggplot(clean_mountain, aes(x=decade, y=`TRUE`, fill=fct_reorder(peak_name, `TRUE`))) + 
  geom_area() +
  theme_minimal()+
  scale_fill_manual(values=pal)+
  scale_x_continuous(limits=c(1969, 2010),expand = c(0, 0))+
  scale_y_continuous(position = "right")+
  labs(
    title = "Help! I've fallen and I can't get up",
    subtitle = "Total injuries on 5 most popular Himalayan peaks by decade",
    caption = "Data: The Himalayan Database | Viz: @aarpurd"
  ) +
  theme(
    plot.title = element_markdown(family = "mountain", color = "white",hjust=2, size = 130, margin=margin(l=-30,t=40,b=14)),
    plot.subtitle = element_text(color="white",hjust=1, size = 35),
    plot.caption = element_text(size = 22, color="white", margin=margin(t=20)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20, color="white"),
    axis.text.y = element_text(size = 18 , color="white"),
    panel.background = element_rect(fill="#00bfff"),
    plot.background = element_rect(fill ="#00bfff", color="#00bfff"),
    panel.border = element_rect(fill=NA,color="#00bfff"),
    legend.title = element_blank(),
    legend.position="right",
    legend.justification = "top",
    legend.text = element_text(size = 20, color="white")
  )
my_plot

# Save chart
ggsave("tidytuesday_2020_39_MountainStacked.png",bg="#00bfff", width = 26, height = 16,units = "cm")

