library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggtext)
library(statebins)
library(RColorBrewer)

# Load fonts
#font_add("kids", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/University.otf")  # Use the actual file path
#showtext_auto()
font_1 <- "kids"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)
kids <- tuesdata$kids

# Group data
clean_kids <- kids %>% 
  filter(variable == 'highered' & year == 2016)

# Chart
my_chart <- ggplot(clean_kids) +
  geom_statebins(aes(fill = inf_adj_perchild, state = state),radius = grid::unit(1, "pt"),border_col = "#c08c0c", border_size = 1, lbl_size = 10) +
  coord_equal() +
  theme_statebins()+
  scale_fill_distiller(palette = "YlGn", direction = 1)+
  labs(x=NULL, y=NULL,
       title = "I'm here for an expensive piece of paper",
       subtitle = "2016 US public spending on higher education, in thousands of dollars per child",
       caption = "Data: Urban Institute | Viz: @aarpurd") +
  theme(
    plot.title = element_markdown(family = "kids", color = "black",hjust=.5, size = 65, margin=margin(t=6,b=14)),
    plot.subtitle = element_text(family = "kids", color="black",hjust=.5, size = 28),
    plot.caption = element_text(size = 22),
    panel.background = element_rect(fill="#c08c0c"),
    plot.background = element_rect(fill ="#c08c0c", color="#c08c0c"),
    panel.border = element_rect(fill=NA,color="#c08c0c"),
    legend.position="bottom",
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "#c08c0c"),
    legend.text = element_text(colour = "black", size=26),
    legend.title = element_blank(),
    legend.key.height = unit(0.7,"line"),
    legend.justification = 0.5
  )
my_chart

# Save chart
ggsave("tidytuesday_2020_38_KidMap.png",bg="#c08c0c", width = 23, height = 18,units = "cm")
