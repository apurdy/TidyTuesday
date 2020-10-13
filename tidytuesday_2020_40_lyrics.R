library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggtext)
library(ggforce)
library(patchwork)

# Load fonts
#font_add("swift", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/Satisfaction.ttf")  # Use the actual file path
font_add("beyonce", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/Beyonce 400.otf")
font_add("love", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/peacechild-beta.ttf")
showtext_auto()
font_1 <- "swift"
font_2 <- "beyonce"
font_3 <- "love"

# Colors
pal <- c("#2a3b90","#ff0040")

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 40)
taylor_swift_lyrics <- tuesdata$taylor_swift_lyrics
beyonce_lyrics <- tuesdata$beyonce_lyrics
sales <- tuesdata$sales
charts <- tuesdata$charts

# Group data - T Swift
swift_love <- taylor_swift_lyrics %>% 
  mutate(love_check = case_when(grepl("love ", Lyrics)==TRUE ~ "love", TRUE ~ "no love")) %>% 
  group_by(love_check)%>%
  summarize(countx=n()) %>% 
  mutate(love_check = factor(love_check, levels = c("no love", "love")),
         cumulative = cumsum(countx),
         midpoint = cumulative - countx / 2,
         label = paste0(love_check, " ", round(countx / sum(countx) * 100, 1), "%")) %>% 
  mutate(percent = paste0(round(countx / sum(countx) * 100, 0), "%"))

# Group data - Queen B
beyonce_love <- beyonce_lyrics %>%
  select(line, song_name) %>% 
  group_by(song_name) %>% 
  summarise_all(funs(paste(na.omit(.), collapse = " "))) %>% 
  mutate(love_check = case_when(grepl("love ", line)==TRUE ~ "love", TRUE ~ "no love")) %>% 
  group_by(love_check) %>% 
  summarize(countx=n()) %>% 
  mutate(love_check = factor(love_check, levels = c("no love", "love")),
         cumulative = cumsum(countx),
         midpoint = cumulative - countx / 2,
         label = paste0(love_check, " ", round(countx / sum(countx) * 100, 1), "%")) %>% 
  mutate(percent = paste0(round(countx / sum(countx) * 100, 0), "%"))

# Chart data - T Swift
my_chart<- ggplot(swift_love, aes(x=0, y=countx, fill=love_check))+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=2.5,fill="#333333", inherit.aes = FALSE)+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=2,fill="black", inherit.aes = FALSE)+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=1.95,fill="#333333", inherit.aes = FALSE)+
  geom_bar(aes(x=1,y=countx),stat="identity",width=1.8,fill="black", inherit.aes = FALSE)+
  geom_bar(aes(x=1,y=countx),stat="identity",width=1.78,fill="#333333", inherit.aes = FALSE)+
  geom_bar(aes(x=1,y=countx),stat="identity",width=1.1,fill="black", inherit.aes = FALSE)+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=1.05,fill="#333333", inherit.aes = FALSE)+
  geom_bar(stat="identity", width=1, aes(group=love_check))+
  geom_text(aes(y = midpoint, label = percent), color = "white", size = 12) +
  geom_bar(aes(x=-0.65,y=countx),stat="identity",width=0.15,fill="#ffc0b6", inherit.aes = FALSE)+
  coord_polar(theta="y")+
  scale_fill_manual(values=pal)+
  theme_void()+
  labs(
    #title = "",
    subtitle = "Taylor Swift"
    #caption= ""
  ) +
  theme(legend.position="none",
        panel.background = element_rect(fill="#ffc0b6"),
        plot.background = element_rect(fill ="#ffc0b6", color="#ffc0b6"),
        panel.border = element_rect(fill=NA,color="#ffc0b6"),
        plot.subtitle = element_markdown(family = "swift", colour = "#5e1553",
                                     size = 70, hjust = 0.5, margin = margin(0, 0, -40, 0))
        ) 
#my_chart

# Chart data - Queen B
my_chart2<- ggplot(beyonce_love, aes(x=0, y=countx, fill=love_check))+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=2.5,fill="#333333", inherit.aes = FALSE)+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=2,fill="black", inherit.aes = FALSE)+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=1.95,fill="#333333", inherit.aes = FALSE)+
  geom_bar(aes(x=1,y=countx),stat="identity",width=1.8,fill="black", inherit.aes = FALSE)+
  geom_bar(aes(x=1,y=countx),stat="identity",width=1.78,fill="#333333", inherit.aes = FALSE)+
  geom_bar(aes(x=1,y=countx),stat="identity",width=1.1,fill="black", inherit.aes = FALSE)+
  geom_bar(aes(x=1.5,y=countx),stat="identity",width=1.05,fill="#333333", inherit.aes = FALSE)+
  geom_bar(stat="identity", width=1, aes(group=love_check))+
  geom_text(aes(y = midpoint, label = percent), color = "white", size = 12) +
  geom_bar(aes(x=-0.65,y=countx),stat="identity",width=0.15,fill="#ffc0b6", inherit.aes = FALSE)+
  coord_polar(theta="y")+
  scale_fill_manual(values=pal)+
  theme_void()+
  labs(
    #title = "",
    subtitle = "Beyoncé"
    #caption=""
  ) +
  theme(legend.position="none",
        panel.background = element_rect(fill="#ffc0b6"),
        plot.background = element_rect(fill ="#ffc0b6", color="#ffc0b6"),
        panel.border = element_rect(fill=NA,color="#ffc0b6"),
        plot.subtitle = element_markdown(family = "beyonce", colour = "#c93667",
                                     size = 70, hjust = 0.5, margin = margin(0, 0, -40, 0))
  ) 

# Combine charts
my_chart + my_chart2 + plot_annotation(
  title = "Got me looking so crazy in <span style='color:#ff0040'>Love</span>",
  subtitle = "Percentage of songs that mention <span style='color:#ff0040'>Love</span>",
  caption = 'Data: Rosie Baillie & Dr. Sara Stoudt | Viz: @aarpurd',
  theme = theme(plot.title = element_markdown(family = "love", size = 120, hjust=0.5, color = "#7c0000", margin = margin(t=30)),
                plot.subtitle = element_markdown(family = 'love', size = 50,hjust=0.5, color = "#7c0000", margin = margin(t=10,b=40)),
                plot.caption = element_text(size = 28),
                panel.background = element_rect(fill="#ffc0b6"),
                plot.background = element_rect(fill ="#ffc0b6", color="#ffc0b6"),
                panel.border = element_rect(fill=NA,color="ffc0b6"))
)

# Save charts
ggsave("tidytuesday_2020_40_LyricsPie.png",bg="#ffc0b6", width = 22, height = 16,units = "cm")
