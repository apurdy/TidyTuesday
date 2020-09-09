library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggalt)
library(ggtext)
library(ggridges)


# Load fonts
#font_add("friends", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/GABRWFFR.ttf")  # Use the actual file path
#showtext_auto()
font_1 <- "friends"
pal <- c("#4363d8","#EC5244", "#5CA1D1", "#8372AA" ,"#FADC4A","#9A6324" )

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 37)
friends <- tuesdata$friends
friends_info <- tuesdata$friends_info

# Clean rating data
rating_clean <- friends_info %>% 
  select(season, episode, imdb_rating) %>%
  unite("season_ep",c(season, episode),sep="") 
  
# Clean speaker data
friends_clean <- friends %>% 
  select(speaker, season, episode) %>% 
  filter(speaker %in% c('Monica Geller', 'Joey Tribbiani','Chandler Bing', 'Ross Geller', 'Phoebe Buffay', 'Rachel Green')) %>% 
  count(season, episode, speaker, sort=TRUE) %>% 
  unite("season_ep",c(season, episode),sep="") %>%
  group_by(season_ep, speaker) %>%
  summarize(n=sum(n)) %>% 
  arrange(season_ep, desc(n), speaker) %>% 
  mutate(ranking = row_number(), speaker) %>%
  filter(ranking==1) %>% 
  inner_join(rating_clean)

# Chart
my_plot <-
ggplot(friends_clean, aes(x = imdb_rating, y = fct_reorder(speaker, imdb_rating, median), group=speaker, fill=speaker, label = speaker)) +
  geom_density_ridges(scale = .9, rel_min_height = .01, size = .5,
  quantile_lines = T, quantiles = 2, color="white")+
  scale_y_discrete(expand = expand_scale(add = c(0.2, 1))) +
  geom_jitter(height = 0, color="white", size = 1) +
  labs(x="IMDB Rating", y=NULL,
       title = "You-You-You-You Threw My Sandwich Away",
       subtitle = "IMDB episode rating by character having the most lines in the episode",
       caption = "Data: friends R package | Viz: @aarpurd") +
  theme_minimal() +
  scale_fill_manual(values=pal) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_markdown(family = "friends", color = "white", size = 86, hjust=-0.5, margin=margin(t=12, b=6)),
    plot.subtitle = element_text(family = "friends", color = "white", size = 28, hjust=0.5, margin=margin(t=5)),
    plot.caption = element_text(color="white", size = 20),
    legend.position="none",
    panel.background = element_rect(fill ="black"),
    plot.background = element_rect(fill ="black"),
    panel.border = element_rect(fill=NA,color="black"),
    axis.text.x = element_text(color="white", family = "friends", size = 18),
    axis.title.x = element_text(color="white", family="friends", size = 18),
    axis.text.y = element_text(color="white", family = "friends", margin = margin(r = -70, l=20), vjust=-2, size=36)
    
  )
my_plot

# Save chart
ggsave("tidytuesday_2020_37_FriendsRidge.png", width = 24, height = 16,units = "cm")