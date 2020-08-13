library(tidytuesdayR)
library(hrbrthemes)
library(extrafont)
#extrafont::loadfonts(device="win")
library(tidyverse)
library(tvthemes)
library(tidytext)
library(viridis)
library(ggrepel)

#font_import()
import_avatar()
font_1 <- "Herculanum"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)
avatar <- tuesdata$avatar


# Top 6 Characters by number of lines
top_characters <- avatar %>%
  filter(character != 'Scene Description') %>%
  count(character, sort = TRUE) %>%
  top_n(6)

 top_avatar <- avatar %>%
  filter(character %in% top_characters$character)

# Prep for sentiment analysis
token.mydata <- top_avatar %>%
   unnest_tokens(word, character_words)
 
stop_words <- stop_words
tidy.token.mydata <- token.mydata %>%
   anti_join(stop_words, by = "word")
 
 # Obtain sentiment for each of the top 10 characters
character.sentiment <- tidy.token.mydata %>%
   inner_join(get_sentiments("bing")) %>%
   group_by(character) %>%
   filter(is.element(character,top_characters$character)) %>%
   count(character, index = chapter, book, sentiment) %>%
   spread(sentiment, n, fill = 0) %>%
   mutate(sentiment = positive - negative)

# Chart
 my_plot<- ggplot(character.sentiment, aes(x=reorder(character,sentiment),y=sentiment)) +
   geom_violin( scale = "width", aes(fill = factor(character)), lwd =1, bw =.7) + 
   geom_jitter(width = 0) +
   coord_flip() +
   scale_fill_avatar(palette = "FireNation") +
   labs(x = "Character", y ="Sentiment Rating" ,title ="A Song of Water, Earth and Fire"
        , subtitle = "Violin plots of descending sentiment among the top 6 characters"
        , caption = 'Data: appa R Package | Viz: @aarpurd') +
   theme_avatar(legend.position = "none", title.font = font_1) +
   theme(
     panel.grid.major.y = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     axis.title.y = element_blank(),
     axis.title.x = element_blank(),
     axis.text.x = element_blank(),
     plot.caption = element_text(size =11),
     plot.subtitle = element_text(size = 15),
     plot.title = element_text(size=30), #hjust = 0.5),
     axis.text.y = element_text(family = font_1, size = 16, face = "bold")
 
   )
 ggsave("tidytuesday_2020_33_AvatarViolin.png", width = 26, height = 18, units = "cm")




