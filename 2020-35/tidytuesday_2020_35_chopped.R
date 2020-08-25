library(tidytuesdayR)
library(hrbrthemes)
library(showtext)
library(waffle)
library(ggtext)

# Load fonts
#font_add("logo", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/LemonJellyPersonalUse-dEqR.ttf")  # Use the actual file path
#showtext_auto()
 font_1 <- "logo"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)
chopped <- tuesdata$chopped

# Group data
new_chop <- chopped %>% 
  select(appetizer,entree,dessert) %>% 
  pivot_longer(c(appetizer, entree, dessert),names_to = "type", values_to = "ingredients") %>%
  mutate(cheese_check = case_when(grepl("cheese ", ingredients)==TRUE ~ "cheese", TRUE ~ "not")) %>% 
  group_by(cheese_check) %>% 
  summarize(count=n()) %>% 
  mutate(cheese_check=recode(cheese_check, 
                    'cheese'="Dishes with Cheese",
                    'not'="Dishes without Cheese"))

# Chart
my_plot <-ggplot(new_chop, aes(fill = cheese_check, values = count))  +
  geom_waffle(n_rows = 35, size = 0.4, colour = "#36454f") +
  scale_fill_manual(
    name = NULL,
    values = c("#ffa600", "grey"),
    labels = c("Dishes with Cheese", "Dishes without Cheese")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  labs(
    title = "Everything is Better with 
    <span style='color:#ffa600'>Cheese</span>",
    subtitle = "Of all the dishes on Chopped, only 3% had 
    <span style='color:#ffa600'>**cheese**</span>",
    caption = "Data: Kaggle | Viz: @aarpurd"
  ) +
  theme(strip.text = element_text(face="bold", size=12.5),
        plot.title = element_markdown(family = "logo", color = "white", size = 190, hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5, color = "white", size = 28),
        plot.caption = element_text(color="white", size = 24),
        legend.position="none",
        panel.background = element_rect(fill ="#36454f"),
        plot.background = element_rect(fill ="#36454f"),
        panel.border = element_rect(fill=NA,color="#36454f"),
        plot.margin=grid::unit(c(11,52,9,52), "mm")
        )
my_plot

ggsave("tidytuesday_2020_35_ChoppedWaffle.png", width = 24.15, height = 15, units = "cm")

