library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggalt)
library(ggtext)

# Load fonts
#font_add("logo", "C:/Users/purdya/AppData/Local/Microsoft/Windows/Fonts/gomarice_gogono_cocoa_mochi.ttf")  # Use the actual file path
#showtext_auto()
font_1 <- "logo"

# Get data
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application
yield <- tuesdata$key_crop_yields

# Clean data
clean_crops <- yield %>%
  filter(!is.na(`Cocoa beans (tonnes per hectare)`) & Year %in% c('2018','2000') & !is.na(Code) & Code != 'OWID_WRL') %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower()) %>% 
  filter(crop == 'Cocoa beans') %>% 
  pivot_wider(names_from = year,
              names_glue = "c_{year}_{.value}",
              values_from = crop_production) %>% 
  arrange(desc(c_2018_crop_production)) %>% 
  mutate(prod_diff = round(c_2018_crop_production-c_2000_crop_production,2)) %>%
  mutate(diff_clean = ifelse(prod_diff<0, "","+")) %>%
  mutate(prod_diff_str=prod_diff) %>% 
  unite("clean",c(diff_clean, prod_diff_str),sep="", remove = FALSE) %>% 
  mutate(bar_col = ifelse(prod_diff<0, "red","green"))

# Chart
my_plot <- 
ggplot(clean_crops, aes(x=c_2000_crop_production, xend=c_2018_crop_production, y=reorder(entity, c_2018_crop_production))) +
  geom_segment(data=clean_crops,aes(y=reorder(entity, c_2018_crop_production), yend=entity, x=0, xend=3.12), color="grey", size=0.08,linetype="dotted") +
  geom_segment(data=clean_crops,aes(x=c_2000_crop_production, xend=c_2018_crop_production, y=reorder(entity, c_2018_crop_production), yend=entity, color = bar_col),size = 1.3) +
  scale_color_manual(values = c("#228B22","#e60000")) + 
  geom_point(data=clean_crops,aes(x=c_2000_crop_production,y=entity), color = "#808080", size=1.5) +
  geom_point(data=clean_crops,aes(x=c_2018_crop_production,y=entity), color="black", size=1.5) +
  geom_text(data=filter(clean_crops, entity=="Thailand"),
            aes(c_2000_crop_production,y=entity, label="2000 Yield"),
            color="#808080", size=8, vjust=-2, fontface="bold", family="logo") +
  geom_text(data=filter(clean_crops, entity=="Thailand"),
            aes(c_2018_crop_production,y=entity, label="2018 Yield"),
            color="black", size=8, vjust=-2, fontface="bold", family="logo") +
  #geom_rect(aes(xmin=3.2, xmax=3.45, ymin=-0.5, ymax=20), fill="#efefe3") +
  geom_text(data=clean_crops,aes(label=clean, y=entity, x=3.325), fontface="bold", size=6, family="Calibri", color = "#35281E") +
  geom_text(data=filter(clean_crops, entity=="Thailand"), aes(x=3.325, y=entity, label="DIFF"),
            color="#35281E", size=8, vjust=-2, fontface="bold", family="logo") +
  scale_x_continuous(expand=c(0,0), limits=c(-.08, 3.45)) +
  scale_y_discrete(expand=c(0.1,0.1))+
  labs(x=NULL, y=NULL,
       title = "I'm in love with the cocoa",
       subtitle = "Change in cocoa bean yield (in tonnes per hectare) between 2000 and 2018",
       caption = "Data: Our World in Data | Viz: @aarpurd") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position="none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color="#35281E", family = "logo",margin = margin(l = 5), size = 17),
    plot.title = element_markdown(family = "logo", color = "#35281E", size = 88, margin = margin(t = 10, r=10), hjust=-.5),
    plot.subtitle = element_markdown(family = "logo", color = "#35281E", size = 22, margin = margin(t=5)),
    panel.background = element_rect(fill ="#f3e4ad"),
    plot.background = element_rect(fill ="#f3e4ad"),
    panel.border = element_rect(fill=NA,color="#f3e4ad"),
    plot.caption = element_text(size = 16)
    
  )
my_plot

# Save plot
ggsave("tidytuesday_2020_36_CropsDumbbell.png", width = 17, height = 24,units = "cm")