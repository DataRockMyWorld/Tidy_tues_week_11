
## Set Up Environment----

library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(lubridate)
library(scales)
library(showtext)
library(ggforce)
library(patchwork)

## showtext environmet----

font_add_google("Amita","Amita")
font_add_google("Denk One","Denk One")
font_add_google("Bangers","Bangers")
font_add_google("Diplomata","Diplomata")

showtext_auto()


## Import the Data----
tidytues <- tidytuesdayR::tt_load("2022-03-15")
cran <- tidytues$cran
bioc <- tidytues$bioc

### Inspect the Data----

glimpse(cran)
glimpse(bioc)


## convert date of dataframes to date_time ----

cran1 <- cran %>% 
  drop_na() %>% 
  mutate(date = as_datetime(date))


# Analyze data----

skim(cran1)

## Prepare the Input Data and calculate mean number of packages uploaded per hr each day

cran_plot <- cran1 %>% mutate(hour = hour(date)) %>% 
  pivot_longer(cols = c(rnw,rmd), names_to = "archive", values_to = "No of packages") %>% 
  group_by(hour,archive) %>% 
  summarise(No_of_packages = mean(`No of packages`, na.rm = TRUE))

## Plot Labels
labels_cran <- labs(title = "RMD Based Vignettes are Put Out at Higher Rate",
                    subtitle = "Average Package output & Update Each Hour of the day",
                    caption = "Data from Robert Flight | plot by Juelzgh",
                    y = "Average no of packages",
                    x = "Hours of the day")


## PLOTS
## No of cran packages uploaded/updated per hour per day
plot_cran <- cran_plot %>%
  ggplot(aes(hour, No_of_packages, color = archive)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(x = 4, y = 0.2,label = "RNW"),stat = "unique", family = "Bangers",size = 7, color = "#ffd500") +
  geom_text(aes(x = 16, y = 0.58,label = "RMD"),stat = "unique", family = "Bangers",size = 7, color = "#005bbb") +
  scale_x_continuous(limits = c(0,24), breaks = c(0, 4,8,12,16,20), labels = c("12am","4am","8am","12pm","4pm","8pm")) +
  scale_color_manual(values = c("#005bbb","#ffd500")) +
  labels_cran +
  theme_minimal() +
  
  theme(plot.title = element_text(size = 25, colour = "black", face = "bold",family = "Amita"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(colour = "black", face = "bold"),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 13, face = "bold", family = "Amita", margin = margin(t = 13)),
        axis.title.y = element_text(size = 13, face = "bold", family = "Amita", margin = margin(r = 13)),
        panel.grid.minor = element_blank(),
        legend.position   = "none"
  )


ggsave("plot_cran.png", width = 10, height = 9)

