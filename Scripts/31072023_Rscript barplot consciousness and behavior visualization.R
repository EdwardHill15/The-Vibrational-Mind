library(tidyverse)
library(forcats)
library(gt)
library(ggridges)
library(psych)
library(dplyr)
library(gtsummary)
library(corrr)

# NVM datamatrix and definition "unwholesome" consciousness

NVM1_Data <- read_delim("D:/DiagnostiekOnderzoek/Data/NVM1_Data.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
NVM <- NVM1_Data
nvm <- NVM
colnames(nvm) <- c("code", "nr", "name", "gender", "age", 
                   "status", "negativism","somatization", "shyness", 
                   "psychopathology", "extraversion")
nvm <- na.omit(nvm)
nvm$consciousness <- ifelse(nvm$psychopathology >= 6, "unwholesome", 
                            ifelse(nvm$psychopathology < 2.0, "wholesome", "neutral"))
nvm$consciousness <- as.factor(nvm$consciousness)

nvm
glimpse(nvm)


# make a longer dataframe with pivot_longer for visualizations

long <- nvm %>% 
  select(gender, 
         consciousness, 
         negativism, 
         somatization, 
         shyness, 
         extraversion) %>% 
  pivot_longer(cols = c("negativism", "somatization", "shyness", "extraversion"),
               names_to = "symptoms", values_to = "impact") %>% 
  group_by(consciousness,
           gender,
           symptoms) %>% 
  summarise(avg_impact = mean(impact))
long$symptoms <- as.factor(long$symptoms) 
long$consciousness <- fct_reorder(long$consciousness, long$avg_impact)
long <- as.data.frame(long)

glimpse(long)

 # create the barplot in ggplot

  long %>% 
    mutate(gender = ifelse(gender == "f", "Female", "Male")) %>%  
    ggplot(aes(consciousness, avg_impact, fill = symptoms)) +
    geom_col(position = "dodge",
           width = .75) +
    scale_x_discrete(expand = c(.3, .3)) +
    scale_fill_manual(values = c("grey90", "grey60", "grey25", "grey1")) +
    facet_wrap(~ gender, strip.position = "bottom") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.placement = "outside",
          panel.spacing = unit(0, "cm"),
          legend.position = "bottom",
          strip.text = element_text(size = 20, face = "bold"),
          plot.title.position = "plot",
          plot.title = element_text(size = 24, face = "bold",
                                    margin = margin(b = 20)),
          plot.subtitle = element_text(size = 14, color = "grey30",
                                       margin = margin(b = 20)),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0,
                                      color = "grey50",
                                      size = 12)) +
    labs(fill = "", x = "", y = "Stress Experience",
         caption = "Data from {NVM database} The Vibrational Mind.",
         title = "Consciousness and Behavior",
         subtitle = "The relation between unwholesome consciousness and stress experience by gender.")
  
  # save the created barplot as a png file
  
  ggsave("plots/unwholesome_behavior.png", bg = "white")

















