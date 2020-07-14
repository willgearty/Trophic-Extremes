library(tidyverse)
library(readxl)
library(here)

mytheme <- function(base_size = 18, base_family="helvetica", axis_text_adj = 2) {
  theme_minimal(base_size = base_size) + 
  theme(axis.line = element_line(colour="black"), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(size = base_size - axis_text_adj, vjust = -1.5, margin = margin(t = 0.5, b = 10)), 
        axis.text.y = element_text(size = base_size - axis_text_adj, hjust = 0, margin = margin(r = 0.5, l = 12)), 
        axis.title.y = element_text(angle = 90, vjust = 0, margin = margin(r = 10, l = 0.5)),
        axis.title.x = element_text(vjust = -0.2, margin = margin(t = 10, b = 0.5)), 
        plot.margin = unit(c(10,5,5,5),"mm"), 
        strip.background = element_rect(colour = "white", fill = "transparent")
  )
}

kemkem_data <- 
  read_csv(here("data/fossil-fish/Calvin-etal-2015.csv")) %>% 
  mutate(diet_cat = factor(V_diet_cat, levels = c("Herbivore", "Planktivore", "Omnivore", "Benthic carnivore", "Higher carnivore"))) %>% 
  mutate(study = "Cavin et al. 2015") %>% 
  rename(notes = X8)

kemkem_data %>% 
  drop_na(V_diet_cat) %>%
ggplot(data = ., aes(x = diet_cat, y = log(`Length (cm)`), colour = Species)) + 
  geom_point(size = 4)

maisey <- 
  read_csv(here("data/fossil-fish/Maisey-1994.csv")) %>%
  mutate(diet_cat = factor(V_diet, levels = c("Herbivore", "Planktivore", "Omnivore", "Benthic carnivore", "Higher carnivore"))) %>%
  mutate(`Length (cm)` = parse_number(est_ML), 
         study = "Maisey 1994")

maisey %>% 
ggplot(data = ., aes(x = diet_cat, y = `Length (cm)`)) + 
  geom_boxplot()

all <- bind_rows(kemkem_data, maisey)

all %>% 
  drop_na(diet_cat) %>%
ggplot(data = ., aes(x = diet_cat, y = `Length (cm)`)) + 
  geom_point(aes(colour = study), size = 3) + 
  labs(y = "Estimated standard length (cm)", x = "Diet category") +
  mytheme()

