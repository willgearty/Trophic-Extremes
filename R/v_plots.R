library(tidyverse)
library(nlme)
library(broom.mixed)  # get tidy tables from gls model objects

# Prepare V trait data
# ------------------------------------------------------------------------------
# Prep biome and trait data
biome_traits <- 
  readRDS("../data/amanda_biome_traits.rds") %>% 
  # Make sure that biome ids are numeric
  mutate(biome = as.numeric(biome))
# Create lookup for biome ids
biome_key <- tibble(biome = 1:14, biome_name = c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"), 
  biome_label = c("Tropical & Subtropical\nMoist Broadleaf Forests", "Tropical & Subtropical\nDry Broadleaf Forests", "Tropical & Subtropical\nConiferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical \nGrasslands, Savannas & Shrublands", "Temperate\nGrasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean\nForests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"))

# Master biome data
biomes <- 
  left_join(biome_traits, biome_key) %>% 
  # We only need the biome information for now
  select(binomial, class, biome, biome_name, biome_label, body_mass_median) %>% 
  # Clarify that body_mass_median in this dataframe is actually log10 body mass
  rename(log10_body_mass_median = body_mass_median)

# Prepare diet labels
diet_cat_key <- tibble(diet_5cat = c(1, 2, 3, 4, 5), 
                       diet_name = c('Herbivores', 'Frugivores', 'Omnivores', 
                                     'Insectivores', 'Carnivores'), 
                       diet_abbr = c('H', 'F', 'O', 'I', 'C'))

# These data supply the realm and diet category
v_traits <- 
  readRDS("../data/v_trait.rds") %>% 
  as_tibble()  %>% # easier to manage
  select(binomial, realm, diet_5cat, body_mass_median)

# Master V data
v_data <- 
  left_join(biomes, v_traits, by = "binomial") %>%
  # Relabel diet categories to match diet_cat_key
  mutate(diet_5cat = case_when(diet_5cat == 1 ~ 1, 
                               diet_5cat == 2 ~ 2, 
                               diet_5cat == 3 ~ 5, 
                               diet_5cat == 4 ~ 3, 
                               diet_5cat == 5 ~ 4)) %>% 
  # Add diet labels
  left_join(., diet_cat_key) %>% 
  mutate(diet_abbr = factor(diet_abbr, levels = diet_cat_key$diet_abbr)) %>% 
  # Remove species missing diet category
  drop_na(diet_abbr)

terr_mammals <- filter(v_data, realm == "terrestrial", class == "Mammalia")
terr_birds   <- filter(v_data, realm == "terrestrial", class == "Aves")

#-------------------------------------------------------------------------------
# Figures 3 & 4
#-------------------------------------------------------------------------------
boxplot_theme <- function(base_size = 16) {
  theme_minimal(base_size = base_size) + 
  theme(axis.line = element_line(colour = "black", size = 0.5, lineend = "butt"), 
        panel.grid = element_blank(), 
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -5),
        strip.background = element_rect(fill = "grey90", colour = "white"),
        plot.margin = unit(c(10, 5, 10, 5), "mm")
  )
}

# Terrestrial mammal body size ~ diet plot
terr_mammals %>% 
  ggplot(., aes(x = diet_abbr, y = log10_body_mass_median, group = diet_abbr)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(~ biome_label) +
    boxplot_theme() +
    labs(x = "Diet category", y = expression(Log[10]~median~body~mass))
ggsave('../figures/V_plots_terrestrial_mammals.pdf')

# Terrestrial bird body size ~ diet plot
terr_birds %>% 
  ggplot(., aes(diet_abbr, log10_body_mass_median, group = diet_abbr))+
    geom_boxplot(outlier.shape = NA) +
    facet_wrap( ~ biome_label) +
    boxplot_theme() +
    labs(x = "Diet category", y = expression(Log[10]~median~body~mass))
ggsave('../figures/V_plots_terrestrial_birds.pdf')

#-------------------------------------------------------------------------------
# Calculated means
#-------------------------------------------------------------------------------

# gls model for the different body mass ~ diet category across space
mammal_gls <- 
  gls(log10_body_mass_median ~ diet_name - 1, 
      weights = varIdent(form = ~ 1 | diet_name), 
      data = terr_mammals %>% drop_na(diet_name, biome_name))
summary(mammal_gls)

broom.mixed::tidy(mammal_gls)

bird_gls <- 
  gls(log10_body_mass_median ~ diet_name - 1, 
      weights = varIdent(form = ~ 1 | diet_name), 
      data = terr_birds %>% drop_na(diet_name, biome_name))
summary(bird_gls)

broom.mixed::tidy(bird_gls)